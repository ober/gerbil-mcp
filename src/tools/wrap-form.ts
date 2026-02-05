/**
 * gerbil_wrap_form — Wrap lines in a new form with guaranteed matching parens.
 *
 * Pure TypeScript, no subprocess.
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, writeFile } from 'node:fs/promises';
import { checkBalance } from './check-balance.js';
import { findFormAt } from './scheme-scanner.js';

export function registerWrapFormTool(server: McpServer): void {
  server.registerTool(
    'gerbil_wrap_form',
    {
      title: 'Wrap Form',
      description:
        'Wrap lines in a new Scheme form with guaranteed matching parentheses. ' +
        'For example, wrapping with "when (> x 0)" produces (when (> x 0) <body>). ' +
        'Dry-run by default. Pure TypeScript — no subprocess.',
      inputSchema: {
        file_path: z.string().describe('Absolute path to the Gerbil source file'),
        start_line: z
          .number()
          .int()
          .describe('Start line (1-based) of the code to wrap'),
        end_line: z
          .number()
          .int()
          .optional()
          .describe(
            'End line (1-based, inclusive). If omitted, auto-detects the end of the form at start_line.',
          ),
        wrapper: z
          .string()
          .describe(
            'The wrapper form head. E.g. "when (> x 0)" produces (when (> x 0) <body>). ' +
              'E.g. "let ((x 1))" produces (let ((x 1)) <body>).',
          ),
        dry_run: z
          .boolean()
          .optional()
          .describe(
            'If true (default), only preview the change without writing. Set to false to apply.',
          ),
      },
    },
    async ({ file_path, start_line, end_line, wrapper, dry_run }) => {
      const isDryRun = dry_run !== false;

      // Read file
      let source: string;
      try {
        source = await readFile(file_path, 'utf-8');
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [{ type: 'text' as const, text: `Failed to read file: ${msg}` }],
          isError: true,
        };
      }

      const sourceLines = source.split('\n');

      // Validate start_line
      if (start_line < 1 || start_line > sourceLines.length) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `start_line ${start_line} is out of range (file has ${sourceLines.length} lines).`,
            },
          ],
          isError: true,
        };
      }

      // Determine end line
      let effectiveEndLine: number;
      if (end_line !== undefined) {
        if (end_line < start_line || end_line > sourceLines.length) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `end_line ${end_line} is out of range (must be >= start_line and <= ${sourceLines.length}).`,
              },
            ],
            isError: true,
          };
        }
        effectiveEndLine = end_line;
      } else {
        // Auto-detect form end
        const form = findFormAt(source, start_line);
        if (form) {
          effectiveEndLine = form.end.line;
          // If the closer is at col 1 on a line by itself, the form actually
          // ends on that line
        } else {
          // Fall back to just this one line
          effectiveEndLine = start_line;
        }
      }

      // Validate wrapper fragment balance
      // The wrapper should be balanced on its own when we wrap it: "(" + wrapper + " x)"
      const testForm = '(' + wrapper + ' x)';
      const wrapperBalance = checkBalance(testForm);
      if (!wrapperBalance.ok) {
        const errors = wrapperBalance.errors
          .map((e) => `  ${e.kind}: '${e.char}' at col ${e.col}`)
          .join('\n');
        return {
          content: [
            {
              type: 'text' as const,
              text: `Invalid wrapper — "${wrapper}" has unbalanced delimiters:\n${errors}`,
            },
          ],
          isError: true,
        };
      }

      // Extract body lines and detect indentation
      const bodyLines = sourceLines.slice(start_line - 1, effectiveEndLine);
      const firstLine = bodyLines[0];
      const leadingMatch = firstLine.match(/^(\s*)/);
      const baseIndent = leadingMatch ? leadingMatch[1] : '';
      const innerIndent = baseIndent + '  ';

      // Construct wrapped form
      const wrappedLines: string[] = [];
      wrappedLines.push(`${baseIndent}(${wrapper}`);
      for (const bodyLine of bodyLines) {
        // Re-indent body lines relative to the wrapper
        const trimmed = bodyLine.replace(/^\s*/, '');
        if (trimmed.length === 0) {
          wrappedLines.push('');
        } else {
          wrappedLines.push(`${innerIndent}${trimmed}`);
        }
      }
      // Add closing paren
      const lastIdx = wrappedLines.length - 1;
      wrappedLines[lastIdx] = wrappedLines[lastIdx] + ')';

      const wrappedText = wrappedLines.join('\n');

      // Build the new file content
      const beforeLines = sourceLines.slice(0, start_line - 1);
      const afterLines = sourceLines.slice(effectiveEndLine);
      const newSource = [...beforeLines, wrappedText, ...afterLines].join('\n');

      // Validate balance of the result
      const newBalance = checkBalance(newSource);
      const originalBalance = checkBalance(source);

      if (originalBalance.ok && !newBalance.ok) {
        const errorDetails = newBalance.errors
          .map((e) => `  ${e.kind}: '${e.char}' at line ${e.line}, col ${e.col}`)
          .join('\n');
        return {
          content: [
            {
              type: 'text' as const,
              text:
                `REJECTED: Wrapping would break delimiter balance.\n\n` +
                `Balance errors:\n${errorDetails}\n\n` +
                `This is likely a bug in the wrapper text. The file was NOT modified.`,
            },
          ],
          isError: true,
        };
      }

      if (isDryRun) {
        const lines: string[] = [];
        lines.push(`Dry run — wrap lines ${start_line}-${effectiveEndLine} with "${wrapper}":`);
        lines.push('');
        lines.push('--- result ---');
        lines.push(wrappedText);
        lines.push('---');
        lines.push('');
        lines.push(`Balance: OK (${newBalance.topLevelForms} top-level forms)`);
        lines.push('');
        lines.push('Set dry_run to false to apply this change.');
        return {
          content: [{ type: 'text' as const, text: lines.join('\n') }],
        };
      }

      // Apply the change
      await writeFile(file_path, newSource, 'utf-8');

      return {
        content: [
          {
            type: 'text' as const,
            text:
              `Wrapped lines ${start_line}-${effectiveEndLine} with "${wrapper}" in ${file_path}\n` +
              `Balance: OK (${newBalance.topLevelForms} top-level forms)`,
          },
        ],
      };
    },
  );
}
