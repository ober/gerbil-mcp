/**
 * gerbil_splice_form — Remove a wrapper form, keeping selected children.
 *
 * Pure TypeScript, no subprocess.
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, writeFile } from 'node:fs/promises';
import { checkBalance } from './check-balance.js';
import { findFormAt } from './scheme-scanner.js';

export function registerSpliceFormTool(server: McpServer): void {
  server.registerTool(
    'gerbil_splice_form',
    {
      title: 'Splice Form',
      description:
        'Remove a wrapper form, keeping selected children (inverse of wrap). ' +
        'E.g. splicing (when cond (do-x) (do-y)) with keep_children [2, 3] produces (do-x)\\n(do-y). ' +
        'Child indices are 1-based (1=head token, 2=first arg, etc.). ' +
        'Default: keep all children except the head (index 1). ' +
        'Dry-run by default. Pure TypeScript — no subprocess.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z.string().describe('Absolute path to the Gerbil source file'),
        line: z
          .number()
          .int()
          .describe('Line number (1-based) where the form to splice starts'),
        keep_children: z
          .array(z.number().int())
          .optional()
          .describe(
            'Which children to keep (1-based indices). Default: all except index 1 (the head). ' +
              'E.g. for (when cond body1 body2): 1=when, 2=cond, 3=body1, 4=body2.',
          ),
        dry_run: z
          .boolean()
          .optional()
          .describe(
            'If true (default), only preview the change without writing. Set to false to apply.',
          ),
      },
    },
    async ({ file_path, line: targetLine, keep_children, dry_run }) => {
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

      // Find the form at the target line
      const form = findFormAt(source, targetLine);
      if (!form) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No form found at or near line ${targetLine}.`,
            },
          ],
          isError: true,
        };
      }

      const children = form.children;
      if (children.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Form at line ${form.start.line} has no children to splice.`,
            },
          ],
          isError: true,
        };
      }

      // Determine which children to keep (1-based indices)
      let keepIndices: number[];
      if (keep_children && keep_children.length > 0) {
        keepIndices = keep_children;
      } else {
        // Default: all except head (index 1)
        keepIndices = [];
        for (let ci = 2; ci <= children.length; ci++) {
          keepIndices.push(ci);
        }
      }

      // Validate indices
      for (const idx of keepIndices) {
        if (idx < 1 || idx > children.length) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Child index ${idx} is out of range (form has ${children.length} children).`,
              },
            ],
            isError: true,
          };
        }
      }

      if (keepIndices.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No children selected to keep. This would delete the entire form.`,
            },
          ],
          isError: true,
        };
      }

      // Extract kept children text
      const keptTexts: string[] = [];
      for (const idx of keepIndices) {
        keptTexts.push(children[idx - 1].text);
      }

      // Detect indentation of the original form
      const sourceLines = source.split('\n');
      const formLineText = sourceLines[form.start.line - 1] || '';
      const leadingMatch = formLineText.match(/^(\s*)/);
      const baseIndent = leadingMatch ? leadingMatch[1] : '';

      // Build spliced text with proper indentation
      const splicedLines: string[] = [];
      for (const text of keptTexts) {
        const textLines = text.split('\n');
        for (let li = 0; li < textLines.length; li++) {
          const trimmed = textLines[li].replace(/^\s*/, '');
          if (trimmed.length === 0 && li > 0) {
            splicedLines.push('');
          } else if (li === 0) {
            splicedLines.push(baseIndent + trimmed);
          } else {
            splicedLines.push(baseIndent + '  ' + trimmed);
          }
        }
      }

      const splicedText = splicedLines.join('\n');

      // Build new file content by replacing the form span
      const before = source.slice(0, form.start.offset);
      const after = source.slice(form.end.offset);
      const newSource = before + splicedText + after;

      // Validate balance
      const originalBalance = checkBalance(source);
      const newBalance = checkBalance(newSource);

      if (originalBalance.ok && !newBalance.ok) {
        const errorDetails = newBalance.errors
          .map((e) => `  ${e.kind}: '${e.char}' at line ${e.line}, col ${e.col}`)
          .join('\n');
        return {
          content: [
            {
              type: 'text' as const,
              text:
                `REJECTED: Splicing would break delimiter balance.\n\n` +
                `Balance errors:\n${errorDetails}\n\n` +
                `The file was NOT modified.`,
            },
          ],
          isError: true,
        };
      }

      // Format children summary for display
      const childSummary = children
        .map((c, i) => `  ${i + 1}: ${c.kind} "${c.text.length > 40 ? c.text.slice(0, 40) + '...' : c.text}"`)
        .join('\n');

      if (isDryRun) {
        const lines: string[] = [];
        lines.push(
          `Dry run — splice form at line ${form.start.line} (${form.opener}${form.headToken || ''}...):`,
        );
        lines.push('');
        lines.push('Children:');
        lines.push(childSummary);
        lines.push('');
        lines.push(`Keeping indices: [${keepIndices.join(', ')}]`);
        lines.push('');
        lines.push('--- result ---');
        lines.push(splicedText);
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
              `Spliced form at line ${form.start.line} in ${file_path}\n` +
              `Kept children: [${keepIndices.join(', ')}]\n` +
              `Balance: OK (${newBalance.topLevelForms} top-level forms)`,
          },
        ],
      };
    },
  );
}
