/**
 * gerbil_balanced_replace — Like Edit but with balance validation.
 * Rejects edits that break delimiter balance.
 *
 * Pure TypeScript, no subprocess.
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, writeFile } from 'node:fs/promises';
import { checkBalance } from './check-balance.js';

export function registerBalancedReplaceTool(server: McpServer): void {
  server.registerTool(
    'gerbil_balanced_replace',
    {
      title: 'Balanced String Replace',
      description:
        'Like Edit/string-replace but validates parenthesis/bracket/brace balance before and after. ' +
        'Rejects edits that break balance. Dry-run by default. ' +
        'Pure TypeScript — no subprocess, runs in milliseconds.',
      inputSchema: {
        file_path: z.string().describe('Absolute path to the Gerbil source file'),
        old_string: z.string().describe('The exact text to find and replace'),
        new_string: z.string().describe('The replacement text'),
        dry_run: z
          .boolean()
          .optional()
          .describe(
            'If true (default), only preview the change without writing. Set to false to apply.',
          ),
      },
    },
    async ({ file_path, old_string, new_string, dry_run }) => {
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

      // Find old_string
      const idx = source.indexOf(old_string);
      if (idx === -1) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `old_string not found in ${file_path}. Make sure the text matches exactly (including whitespace and indentation).`,
            },
          ],
          isError: true,
        };
      }

      // Check for ambiguity (multiple matches)
      const secondIdx = source.indexOf(old_string, idx + 1);
      if (secondIdx !== -1) {
        return {
          content: [
            {
              type: 'text' as const,
              text:
                `old_string appears multiple times in ${file_path}. ` +
                `Provide a larger string with more surrounding context to make it unique.`,
            },
          ],
          isError: true,
        };
      }

      // Check original balance
      const originalBalance = checkBalance(source);

      // Compute replacement
      const replaced = source.slice(0, idx) + new_string + source.slice(idx + old_string.length);

      // Check new balance
      const newBalance = checkBalance(replaced);

      // Decision logic
      if (originalBalance.ok && !newBalance.ok) {
        // Edit broke balance — reject
        const errorDetails = newBalance.errors
          .map((err) => {
            switch (err.kind) {
              case 'unclosed':
                return `  Unclosed '${err.char}' at line ${err.line}, col ${err.col}${err.context ? ` (near '${err.context}')` : ''}`;
              case 'unexpected':
                return `  Unexpected closer '${err.char}' at line ${err.line}, col ${err.col}`;
              case 'mismatch':
                return `  Mismatched '${err.char}' at line ${err.line}, col ${err.col} — expected '${err.expected}'`;
            }
          })
          .join('\n');

        return {
          content: [
            {
              type: 'text' as const,
              text:
                `REJECTED: This edit would break delimiter balance.\n\n` +
                `Balance errors in result:\n${errorDetails}\n\n` +
                `The file was NOT modified. Fix the new_string to maintain balanced delimiters.`,
            },
          ],
          isError: true,
        };
      }

      // Compute a summary
      const lines: string[] = [];

      if (!originalBalance.ok && newBalance.ok) {
        lines.push('Note: This edit FIXES a pre-existing balance issue.');
      } else if (!originalBalance.ok && !newBalance.ok) {
        lines.push(
          'Warning: The file has pre-existing balance issues. ' +
            'The edit does not make them worse, but the file is still unbalanced.',
        );
      }

      // Compute line numbers for context
      const beforeReplace = source.slice(0, idx);
      const startLine = beforeReplace.split('\n').length;
      const oldLines = old_string.split('\n').length;
      const newLines = new_string.split('\n').length;

      if (isDryRun) {
        lines.push(`Dry run — change at line ${startLine}:`);
        lines.push('');
        lines.push('--- old ---');
        lines.push(old_string);
        lines.push('--- new ---');
        lines.push(new_string);
        lines.push('---');
        lines.push('');
        lines.push(`Lines: ${oldLines} -> ${newLines}`);
        lines.push(`Balance: OK (${newBalance.topLevelForms} top-level forms)`);
        lines.push('');
        lines.push('Set dry_run to false to apply this change.');

        return {
          content: [{ type: 'text' as const, text: lines.join('\n') }],
        };
      }

      // Apply the change
      await writeFile(file_path, replaced, 'utf-8');

      lines.push(`Applied change at line ${startLine} in ${file_path}`);
      lines.push(`Lines: ${oldLines} -> ${newLines}`);
      lines.push(`Balance: OK (${newBalance.topLevelForms} top-level forms)`);

      return {
        content: [{ type: 'text' as const, text: lines.join('\n') }],
      };
    },
  );
}
