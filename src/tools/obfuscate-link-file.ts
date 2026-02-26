import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, writeFile, stat } from 'node:fs/promises';
import { createHash } from 'node:crypto';

/**
 * Obfuscate symbol name strings in a Gambit link file.
 *
 * The link file contains ___DEF_SYM, ___DEF_GLO, and ___DEF_SUP macros
 * that embed readable Scheme symbol names as C string literals.
 * This replaces those strings with short hashes.
 */
function obfuscateSymbolNames(content: string): { result: string; count: number } {
  let count = 0;

  // Pattern: ___DEF_SYM(idx, c_name, "readable-name")
  // Pattern: ___DEF_GLO(idx, c_name, "readable-name")
  // Pattern: ___DEF_SUP(idx, c_name, "readable-name")
  const result = content.replace(
    /(___DEF_(?:SYM|GLO|SUP)\s*\(\s*\d+\s*,\s*[A-Za-z_][A-Za-z0-9_]*\s*,\s*)"([^"]*)"/g,
    (_match, prefix, name) => {
      count++;
      const hash = createHash('md5').update(name).digest('hex').slice(0, 8);
      return `${prefix}"${hash}"`;
    },
  );

  // Also handle ___NSTR patterns (Gambit's encoded string format)
  // These appear as string literals in symbol name arrays
  const result2 = result.replace(
    /(___DEF_(?:SYM|GLO|SUP)_NS\s*\(\s*\d+\s*,\s*[A-Za-z_][A-Za-z0-9_]*\s*,\s*)"([^"]*)"/g,
    (_match, prefix, name) => {
      count++;
      const hash = createHash('md5').update(name).digest('hex').slice(0, 8);
      return `${prefix}"${hash}"`;
    },
  );

  return { result: result2, count };
}

export function registerObfuscateLinkFileTool(server: McpServer): void {
  server.registerTool(
    'gerbil_obfuscate_link_file',
    {
      title: 'Obfuscate Gambit Link File',
      description:
        'Post-process a Gambit-generated link file (*__exe_.c or *_.c) to replace all ' +
        'Scheme symbol name strings with short hashes. The link file is the primary source ' +
        'of symbol name leaks in compiled Gerbil executables — it contains ___sym_names[], ' +
        '___glo_names[], and ___sup_names[] arrays with ALL Scheme identifiers in plaintext. ' +
        'Source-level obfuscation before gcc is more effective than post-build binary patching. ' +
        'Dry-run by default.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        file_path: z
          .string()
          .describe('Path to the link file (*__exe_.c or *_.c)'),
        dry_run: z
          .boolean()
          .optional()
          .describe('Preview changes without modifying the file (default: true)'),
        backup: z
          .boolean()
          .optional()
          .describe('Create a .bak backup before modifying (default: true when not dry_run)'),
      },
    },
    async ({ file_path, dry_run, backup }) => {
      if (!file_path) {
        return {
          content: [
            { type: 'text' as const, text: 'The "file_path" parameter is required.' },
          ],
          isError: true,
        };
      }

      const isDryRun = dry_run ?? true;
      const doBackup = backup ?? true;

      // Verify file exists
      try {
        const st = await stat(file_path);
        if (!st.isFile()) {
          return {
            content: [
              { type: 'text' as const, text: `Not a file: ${file_path}` },
            ],
            isError: true,
          };
        }
      } catch {
        return {
          content: [
            { type: 'text' as const, text: `File not found: ${file_path}` },
          ],
          isError: true,
        };
      }

      let content: string;
      try {
        content = await readFile(file_path, 'utf-8');
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            { type: 'text' as const, text: `Failed to read file: ${msg}` },
          ],
          isError: true,
        };
      }

      // Check if this looks like a Gambit link file
      if (!content.includes('___DEF_SYM') && !content.includes('___DEF_GLO')) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `File does not appear to be a Gambit link file (no ___DEF_SYM/___DEF_GLO found): ${file_path}`,
            },
          ],
          isError: true,
        };
      }

      const { result, count } = obfuscateSymbolNames(content);

      if (count === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No symbol name strings found to obfuscate in ${file_path}.`,
            },
          ],
        };
      }

      if (isDryRun) {
        // Show a sample of before/after
        const beforeLines = content.split('\n').filter((l) =>
          l.match(/___DEF_(?:SYM|GLO|SUP)\s*\(/)
        );
        const afterLines = result.split('\n').filter((l) =>
          l.match(/___DEF_(?:SYM|GLO|SUP)\s*\(/)
        );

        const samples: string[] = [];
        for (let i = 0; i < Math.min(3, beforeLines.length); i++) {
          samples.push(`  Before: ${beforeLines[i].trim()}`);
          samples.push(`  After:  ${afterLines[i].trim()}`);
          samples.push('');
        }

        return {
          content: [
            {
              type: 'text' as const,
              text: [
                `Dry run: would obfuscate ${count} symbol name string(s) in ${file_path}`,
                '',
                'Sample transformations:',
                '',
                ...samples,
                `To apply: call with dry_run: false`,
              ].join('\n'),
            },
          ],
        };
      }

      // Write backup
      if (doBackup) {
        try {
          await writeFile(file_path + '.bak', content, 'utf-8');
        } catch (err) {
          const msg = err instanceof Error ? err.message : 'Unknown error';
          return {
            content: [
              { type: 'text' as const, text: `Failed to create backup: ${msg}` },
            ],
            isError: true,
          };
        }
      }

      // Write obfuscated file
      try {
        await writeFile(file_path, result, 'utf-8');
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            { type: 'text' as const, text: `Failed to write obfuscated file: ${msg}` },
          ],
          isError: true,
        };
      }

      return {
        content: [
          {
            type: 'text' as const,
            text: [
              `Obfuscated ${count} symbol name string(s) in ${file_path}`,
              doBackup ? `Backup saved to: ${file_path}.bak` : '',
              '',
              'Next steps:',
              '1. Compile the obfuscated link file with gcc',
              '2. Link into the final executable',
              '3. Run strip -s on the final binary',
            ].filter(Boolean).join('\n'),
          },
        ],
      };
    },
  );
}
