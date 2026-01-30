import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, writeFile } from 'node:fs/promises';
import { join, relative } from 'node:path';
import { scanSchemeFiles, findSymbolOccurrences } from './parse-utils.js';

function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function replaceSymbolInContent(
  content: string,
  oldName: string,
  newName: string,
): string {
  const escaped = escapeRegExp(oldName);
  // Use Scheme word boundaries for safe replacement
  const pattern = new RegExp(
    `(?<=[ \\t\\n\\r()\\[\\]{}"',;\`#]|^)${escaped}(?=[ \\t\\n\\r()\\[\\]{}"',;\`#]|$)`,
    'gm',
  );
  return content.replace(pattern, newName);
}

export function registerRenameSymbolTool(server: McpServer): void {
  server.registerTool(
    'gerbil_rename_symbol',
    {
      title: 'Project-Wide Rename',
      description:
        'Rename a symbol across all .ss files in a project directory. ' +
        'Default is dry-run mode (showing proposed changes). ' +
        'Set dry_run to false to apply changes. Uses word-boundary detection ' +
        'to avoid renaming partial matches.',
      inputSchema: {
        old_name: z.string().describe('Current symbol name to rename'),
        new_name: z.string().describe('New symbol name'),
        directory: z
          .string()
          .describe('Project directory to search (absolute path)'),
        dry_run: z
          .boolean()
          .optional()
          .describe(
            'If true (default), only show changes without applying them',
          ),
      },
    },
    async ({ old_name, new_name, directory, dry_run }) => {
      const isDryRun = dry_run !== false;

      if (old_name === new_name) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Old and new names are identical. Nothing to do.',
            },
          ],
        };
      }

      const files = await scanSchemeFiles(directory);
      if (files.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No .ss files found in ${directory}.`,
            },
          ],
        };
      }

      const allChanges: Array<{
        file: string;
        line: number;
        column: number;
        lineText: string;
      }> = [];

      for (const file of files) {
        try {
          const content = await readFile(file, 'utf-8');
          const occurrences = findSymbolOccurrences(content, old_name);
          for (const occ of occurrences) {
            allChanges.push({
              file: relative(directory, file),
              line: occ.line,
              column: occ.column,
              lineText: occ.lineText,
            });
          }
        } catch {
          /* skip unreadable files */
        }
      }

      if (allChanges.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No occurrences of "${old_name}" found in ${directory}.`,
            },
          ],
        };
      }

      if (isDryRun) {
        const sections: string[] = [
          `Dry run: ${allChanges.length} occurrence${allChanges.length === 1 ? '' : 's'} of "${old_name}" -> "${new_name}":`,
          '',
        ];

        const byFile = new Map<string, typeof allChanges>();
        for (const ch of allChanges) {
          if (!byFile.has(ch.file)) byFile.set(ch.file, []);
          byFile.get(ch.file)!.push(ch);
        }

        for (const [file, changes] of byFile) {
          sections.push(`  ${file} (${changes.length}):`);
          for (const ch of changes) {
            const preview = ch.lineText.trim();
            const replaced = replaceSymbolInContent(
              preview,
              old_name,
              new_name,
            );
            sections.push(`    L${ch.line}:${ch.column}  ${preview}`);
            if (replaced !== preview) {
              sections.push(`         -> ${replaced}`);
            }
          }
        }

        sections.push('');
        sections.push('Set dry_run to false to apply these changes.');

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      // Apply changes
      const filesModified = new Set<string>();

      for (const file of files) {
        const fullPath = file;
        try {
          const content = await readFile(fullPath, 'utf-8');
          const replaced = replaceSymbolInContent(content, old_name, new_name);
          if (replaced !== content) {
            await writeFile(fullPath, replaced, 'utf-8');
            filesModified.add(relative(directory, fullPath));
          }
        } catch {
          /* skip unreadable files */
        }
      }

      return {
        content: [
          {
            type: 'text' as const,
            text:
              `Renamed "${old_name}" -> "${new_name}": ` +
              `${allChanges.length} occurrence${allChanges.length === 1 ? '' : 's'} ` +
              `in ${filesModified.size} file${filesModified.size === 1 ? '' : 's'}.`,
          },
        ],
      };
    },
  );
}
