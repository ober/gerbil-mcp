import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { relative } from 'node:path';
import { scanSchemeFiles, parseDefinitions } from './parse-utils.js';

export function registerWorkspaceSymbolsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_workspace_symbols',
    {
      title: 'Workspace Symbol Search',
      description:
        'Search for symbol definitions across all .ss files in a project directory. ' +
        'Returns matching definitions with name, kind, line number, and file path.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        query: z
          .string()
          .describe('Search query (substring match, case-insensitive)'),
        directory: z
          .string()
          .optional()
          .describe(
            'Directory to search in (absolute path). Defaults to current working directory.',
          ),
      },
    },
    async ({ query, directory }) => {
      const dir = directory || process.cwd();
      const queryLower = query.toLowerCase();

      const files = await scanSchemeFiles(dir);
      if (files.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No .ss files found in ${dir}.`,
            },
          ],
        };
      }

      const matches: Array<{
        name: string;
        kind: string;
        line: number;
        file: string;
      }> = [];

      for (const file of files) {
        try {
          const content = await readFile(file, 'utf-8');
          const analysis = parseDefinitions(content);
          for (const def of analysis.definitions) {
            if (def.name.toLowerCase().includes(queryLower)) {
              matches.push({
                name: def.name,
                kind: def.kind,
                line: def.line,
                file: relative(dir, file),
              });
            }
          }
        } catch {
          /* skip unreadable files */
        }
      }

      if (matches.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No definitions matching "${query}" found in ${dir}.`,
            },
          ],
        };
      }

      const sections: string[] = [
        `Symbols matching "${query}" (${matches.length} result${matches.length === 1 ? '' : 's'}):`,
        '',
      ];
      for (const m of matches) {
        sections.push(`  ${m.file}:${m.line}  ${m.name}  (${m.kind})`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
