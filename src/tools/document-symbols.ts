import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { parseDefinitions } from './parse-utils.js';

export function registerDocumentSymbolsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_document_symbols',
    {
      title: 'Document Symbols with Positions',
      description:
        'List all definitions in a Gerbil source file with name, kind, and line number. ' +
        'Returns structs, classes, procedures, macros, methods, constants, imports, and exports.',
      inputSchema: {
        file_path: z
          .string()
          .describe(
            'Absolute path to a Gerbil source file (.ss or .scm)',
          ),
      },
    },
    async ({ file_path }) => {
      let content: string;
      try {
        content = await readFile(file_path, 'utf-8');
      } catch (err) {
        const msg =
          err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            { type: 'text' as const, text: `Failed to read file: ${msg}` },
          ],
          isError: true,
        };
      }

      if (!content.trim()) {
        return {
          content: [{ type: 'text' as const, text: 'File is empty.' }],
        };
      }

      const analysis = parseDefinitions(content);
      const sections: string[] = [`File: ${file_path}`, ''];

      if (analysis.imports.length > 0) {
        sections.push(`Imports (${analysis.imports.length}):`);
        for (const imp of analysis.imports) {
          const display =
            imp.raw.length > 120 ? imp.raw.slice(0, 120) + '...' : imp.raw;
          sections.push(`  L${imp.line}: ${display}`);
        }
        sections.push('');
      }

      if (analysis.exports.length > 0) {
        sections.push(`Exports (${analysis.exports.length}):`);
        for (const exp of analysis.exports) {
          const display =
            exp.raw.length > 120 ? exp.raw.slice(0, 120) + '...' : exp.raw;
          sections.push(`  L${exp.line}: ${display}`);
        }
        sections.push('');
      }

      if (analysis.definitions.length > 0) {
        sections.push(`Definitions (${analysis.definitions.length}):`);
        for (const def of analysis.definitions) {
          sections.push(`  L${def.line}: ${def.name}  (${def.kind})`);
        }
      }

      if (
        analysis.definitions.length === 0 &&
        analysis.imports.length === 0 &&
        analysis.exports.length === 0
      ) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No definitions found in ${file_path}.`,
            },
          ],
        };
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
