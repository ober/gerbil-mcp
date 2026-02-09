import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, stat } from 'node:fs/promises';
import { basename } from 'node:path';
import { parseDefinitions, extractModulePaths } from './parse-utils.js';

export function registerFileSummaryTool(server: McpServer): void {
  server.registerTool(
    'gerbil_file_summary',
    {
      title: 'File Summary',
      description:
        'Structural overview of a Gerbil source file without reading the whole file. ' +
        'Shows imports, exports, definitions grouped by kind, and top-level form locations. ' +
        'Pure TypeScript — no subprocess, fast.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .describe('Absolute path to a Gerbil source file (.ss or .scm)'),
      },
    },
    async ({ file_path }) => {
      let content: string;
      try {
        content = await readFile(file_path, 'utf-8');
      } catch (err) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to read file: ${file_path}`,
            },
          ],
          isError: true,
        };
      }

      if (content.trim().length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `File: ${basename(file_path)} (empty file)\nPath: ${file_path}`,
            },
          ],
        };
      }

      const lineCount = content.split('\n').length;
      const analysis = parseDefinitions(content);

      const sections: string[] = [
        `File: ${basename(file_path)} (${lineCount} lines)`,
        `Path: ${file_path}`,
        '',
      ];

      // Imports
      if (analysis.imports.length > 0) {
        const modulePaths = analysis.imports.flatMap((imp) =>
          extractModulePaths(imp.raw),
        );
        const uniqueModules = [...new Set(modulePaths)];
        sections.push(`Imports: ${uniqueModules.join(', ')}`);
      } else {
        sections.push('Imports: (none)');
      }

      // Exports
      if (analysis.exports.length > 0) {
        const exportSymbols = analysis.exports.flatMap((exp) => {
          // Extract symbol names from export form
          const raw = exp.raw;
          // Remove (export and trailing )
          const inner = raw.replace(/^\(export\s+/, '').replace(/\)\s*$/, '');
          if (inner === '#t') return ['#t (all)'];
          // Split on whitespace, filter out sub-forms
          return inner
            .split(/\s+/)
            .filter((s) => s && !s.startsWith('(') && !s.startsWith('#'));
        });
        sections.push(`Exports: ${exportSymbols.join(', ')}`);
      } else {
        sections.push('Exports: (none)');
      }

      sections.push('');

      // Group definitions by kind
      const byKind = new Map<string, typeof analysis.definitions>();
      for (const def of analysis.definitions) {
        const group = byKind.get(def.kind) ?? [];
        group.push(def);
        byKind.set(def.kind, group);
      }

      if (byKind.size > 0) {
        sections.push('Structure:');
        const kindOrder = [
          'procedure', 'struct', 'class', 'interface', 'macro',
          'method', 'constant', 'inline', 'values', 'alias', 'type',
        ];

        for (const kind of kindOrder) {
          const defs = byKind.get(kind);
          if (!defs || defs.length === 0) continue;

          const kindLabel = kind.charAt(0).toUpperCase() + kind.slice(1) + 's';
          sections.push(`  ${kindLabel} (${defs.length}):`);

          // Show an example with context from source
          const first = defs[0];
          const exampleLine = getDefinitionLine(content, first.line - 1);
          if (exampleLine) {
            sections.push(`    Example: ${exampleLine.trim()}`);
          }

          if (defs.length > 1) {
            const otherNames = defs.slice(1).map((d) => d.name);
            // Truncate if too many
            if (otherNames.length > 10) {
              const shown = otherNames.slice(0, 10).join(', ');
              sections.push(
                `    Others: ${shown}, ... (${otherNames.length - 10} more)`,
              );
            } else {
              sections.push(`    Others: ${otherNames.join(', ')}`);
            }
          }
        }
      } else {
        sections.push('Structure: (no definitions found)');
      }

      // Top-level forms summary — show significant non-definition forms
      const lines = content.split('\n');
      const topForms: Array<{ line: number; summary: string }> = [];
      for (let i = 0; i < lines.length; i++) {
        const trimmed = lines[i].trimStart();
        if (!trimmed || trimmed.startsWith(';') || trimmed.startsWith('#|'))
          continue;
        if (!trimmed.startsWith('(')) continue;

        const rest = trimmed.slice(1);
        // Skip import/export/def forms (already shown above)
        if (
          rest.startsWith('import ') ||
          rest.startsWith('export ') ||
          rest.startsWith('def ') ||
          rest.startsWith('def* ') ||
          rest.startsWith('define ') ||
          rest.startsWith('defstruct ') ||
          rest.startsWith('defclass ') ||
          rest.startsWith('definterface ') ||
          rest.startsWith('defrules ') ||
          rest.startsWith('defrule ') ||
          rest.startsWith('defsyntax ') ||
          rest.startsWith('defmacro ') ||
          rest.startsWith('defmethod ') ||
          rest.startsWith('defconst ') ||
          rest.startsWith('definline ') ||
          rest.startsWith('defvalues ') ||
          rest.startsWith('defalias ') ||
          rest.startsWith('deftype ')
        )
          continue;

        // Extract the first token as a summary
        const spaceIdx = rest.search(/[\s)]/);
        const head = spaceIdx === -1 ? rest : rest.slice(0, spaceIdx);
        if (head) {
          topForms.push({ line: i + 1, summary: `(${head} ...)` });
        }
      }

      if (topForms.length > 0) {
        sections.push('');
        sections.push('Top-level forms:');
        // Show up to 15 forms
        const shown = topForms.slice(0, 15);
        for (const form of shown) {
          sections.push(`  L${form.line}: ${form.summary}`);
        }
        if (topForms.length > 15) {
          sections.push(`  ... (${topForms.length - 15} more)`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

/**
 * Get the definition line from source content, truncated to a reasonable length.
 */
function getDefinitionLine(content: string, lineIdx: number): string | null {
  const lines = content.split('\n');
  if (lineIdx < 0 || lineIdx >= lines.length) return null;
  const line = lines[lineIdx];
  if (line.length > 100) {
    return line.slice(0, 97) + '...';
  }
  return line;
}
