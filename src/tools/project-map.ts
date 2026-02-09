import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join, relative } from 'node:path';
import {
  scanSchemeFiles,
  parseDefinitions,
  extractModulePaths,
} from './parse-utils.js';

export function registerProjectMapTool(server: McpServer): void {
  server.registerTool(
    'gerbil_project_map',
    {
      title: 'Project Export Map',
      description:
        'Single call that returns all modules in a project with their exports, ' +
        'key definitions grouped by kind, and import dependencies. ' +
        'Reads gerbil.pkg for package context and scans all .ss files.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        project_path: z
          .string()
          .describe(
            'Directory containing the Gerbil project (with gerbil.pkg)',
          ),
      },
    },
    async ({ project_path }) => {
      // 1. Read package name from gerbil.pkg
      let packageName = '';
      try {
        const pkgContent = await readFile(
          join(project_path, 'gerbil.pkg'),
          'utf-8',
        );
        const pkgMatch = pkgContent.match(/\(package:\s+([^\s)]+)\)/);
        if (pkgMatch) packageName = pkgMatch[1];
      } catch {
        // No gerbil.pkg is fine
      }

      // 2. Scan all .ss files
      const ssFiles = await scanSchemeFiles(project_path);
      if (ssFiles.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No .ss files found in ${project_path}.`,
            },
          ],
        };
      }

      const sections: string[] = [];
      if (packageName) {
        sections.push(`Project: ${packageName}`);
      }
      sections.push(`Location: ${project_path}`);
      sections.push(`Modules: ${ssFiles.length}`);
      sections.push('');

      // 3. For each file, extract definitions, exports, imports
      for (const file of ssFiles) {
        const rel = relative(project_path, file);
        let content: string;
        try {
          content = await readFile(file, 'utf-8');
        } catch {
          continue;
        }

        const analysis = parseDefinitions(content);

        sections.push(`Module: ${rel}`);

        // Exports
        if (analysis.exports.length > 0) {
          const exportNames: string[] = [];
          for (const exp of analysis.exports) {
            const raw = exp.raw;
            if (raw.includes('#t')) {
              exportNames.push('#t (all)');
            } else {
              // Extract bare identifiers from export form
              const inner = raw
                .replace(/^\(export\s+/, '')
                .replace(/\)\s*$/, '')
                .trim();
              const tokens = inner
                .split(/\s+/)
                .filter(
                  (t) =>
                    t &&
                    !t.startsWith('(') &&
                    t !== ')' &&
                    !t.endsWith(':'),
                );
              exportNames.push(...tokens);
            }
          }
          if (exportNames.length > 0) {
            sections.push(`  Exports: ${exportNames.join(', ')}`);
          }
        }

        // Definitions grouped by kind
        const byKind = new Map<string, string[]>();
        for (const def of analysis.definitions) {
          if (!byKind.has(def.kind)) byKind.set(def.kind, []);
          byKind.get(def.kind)!.push(def.name);
        }

        const kindOrder = [
          'struct',
          'class',
          'interface',
          'procedure',
          'macro',
          'method',
          'constant',
          'inline',
          'values',
          'alias',
          'type',
        ];
        for (const kind of kindOrder) {
          const names = byKind.get(kind);
          if (names && names.length > 0) {
            const label =
              kind.charAt(0).toUpperCase() + kind.slice(1) + 's';
            sections.push(`  ${label}: ${names.join(', ')}`);
          }
        }

        // Import dependencies
        if (analysis.imports.length > 0) {
          const allMods: string[] = [];
          for (const imp of analysis.imports) {
            const mods = extractModulePaths(imp.raw);
            allMods.push(...mods);
          }
          if (allMods.length > 0) {
            const internal = allMods.filter(
              (m) =>
                m.startsWith('./') ||
                (packageName && m.startsWith(`:${packageName}`)),
            );
            const external = allMods.filter(
              (m) =>
                !m.startsWith('./') &&
                !(packageName && m.startsWith(`:${packageName}`)),
            );
            if (internal.length > 0) {
              sections.push(
                `  Internal imports: ${internal.join(', ')}`,
              );
            }
            if (external.length > 0) {
              sections.push(
                `  External imports: ${external.join(', ')}`,
              );
            }
          }
        }

        sections.push('');
      }

      return {
        content: [
          {
            type: 'text' as const,
            text: sections.join('\n').trimEnd(),
          },
        ],
      };
    },
  );
}
