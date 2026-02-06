import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join, relative } from 'node:path';
import {
  scanSchemeFiles,
  parseDefinitions,
  extractModulePaths,
} from './parse-utils.js';

export function registerProjectDepGraphTool(server: McpServer): void {
  server.registerTool(
    'gerbil_project_dep_graph',
    {
      title: 'Project Dependency Graph',
      description:
        'Visualize project module dependency graph as an ASCII tree. ' +
        'Shows which project modules import from which other project modules. ' +
        'External dependencies are listed separately. ' +
        'Pure static analysis — no subprocess, fast.',
      inputSchema: {
        project_path: z
          .string()
          .describe(
            'Directory containing the Gerbil project (with gerbil.pkg)',
          ),
      },
    },
    async ({ project_path }) => {
      // 1. Read package name
      let packageName = '';
      try {
        const pkgContent = await readFile(
          join(project_path, 'gerbil.pkg'),
          'utf-8',
        );
        const pkgMatch = pkgContent.match(/\(package:\s+([^\s)]+)\)/);
        if (pkgMatch) packageName = pkgMatch[1];
      } catch {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No gerbil.pkg found in ${project_path}.`,
            },
          ],
          isError: true,
        };
      }

      // 2. Scan .ss files
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

      // 3. Build adjacency: module -> { internalDeps, externalDeps }
      const moduleMap = new Map<
        string,
        { internalDeps: string[]; externalDeps: string[] }
      >();
      const allModuleIds = new Set<string>();

      // First pass: determine all module IDs
      for (const file of ssFiles) {
        const rel = relative(project_path, file)
          .replace(/\.ss$/, '')
          .replace(/\.scm$/, '');
        const modId = `:${packageName}/${rel}`;
        allModuleIds.add(modId);
      }

      // Second pass: parse imports
      for (const file of ssFiles) {
        const rel = relative(project_path, file)
          .replace(/\.ss$/, '')
          .replace(/\.scm$/, '');
        const modId = `:${packageName}/${rel}`;

        let content: string;
        try {
          content = await readFile(file, 'utf-8');
        } catch {
          continue;
        }

        const analysis = parseDefinitions(content);
        const internalDeps: string[] = [];
        const externalDeps: string[] = [];

        for (const imp of analysis.imports) {
          const mods = extractModulePaths(imp.raw);
          for (const m of mods) {
            // Resolve relative imports
            let resolved = m;
            if (m.startsWith('./')) {
              // Relative to the current file's directory
              const fileDir = relative(
                project_path,
                file.replace(/[/][^/]+$/, ''),
              );
              const relTarget = m.slice(2);
              resolved = `:${packageName}/${fileDir ? fileDir + '/' : ''}${relTarget}`;
            }

            if (
              allModuleIds.has(resolved) ||
              (packageName && resolved.startsWith(`:${packageName}/`))
            ) {
              if (resolved !== modId) {
                internalDeps.push(resolved);
              }
            } else {
              externalDeps.push(m);
            }
          }
        }

        moduleMap.set(modId, {
          internalDeps: [...new Set(internalDeps)],
          externalDeps: [...new Set(externalDeps)],
        });
      }

      // 4. Find root modules (not depended on by others)
      const depended = new Set<string>();
      for (const [, info] of moduleMap) {
        for (const dep of info.internalDeps) {
          depended.add(dep);
        }
      }

      const roots = [...moduleMap.keys()]
        .filter((m) => !depended.has(m))
        .sort();

      // If no clear roots (circular), use all modules
      const startNodes = roots.length > 0 ? roots : [...moduleMap.keys()].sort();

      // 5. Build ASCII tree
      const sections: string[] = [];
      sections.push(`Project: ${packageName}`);
      sections.push(`Modules: ${moduleMap.size}`);
      sections.push('');
      sections.push('Dependency tree:');

      const visited = new Set<string>();

      function shortName(modId: string): string {
        const prefix = `:${packageName}/`;
        return modId.startsWith(prefix) ? modId.slice(prefix.length) : modId;
      }

      function renderTree(
        modId: string,
        prefix: string,
        isLast: boolean,
      ): void {
        const connector = isLast ? '`-- ' : '|-- ';
        const name = shortName(modId);

        if (visited.has(modId)) {
          sections.push(`${prefix}${connector}${name} (circular)`);
          return;
        }

        sections.push(`${prefix}${connector}${name}`);
        visited.add(modId);

        const info = moduleMap.get(modId);
        if (info) {
          const childPrefix = prefix + (isLast ? '    ' : '|   ');
          const deps = info.internalDeps
            .filter((d) => moduleMap.has(d))
            .sort();

          for (let i = 0; i < deps.length; i++) {
            renderTree(deps[i], childPrefix, i === deps.length - 1);
          }
        }

        visited.delete(modId);
      }

      for (let i = 0; i < startNodes.length; i++) {
        renderTree(startNodes[i], '', i === startNodes.length - 1);
      }

      // 6. External dependencies summary
      const allExternal = new Set<string>();
      for (const [, info] of moduleMap) {
        for (const ext of info.externalDeps) {
          allExternal.add(ext);
        }
      }

      if (allExternal.size > 0) {
        sections.push('');
        sections.push('External dependencies:');
        const sorted = [...allExternal].sort();
        for (const ext of sorted) {
          sections.push(`  ${ext}`);
        }
      }

      return {
        content: [
          { type: 'text' as const, text: sections.join('\n') },
        ],
      };
    },
  );
}
