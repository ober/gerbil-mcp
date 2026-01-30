import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join, relative, basename } from 'node:path';
import {
  scanSchemeFiles,
  parseDefinitions,
  parseBuildTargets,
  extractModulePaths,
} from './parse-utils.js';

export function registerProjectInfoTool(server: McpServer): void {
  server.registerTool(
    'gerbil_project_info',
    {
      title: 'Project Context Overview',
      description:
        'Single-call summary of a Gerbil project: package name, build targets, ' +
        'source files, and external dependencies. Reads gerbil.pkg, build.ss, ' +
        'and scans source files.',
      inputSchema: {
        project_path: z
          .string()
          .describe(
            'Directory containing the Gerbil project (with gerbil.pkg)',
          ),
      },
    },
    async ({ project_path }) => {
      const sections: string[] = [];

      // 1. Parse gerbil.pkg
      let packageName = '';
      try {
        const pkgContent = await readFile(
          join(project_path, 'gerbil.pkg'),
          'utf-8',
        );
        const pkgMatch = pkgContent.match(/\(package:\s+([^\s)]+)\)/);
        if (pkgMatch) packageName = pkgMatch[1];
        sections.push(`Package: ${packageName || '<unnamed>'}`);
        sections.push(`Location: ${project_path}`);
        sections.push('');
      } catch {
        sections.push(`No gerbil.pkg found in ${project_path}`);
        sections.push('');
      }

      // 2. Parse build.ss
      try {
        const buildContent = await readFile(
          join(project_path, 'build.ss'),
          'utf-8',
        );
        const targets = parseBuildTargets(buildContent);
        if (targets.libraries.length > 0 || targets.executables.length > 0) {
          sections.push('Build Targets:');
          for (const lib of targets.libraries) {
            sections.push(`  [lib] ${lib}`);
          }
          for (const exe of targets.executables) {
            sections.push(`  [exe] ${exe.module} -> ${exe.binary}`);
          }
          sections.push('');
        }
      } catch {
        sections.push('No build.ss found.');
        sections.push('');
      }

      // 3. Scan source files
      const ssFiles = await scanSchemeFiles(project_path);
      if (ssFiles.length > 0) {
        sections.push(`Source Files (${ssFiles.length}):`);
        const byDir = new Map<string, string[]>();
        for (const f of ssFiles) {
          const rel = relative(project_path, f);
          const lastSlash = rel.lastIndexOf('/');
          const dir = lastSlash !== -1 ? rel.slice(0, lastSlash) : '.';
          if (!byDir.has(dir)) byDir.set(dir, []);
          byDir.get(dir)!.push(basename(f));
        }
        for (const [dir, files] of Array.from(byDir.entries()).sort()) {
          sections.push(`  ${dir}/`);
          for (const f of files.sort()) {
            sections.push(`    ${f}`);
          }
        }
        sections.push('');
      }

      // 4. External dependencies
      const allImports = new Set<string>();
      for (const f of ssFiles) {
        try {
          const content = await readFile(f, 'utf-8');
          const analysis = parseDefinitions(content);
          for (const imp of analysis.imports) {
            const mods = extractModulePaths(imp.raw);
            for (const m of mods) {
              if (
                m.startsWith(':') &&
                (!packageName || !m.startsWith(`:${packageName}`))
              ) {
                allImports.add(m);
              }
            }
          }
        } catch {
          /* skip unreadable files */
        }
      }

      if (allImports.size > 0) {
        const sorted = Array.from(allImports).sort();
        sections.push(`External Dependencies (${sorted.length}):`);
        for (const dep of sorted) {
          sections.push(`  ${dep}`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
