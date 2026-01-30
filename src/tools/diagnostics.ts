import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { runGxc } from '../gxi.js';
import {
  parseGxcErrors,
  scanSchemeFiles,
  type Diagnostic,
} from './parse-utils.js';

export function registerDiagnosticsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_diagnostics',
    {
      title: 'Structured Diagnostics',
      description:
        'Run gxc -S on a file (or all .ss files in a project) and return structured ' +
        'diagnostics with file, line, column, severity, and message. ' +
        'Provide either file_path for a single file or project_path for project-wide checking.',
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Path to a single .ss file to check'),
        project_path: z
          .string()
          .optional()
          .describe(
            'Path to a project directory (checks files from build.ss or scans directory)',
          ),
      },
    },
    async ({ file_path, project_path }) => {
      if (!file_path && !project_path) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Either "file_path" or "project_path" must be provided.',
            },
          ],
          isError: true,
        };
      }

      const allDiagnostics: Diagnostic[] = [];

      if (file_path) {
        const diags = await checkFile(file_path);
        if (typeof diags === 'string') {
          return {
            content: [{ type: 'text' as const, text: diags }],
            isError: true,
          };
        }
        allDiagnostics.push(...diags);
      } else {
        const files = await getProjectFiles(project_path!);
        if (files.length === 0) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `No .ss files found in ${project_path}.`,
              },
            ],
          };
        }

        for (const f of files) {
          const diags = await checkFile(f);
          if (typeof diags !== 'string') {
            allDiagnostics.push(...diags);
          } else {
            // Treat infrastructure errors as diagnostics
            allDiagnostics.push({
              file: f,
              line: null,
              column: null,
              severity: 'error',
              message: diags,
            });
          }
        }
      }

      if (allDiagnostics.length === 0) {
        const target = file_path || project_path;
        return {
          content: [
            {
              type: 'text' as const,
              text: `No diagnostics. ${target} compiled cleanly.`,
            },
          ],
        };
      }

      const errors = allDiagnostics.filter((d) => d.severity === 'error');
      const warnings = allDiagnostics.filter((d) => d.severity === 'warning');
      const infos = allDiagnostics.filter((d) => d.severity === 'info');

      const sections: string[] = [
        `Diagnostics (${allDiagnostics.length}): ${errors.length} error(s), ${warnings.length} warning(s), ${infos.length} info`,
        '',
      ];

      for (const d of allDiagnostics) {
        const loc = d.line
          ? `${d.file}:${d.line}${d.column ? ':' + d.column : ''}`
          : d.file;
        sections.push(`  [${d.severity.toUpperCase()}] ${loc}`);
        sections.push(`    ${d.message}`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: errors.length > 0,
      };
    },
  );
}

async function checkFile(
  filePath: string,
): Promise<Diagnostic[] | string> {
  const result = await runGxc(filePath, { timeout: 30_000 });

  if (result.timedOut) {
    return `Compilation of ${filePath} timed out after 30 seconds.`;
  }
  if (result.exitCode === 127) {
    return 'gxc not found. Ensure Gerbil is installed.';
  }
  if (result.exitCode !== 0) {
    return parseGxcErrors(result.stderr, filePath);
  }
  return [];
}

async function getProjectFiles(projectPath: string): Promise<string[]> {
  // Try build.ss first to get declared modules
  try {
    const buildContent = await readFile(
      join(projectPath, 'build.ss'),
      'utf-8',
    );
    const modules = extractBuildModules(buildContent);
    if (modules.length > 0) {
      return modules.map((m) => join(projectPath, m + '.ss'));
    }
  } catch {
    /* fall through to directory scan */
  }

  return scanSchemeFiles(projectPath);
}

function extractBuildModules(content: string): string[] {
  // Extract plain quoted strings from defbuild-script (library modules)
  const matches = content.match(/"([^"]+)"/g) || [];
  return matches.map((m) => m.slice(1, -1));
}
