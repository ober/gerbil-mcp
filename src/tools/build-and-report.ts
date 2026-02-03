import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { runGerbilCmd } from '../gxi.js';
import { parseGxcErrors, type Diagnostic } from './parse-utils.js';

export function registerBuildAndReportTool(server: McpServer): void {
  server.registerTool(
    'gerbil_build_and_report',
    {
      title: 'Build and Report',
      description:
        'Run `gerbil build` on a project directory and return structured diagnostics. ' +
        'On success, reports a summary. On failure, parses compiler errors into ' +
        'structured file:line:column diagnostics. ' +
        'Uses the modern `gerbil` CLI (not gxpkg).',
      inputSchema: {
        project_path: z
          .string()
          .describe(
            'Directory containing the Gerbil project (with gerbil.pkg)',
          ),
        flags: z
          .array(z.string())
          .optional()
          .describe(
            'Extra build flags: "--release", "--optimized", "--debug"',
          ),
        context_lines: z
          .number()
          .optional()
          .describe(
            'Lines of source context to show around each error (default: 3). Set to 0 to disable.',
          ),
      },
    },
    async ({ project_path, flags, context_lines }) => {
      // Detect Makefile and extract targets
      const makefileNote = await detectMakefile(project_path);

      const args = ['build', ...(flags ?? [])];

      const result = await runGerbilCmd(args, {
        cwd: project_path,
        timeout: 120_000,
      });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Build timed out after 120 seconds.',
            },
          ],
          isError: true,
        };
      }

      if (result.exitCode === 127) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'gerbil CLI not found. Ensure Gerbil is installed and the gerbil binary is in PATH.',
            },
          ],
          isError: true,
        };
      }

      // Success path
      if (result.exitCode === 0) {
        const output = [result.stdout, result.stderr]
          .filter(Boolean)
          .join('\n')
          .trim();

        const flagStr =
          flags && flags.length > 0 ? ` (${flags.join(', ')})` : '';
        const sections: string[] = [
          `Build succeeded${flagStr}.`,
        ];
        if (output) {
          sections.push('');
          sections.push(output);
        }
        if (makefileNote) {
          sections.push('');
          sections.push(makefileNote);
        }

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      // Failure path — parse stderr into diagnostics
      const combined = [result.stdout, result.stderr]
        .filter(Boolean)
        .join('\n')
        .trim();

      const diagnostics: Diagnostic[] = parseGxcErrors(
        combined,
        project_path,
      );

      if (diagnostics.length === 0) {
        // Could not parse structured errors — return raw output
        return {
          content: [
            {
              type: 'text' as const,
              text: `Build failed (exit code ${result.exitCode}):\n\n${combined}`,
            },
          ],
          isError: true,
        };
      }

      const errors = diagnostics.filter((d) => d.severity === 'error');
      const warnings = diagnostics.filter((d) => d.severity === 'warning');

      const ctxLines = context_lines ?? 3;

      // Cache source files to avoid re-reads
      const fileCache = new Map<string, string[] | null>();
      async function getFileLines(filePath: string): Promise<string[] | null> {
        if (fileCache.has(filePath)) return fileCache.get(filePath)!;
        try {
          const content = await readFile(filePath, 'utf-8');
          const lines = content.split('\n');
          fileCache.set(filePath, lines);
          return lines;
        } catch {
          fileCache.set(filePath, null);
          return null;
        }
      }

      const sections: string[] = [
        `Build failed: ${errors.length} error(s), ${warnings.length} warning(s)`,
        '',
      ];

      for (const d of diagnostics) {
        const loc = d.line
          ? `${d.file}:${d.line}${d.column ? ':' + d.column : ''}`
          : d.file;
        sections.push(`  [${d.severity.toUpperCase()}] ${loc} \u2014 ${d.message}`);

        // Add source context if available
        if (ctxLines > 0 && d.line !== null && d.file) {
          const resolvedPath = d.file.startsWith('/')
            ? d.file
            : join(project_path, d.file);
          const sourceLines = await getFileLines(resolvedPath);
          if (sourceLines) {
            const startLine = Math.max(0, d.line - 1 - ctxLines);
            const endLine = Math.min(sourceLines.length, d.line + ctxLines);
            for (let li = startLine; li < endLine; li++) {
              const lineNum = li + 1;
              const marker = lineNum === d.line ? '>' : ' ';
              const numStr = String(lineNum).padStart(5);
              sections.push(`  ${marker}${numStr} | ${sourceLines[li]}`);
            }
          }
        }
      }

      if (makefileNote) {
        sections.push('');
        sections.push(makefileNote);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: true,
      };
    },
  );
}

/**
 * Detect a Makefile in the project directory and extract build-related targets.
 * Returns an advisory note string, or null if no Makefile found.
 */
async function detectMakefile(projectPath: string): Promise<string | null> {
  try {
    const content = await readFile(join(projectPath, 'Makefile'), 'utf-8');
    const targets = parseMakeTargets(content);
    if (targets.length === 0) return null;
    return `Note: This project has a Makefile with targets: ${targets.join(', ')}. Use gerbil_make to run them.`;
  } catch {
    return null;
  }
}

const BUILD_TARGETS = new Set([
  'all', 'build', 'clean', 'install', 'uninstall',
  'release', 'test', 'check', 'dist', 'deploy',
]);

function parseMakeTargets(content: string): string[] {
  const targets: string[] = [];
  const seen = new Set<string>();
  for (const line of content.split('\n')) {
    const match = line.match(/^([a-zA-Z_][\w.-]*)\s*:/);
    if (match && !seen.has(match[1])) {
      seen.add(match[1]);
      targets.push(match[1]);
    }
  }
  // Only return if at least one is a "build-related" target
  const hasBuildTarget = targets.some((t) => BUILD_TARGETS.has(t));
  return hasBuildTarget ? targets : [];
}
