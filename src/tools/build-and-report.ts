import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, stat, access, constants } from 'node:fs/promises';
import { execFile } from 'node:child_process';
import { join } from 'node:path';
import { homedir } from 'node:os';
import { runGerbilCmd, buildLoadpathEnv } from '../gxi.js';
import { parseGxcErrors, type Diagnostic } from './parse-utils.js';

export function registerBuildAndReportTool(server: McpServer): void {
  server.registerTool(
    'gerbil_build_and_report',
    {
      title: 'Build and Report',
      description:
        'Run `gerbil build` on a project directory and return structured diagnostics. ' +
        'On success, reports a summary. On failure, if a Makefile with a build target ' +
        'is detected, automatically retries with `make`. Parses compiler errors into ' +
        'structured file:line:column diagnostics. ' +
        'Uses the modern `gerbil` CLI (not gxpkg). ' +
        'Auto-detects external dependencies from gerbil.pkg depend: entries and ' +
        'adds ~/.gerbil/lib to GERBIL_LOADPATH automatically when loadpath is not explicitly provided.',
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
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution',
          ),
      },
    },
    async ({ project_path, flags, context_lines, loadpath }) => {
      // Detect Makefile and extract targets
      const makefileTargets = await detectMakefileTargets(project_path);
      const makefileNote = makefileTargets.length > 0
        ? `Note: This project has a Makefile with targets: ${makefileTargets.join(', ')}. Use gerbil_make to run them.`
        : null;

      // Auto-detect loadpath from gerbil.pkg depend: entries when not explicitly provided
      const effectiveLoadpath = loadpath ?? await autoDetectLoadpath(project_path);

      const args = ['build', ...(flags ?? [])];
      const loadpathEnv = effectiveLoadpath.length > 0 ? buildLoadpathEnv(effectiveLoadpath) : undefined;

      const result = await runGerbilCmd(args, {
        cwd: project_path,
        timeout: 120_000,
        env: loadpathEnv,
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

      // Check for non-executable build.ss before other failure handling
      const buildSsPermissionHint = await checkBuildSsPermissions(project_path, result);
      if (buildSsPermissionHint) {
        return {
          content: [{ type: 'text' as const, text: buildSsPermissionHint }],
          isError: true,
        };
      }

      // Failure path — try Makefile fallback before reporting errors
      const makeTarget = pickMakeBuildTarget(makefileTargets);
      if (makeTarget) {
        const makeResult = await runMake(
          makeTarget === '(default)' ? [] : [makeTarget],
          project_path,
          120_000,
        );

        if (!makeResult.timedOut && makeResult.exitCode === 0) {
          const output = [makeResult.stdout, makeResult.stderr]
            .filter(Boolean)
            .join('\n')
            .trim();

          const sections: string[] = [
            'Build succeeded via Makefile fallback (gerbil build failed, make succeeded).',
          ];
          if (output) {
            sections.push('');
            sections.push(output);
          }
          return {
            content: [{ type: 'text' as const, text: sections.join('\n') }],
          };
        }
      }

      // Both gerbil build and make failed (or no Makefile) — report gerbil build errors
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
 * Returns the list of targets, or empty array if no Makefile found.
 */
async function detectMakefileTargets(projectPath: string): Promise<string[]> {
  try {
    const content = await readFile(join(projectPath, 'Makefile'), 'utf-8');
    return parseMakeTargets(content);
  } catch {
    return [];
  }
}

/**
 * Pick the best make target for a build fallback.
 * Returns 'build', 'all', '(default)' (for make with no target), or null if no suitable target.
 */
function pickMakeBuildTarget(targets: string[]): string | null {
  if (targets.includes('build')) return 'build';
  if (targets.includes('all')) return 'all';
  // If there are any build-related targets, run make with no target (uses default)
  if (targets.length > 0) return '(default)';
  return null;
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

/**
 * Auto-detect loadpath from gerbil.pkg depend: entries.
 * If the project has external dependencies, add ~/.gerbil/lib to loadpath.
 */
async function autoDetectLoadpath(projectPath: string): Promise<string[]> {
  try {
    const content = await readFile(join(projectPath, 'gerbil.pkg'), 'utf-8');
    // Look for depend: followed by a list of package names
    if (/\bdepend:/.test(content)) {
      const gerbilLib = join(
        process.env.GERBIL_PATH ?? join(homedir(), '.gerbil'),
        'lib',
      );
      return [gerbilLib];
    }
  } catch {
    // No gerbil.pkg or can't read it — no auto-detection
  }
  return [];
}

/**
 * Check if build.ss exists but lacks the executable bit when a build fails
 * with "Permission denied". Returns a helpful diagnostic message, or null.
 */
async function checkBuildSsPermissions(
  projectPath: string,
  result: { stdout: string; stderr: string },
): Promise<string | null> {
  const combined = [result.stdout, result.stderr].join('\n');
  if (!/permission denied/i.test(combined)) return null;

  const buildSsPath = join(projectPath, 'build.ss');
  try {
    await stat(buildSsPath);
  } catch {
    return null; // build.ss doesn't exist — not a permission issue
  }

  try {
    await access(buildSsPath, constants.X_OK);
    return null; // build.ss is executable — permission denied is from something else
  } catch {
    return (
      `Build failed: build.ss is not executable.\n\n` +
      `Fix with: chmod +x ${buildSsPath}\n\n` +
      `The file exists but lacks the executable bit, which is required by \`gerbil build\`.`
    );
  }
}

const MAKE_MAX_BUFFER = 1024 * 1024;

function runMake(
  args: string[],
  cwd: string,
  timeout: number,
): Promise<{ stdout: string; stderr: string; exitCode: number; timedOut: boolean }> {
  return new Promise((resolve) => {
    execFile(
      'make',
      args,
      { timeout, maxBuffer: MAKE_MAX_BUFFER, cwd },
      (error, stdout, stderr) => {
        if (error) {
          const timedOut = error.killed === true;
          const code = (error as NodeJS.ErrnoException).code;
          const exitCode =
            typeof error.code === 'number'
              ? error.code
              : code === 'ENOENT'
                ? 127
                : 2;
          resolve({
            stdout: stdout ?? '',
            stderr: stderr ?? '',
            exitCode,
            timedOut,
          });
        } else {
          resolve({
            stdout: stdout ?? '',
            stderr: stderr ?? '',
            exitCode: 0,
            timedOut: false,
          });
        }
      },
    );
  });
}
