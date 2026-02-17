import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { execFile } from 'node:child_process';
import { join, dirname, basename } from 'node:path';
import { z } from 'zod';
import { runGerbilCmd, buildLoadpathEnv } from '../gxi.js';

/**
 * Run patchelf to set rpath on a binary.
 */
function runPatchelf(
  binaryPath: string,
  rpath: string,
): Promise<{ stdout: string; stderr: string; exitCode: number }> {
  return new Promise((resolve) => {
    execFile(
      'patchelf',
      ['--set-rpath', rpath, binaryPath],
      { timeout: 30_000 },
      (error, stdout, stderr) => {
        if (error) {
          resolve({
            stdout: stdout ?? '',
            stderr: stderr ?? '',
            exitCode: typeof error.code === 'number' ? error.code : 2,
          });
        } else {
          resolve({ stdout: stdout ?? '', stderr: stderr ?? '', exitCode: 0 });
        }
      },
    );
  });
}

/**
 * Run a binary with environment variables.
 */
function runBinary(
  binaryPath: string,
  args: string[],
  env: Record<string, string>,
  timeout: number,
): Promise<{ stdout: string; stderr: string; exitCode: number }> {
  return new Promise((resolve) => {
    execFile(
      binaryPath,
      args,
      {
        timeout,
        maxBuffer: 1024 * 1024,
        env: { ...process.env, ...env },
      },
      (error, stdout, stderr) => {
        if (error) {
          const timedOut = error.killed === true;
          resolve({
            stdout: stdout ?? '',
            stderr: timedOut
              ? `Process timed out after ${timeout}ms\n${stderr ?? ''}`
              : stderr ?? '',
            exitCode: typeof error.code === 'number' ? error.code : (timedOut ? 124 : 2),
          });
        } else {
          resolve({ stdout: stdout ?? '', stderr: stderr ?? '', exitCode: 0 });
        }
      },
    );
  });
}

/**
 * Find the compiled binary path for an exe target.
 */
function findBinary(projectPath: string, binName: string): string | null {
  // Check .gerbil/bin/ first, then common locations
  const candidates = [
    join(projectPath, '.gerbil', 'bin', binName),
    join(projectPath, binName),
  ];
  for (const candidate of candidates) {
    if (existsSync(candidate)) return candidate;
  }
  return null;
}

/**
 * Extract rpath entries from build.ss or Makefile.
 */
async function extractRpathHints(projectPath: string): Promise<string[]> {
  const rpaths: string[] = [];

  // Check Makefile for PATCHELF or RPATH hints
  try {
    const makefile = await readFile(join(projectPath, 'Makefile'), 'utf-8');
    const rpathPattern = /--set-rpath\s+['"]?([^'")\s]+)/g;
    let match;
    while ((match = rpathPattern.exec(makefile)) !== null) {
      rpaths.push(match[1]);
    }
    // Also check LD_LIBRARY_PATH entries
    const ldPattern = /LD_LIBRARY_PATH\s*[:?]?=\s*(.+)/g;
    while ((match = ldPattern.exec(makefile)) !== null) {
      const raw = match[1].trim();
      for (const p of raw.split(':')) {
        const trimmed = p.trim()
          .replace(/\$\(HOME\)/g, process.env.HOME || '~')
          .replace(/\$HOME/g, process.env.HOME || '~')
          .replace(/\$\(PWD\)/g, projectPath)
          .replace(/\\\n/g, '');
        if (trimmed && !trimmed.startsWith('#') && !trimmed.startsWith('$')) {
          rpaths.push(trimmed);
        }
      }
    }
  } catch {
    // No Makefile
  }

  return [...new Set(rpaths)];
}

export function registerQtTestRunnerTool(server: McpServer): void {
  server.registerTool(
    'gerbil_qt_test_runner',
    {
      title: 'Qt FFI Test Runner',
      description:
        'Build and run a Qt FFI test executable in one step. Handles the full workflow: ' +
        '(1) build the project with `gerbil build`, (2) apply patchelf for rpath if needed, ' +
        '(3) run the binary with QT_QPA_PLATFORM=offscreen. Eliminates the manual multi-step ' +
        'process of building, patching rpath, and running Qt tests headless.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        project_path: z
          .string()
          .describe('Path to the Gerbil project directory'),
        bin_name: z
          .string()
          .describe('Name of the exe binary to build and run (as specified in build.ss bin:)'),
        rpath: z
          .string()
          .optional()
          .describe('Custom rpath to set via patchelf (auto-detected from Makefile if omitted)'),
        skip_patchelf: z
          .boolean()
          .optional()
          .describe('Skip the patchelf step (default: false)'),
        skip_build: z
          .boolean()
          .optional()
          .describe('Skip the build step, just run the binary (default: false)'),
        args: z
          .array(z.string())
          .optional()
          .describe('Additional arguments to pass to the test binary'),
        env: z
          .record(z.string())
          .optional()
          .describe('Additional environment variables for the test binary'),
        timeout: z
          .number()
          .optional()
          .describe('Timeout in milliseconds for the test run (default: 60000)'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Additional GERBIL_LOADPATH entries for the build'),
      },
    },
    async ({ project_path, bin_name, rpath, skip_patchelf, skip_build, args, env, timeout, loadpath }) => {
      const sections: string[] = [];
      const runTimeout = timeout ?? 60_000;

      // Step 1: Build
      if (!skip_build) {
        sections.push('Step 1: Building project...');

        const buildEnv: Record<string, string> = {};
        if (loadpath) {
          Object.assign(buildEnv, buildLoadpathEnv(loadpath));
        }

        let buildResult;
        if (existsSync(join(project_path, 'Makefile'))) {
          buildResult = await new Promise<{ stdout: string; stderr: string; exitCode: number }>((resolve) => {
            execFile(
              'make',
              ['build'],
              { timeout: 120_000, maxBuffer: 1024 * 1024, cwd: project_path, env: { ...process.env, ...buildEnv } },
              (error, stdout, stderr) => {
                if (error) {
                  resolve({ stdout: stdout ?? '', stderr: stderr ?? '', exitCode: typeof error.code === 'number' ? error.code : 2 });
                } else {
                  resolve({ stdout: stdout ?? '', stderr: stderr ?? '', exitCode: 0 });
                }
              },
            );
          });
        } else {
          buildResult = await runGerbilCmd(['build'], {
            cwd: project_path,
            timeout: 120_000,
            env: buildEnv,
          });
        }

        if (buildResult.exitCode !== 0) {
          const errOutput = [buildResult.stdout, buildResult.stderr].filter(Boolean).join('\n').trim();
          sections.push('  BUILD FAILED');
          if (errOutput) {
            sections.push(errOutput.split('\n').slice(0, 15).join('\n'));
          }
          return {
            content: [{ type: 'text' as const, text: sections.join('\n') }],
            isError: true,
          };
        }
        sections.push('  Build succeeded.');
        sections.push('');
      } else {
        sections.push('Step 1: Build skipped.');
        sections.push('');
      }

      // Find the binary
      const binaryPath = findBinary(project_path, bin_name);
      if (!binaryPath) {
        sections.push(`Binary "${bin_name}" not found.`);
        sections.push(`Looked in: ${join(project_path, '.gerbil', 'bin', bin_name)}, ${join(project_path, bin_name)}`);
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
          isError: true,
        };
      }
      sections.push(`Binary found: ${binaryPath}`);

      // Step 2: Patchelf
      if (!skip_patchelf) {
        const rpathValue = rpath || (await extractRpathHints(project_path)).join(':');
        if (rpathValue) {
          sections.push(`Step 2: Applying patchelf --set-rpath "${rpathValue}"...`);
          const patchResult = await runPatchelf(binaryPath, rpathValue);
          if (patchResult.exitCode !== 0) {
            sections.push(`  patchelf failed: ${patchResult.stderr.trim()}`);
            sections.push('  Continuing anyway — binary may still work if rpath is not needed.');
          } else {
            sections.push('  patchelf succeeded.');
          }
        } else {
          sections.push('Step 2: No rpath detected, skipping patchelf.');
        }
        sections.push('');
      } else {
        sections.push('Step 2: Patchelf skipped.');
        sections.push('');
      }

      // Step 3: Run with offscreen
      sections.push('Step 3: Running test binary...');
      const runEnv: Record<string, string> = {
        QT_QPA_PLATFORM: 'offscreen',
        ...(env || {}),
      };

      const runResult = await runBinary(binaryPath, args || [], runEnv, runTimeout);

      if (runResult.stdout.trim()) {
        sections.push('');
        sections.push('--- stdout ---');
        sections.push(runResult.stdout.trim());
      }
      if (runResult.stderr.trim()) {
        sections.push('');
        sections.push('--- stderr ---');
        sections.push(runResult.stderr.trim());
      }

      sections.push('');
      sections.push(`Exit code: ${runResult.exitCode}`);

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: runResult.exitCode !== 0,
      };
    },
  );
}
