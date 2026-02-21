import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join, dirname, basename } from 'node:path';
import { readFile, readdir, stat, rm, writeFile } from 'node:fs/promises';
import { homedir, tmpdir } from 'node:os';
import { runGxiFile, runGerbilCmd, buildLoadpathEnv } from '../gxi.js';

/**
 * Auto-detect loadpath from gerbil.pkg depend: entries.
 * If the project has external dependencies, add ~/.gerbil/lib to loadpath.
 * Same logic as build-and-report.ts autoDetectLoadpath.
 */
async function autoDetectLoadpath(projectPath: string): Promise<string[]> {
  try {
    const content = await readFile(join(projectPath, 'gerbil.pkg'), 'utf-8');
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
 * Clean stale compiled artifacts that could shadow source files.
 * Removes compiled modules from .gerbil/lib/ and ~/.gerbil/lib/ matching
 * source files in the given directory.
 */
async function cleanStaleArtifacts(testDir: string, projectPath?: string): Promise<string[]> {
  const cleaned: string[] = [];
  const baseDir = projectPath || testDir;

  // Find .ss source files to determine which compiled artifacts to check
  const sourceModules: string[] = [];
  try {
    const entries = await readdir(baseDir);
    for (const entry of entries) {
      if (entry.endsWith('.ss') && !entry.endsWith('-test.ss')) {
        sourceModules.push(entry.replace(/\.ss$/, ''));
      }
    }
  } catch { /* ignore */ }

  // Check .gerbil/lib/ and ~/.gerbil/lib/ for stale artifacts
  const artifactDirs = [
    join(baseDir, '.gerbil', 'lib'),
    join(process.env.GERBIL_PATH ?? join(homedir(), '.gerbil'), 'lib'),
  ];

  for (const artDir of artifactDirs) {
    for (const mod of sourceModules) {
      // Check for compiled artifacts: mod.ssi, mod.scm, mod/ directory
      for (const ext of ['.ssi', '.scm', '']) {
        const artPath = join(artDir, mod + ext);
        try {
          const s = await stat(artPath);
          if (s.isFile() || s.isDirectory()) {
            await rm(artPath, { recursive: true, force: true });
            cleaned.push(artPath);
          }
        } catch { /* doesn't exist */ }
      }
    }
  }

  return cleaned;
}

export function registerRunTestsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_run_tests',
    {
      title: 'Run Gerbil Tests',
      description:
        'Run a Gerbil test file that uses :std/test and return structured results. ' +
        'The file should define test suites with test-suite/test-case/check-equal? etc., ' +
        'call (run-tests!) and (test-report-summary!). ' +
        'Returns pass/fail counts and failure details. ' +
        'Use file_path for a single test file, or directory for project-wide tests via "gerbil test". ' +
        'Use filter to match test names by pattern, quiet for errors-only output.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe(
            'Path to a .ss test file that uses :std/test (single-file mode)',
          ),
        directory: z
          .string()
          .optional()
          .describe(
            'Project directory to run tests in using "gerbil test" (project-wide mode). ' +
            'Runs "gerbil test <dir>/..." recursively. Cannot be used with file_path.',
          ),
        filter: z
          .string()
          .optional()
          .describe(
            'Regular expression pattern to filter test names (directory mode only, maps to -r flag)',
          ),
        quiet: z
          .boolean()
          .optional()
          .describe(
            'Quiet mode: only show errors (directory mode only, maps to -q flag)',
          ),
        timeout: z
          .number()
          .optional()
          .describe(
            'Timeout in milliseconds for test execution (default: 30000 for file, 120000 for directory)',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution (file_path mode only)',
          ),
        project_path: z
          .string()
          .optional()
          .describe(
            'Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib (file_path mode only)',
          ),
        env: z
          .record(z.string())
          .optional()
          .describe(
            'Environment variables to pass to the subprocess ' +
            '(e.g. {"DYLD_LIBRARY_PATH": "/usr/local/lib"})',
          ),
        clean_stale: z
          .boolean()
          .optional()
          .describe(
            'Clean stale compiled artifacts before running tests. ' +
            'Removes compiled modules from .gerbil/lib/ and ~/.gerbil/lib/ ' +
            'that could shadow source files (common cause of test failures when ' +
            'a module exports main for exe builds).',
          ),
        verbose: z
          .boolean()
          .optional()
          .describe(
            'Verbose mode: instrument test check expressions to show intermediate values ' +
            'before and after each check. Captures all stdout/stderr from the test case. ' +
            'Only works in single-file mode (file_path). ' +
            'Useful for debugging failing tests without adding manual displayln statements.',
          ),
      },
    },
    async ({ file_path, directory, filter, quiet, timeout, loadpath, project_path, env: extraEnv, clean_stale, verbose }) => {
      // Validate: exactly one of file_path or directory
      if (file_path && directory) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Cannot specify both file_path and directory. Use file_path for a single test file, or directory for project-wide tests.',
            },
          ],
          isError: true,
        };
      }

      if (!file_path && !directory) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Either file_path or directory is required.',
            },
          ],
          isError: true,
        };
      }

      if (directory) {
        return await runDirectoryTests(directory, { filter, quiet, timeout });
      }

      // Clean stale artifacts if requested
      const staleWarnings: string[] = [];
      if (clean_stale && file_path) {
        const testDir = dirname(file_path);
        const cleaned = await cleanStaleArtifacts(testDir, project_path);
        if (cleaned.length > 0) {
          staleWarnings.push(`Cleaned ${cleaned.length} stale artifact(s): ${cleaned.join(', ')}`);
        }
      }

      // Build effective loadpath from loadpath array and project_path
      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
        // Auto-detect loadpath from gerbil.pkg depend: when not explicitly provided
        if (!loadpath || loadpath.length === 0) {
          const autoPath = await autoDetectLoadpath(project_path);
          effectiveLoadpath.push(...autoPath);
        }
      }

      const result = verbose
        ? await runVerboseTest(file_path!, timeout, effectiveLoadpath, extraEnv)
        : await runSingleFileTest(file_path!, timeout, effectiveLoadpath, extraEnv);
      if (staleWarnings.length > 0 && result.content?.[0]) {
        const existing = (result.content[0] as { text: string }).text;
        (result.content[0] as { text: string }).text = staleWarnings.join('\n') + '\n\n' + existing;
      }
      return result;
    },
  );
}

async function runSingleFileTest(filePath: string, timeout?: number, loadpath?: string[], extraEnv?: Record<string, string>) {
  const effectiveTimeout = timeout ?? 30_000;
  const loadpathEnv = loadpath && loadpath.length > 0 ? buildLoadpathEnv(loadpath) : undefined;
  const env = { ...loadpathEnv, ...extraEnv };
  const testResult = await runGxiFile(filePath, { timeout: effectiveTimeout, env: Object.keys(env).length > 0 ? env : undefined });

  if (testResult.timedOut) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Test execution timed out after ${Math.round(effectiveTimeout / 1000)} seconds.`,
        },
      ],
      isError: true,
    };
  }

  // Combine stdout and stderr — test framework writes to both
  const fullOutput = [testResult.stdout, testResult.stderr]
    .filter(Boolean)
    .join('\n');

  if (testResult.exitCode === 127) {
    return {
      content: [
        {
          type: 'text' as const,
          text: 'gxi not found. Ensure Gerbil is installed.',
        },
      ],
      isError: true,
    };
  }

  // Parse the test output
  const parsed = parseTestOutput(fullOutput);

  // Build formatted output
  const sections: string[] = [];

  if (parsed.passed) {
    sections.push(`Result: PASSED`);
  } else {
    sections.push(`Result: FAILED`);
  }

  sections.push('');

  if (parsed.summary.length > 0) {
    sections.push('Test Summary:');
    for (const line of parsed.summary) {
      sections.push(`  ${line}`);
    }
    sections.push('');
  }

  if (parsed.failures.length > 0) {
    sections.push(`Failures (${parsed.failures.length}):`);
    for (const failure of parsed.failures) {
      sections.push(`  ${failure}`);
    }
    sections.push('');
  }

  if (parsed.checkCount > 0) {
    sections.push(`Checks: ${parsed.checkCount} total`);
  }

  // Include raw output for context
  sections.push('');
  sections.push('--- Full output ---');
  sections.push(fullOutput.trim());

  return {
    content: [
      {
        type: 'text' as const,
        text: sections.join('\n'),
      },
    ],
    isError: !parsed.passed,
  };
}

async function runDirectoryTests(
  directory: string,
  opts: { filter?: string; quiet?: boolean; timeout?: number },
) {
  const effectiveTimeout = opts.timeout ?? 120_000;

  // Build gerbil test arguments
  const args: string[] = ['test'];
  if (opts.quiet) args.push('-q');
  if (opts.filter) args.push('-r', opts.filter);
  args.push(directory + '/...');

  const testResult = await runGerbilCmd(args, {
    cwd: directory,
    timeout: effectiveTimeout,
  });

  if (testResult.timedOut) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Test execution timed out after ${Math.round(effectiveTimeout / 1000)} seconds.`,
        },
      ],
      isError: true,
    };
  }

  if (testResult.exitCode === 127) {
    return {
      content: [
        {
          type: 'text' as const,
          text: 'gerbil command not found. Ensure Gerbil is installed and in PATH.',
        },
      ],
      isError: true,
    };
  }

  // Combine stdout and stderr
  const fullOutput = [testResult.stdout, testResult.stderr]
    .filter(Boolean)
    .join('\n');

  // Parse the test output
  const parsed = parseTestOutput(fullOutput);

  // Build formatted output
  const sections: string[] = [];

  if (parsed.passed) {
    sections.push(`Result: PASSED`);
  } else {
    sections.push(`Result: FAILED`);
  }

  sections.push('');

  if (opts.filter) {
    sections.push(`Filter: ${opts.filter}`);
    sections.push('');
  }

  if (parsed.summary.length > 0) {
    sections.push('Test Summary:');
    for (const line of parsed.summary) {
      sections.push(`  ${line}`);
    }
    sections.push('');
  }

  if (parsed.failures.length > 0) {
    sections.push(`Failures (${parsed.failures.length}):`);
    for (const failure of parsed.failures) {
      sections.push(`  ${failure}`);
    }
    sections.push('');
  }

  if (parsed.checkCount > 0) {
    sections.push(`Checks: ${parsed.checkCount} total`);
  }

  // Include raw output for context
  sections.push('');
  sections.push('--- Full output ---');
  sections.push(fullOutput.trim());

  return {
    content: [
      {
        type: 'text' as const,
        text: sections.join('\n'),
      },
    ],
    isError: !parsed.passed,
  };
}

/**
 * Run a test file in verbose mode: instruments check expressions to log
 * their actual and expected values for every check (not just failures).
 *
 * Approach: read the test source, do a line-based transformation that inserts
 * displayln tracing before each (check ...) form, write to a temp file, run it.
 * Also includes the original source in the output for cross-referencing.
 */
async function runVerboseTest(
  filePath: string,
  timeout?: number,
  loadpath?: string[],
  extraEnv?: Record<string, string>,
) {
  const effectiveTimeout = timeout ?? 30_000;
  const loadpathEnv =
    loadpath && loadpath.length > 0 ? buildLoadpathEnv(loadpath) : undefined;
  const env = { ...loadpathEnv, ...extraEnv };

  // Read the original test file
  let testSource: string;
  try {
    testSource = await readFile(filePath, 'utf-8');
  } catch {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Cannot read test file: ${filePath}`,
        },
      ],
      isError: true,
    };
  }

  // Instrument the source: insert tracing lines before check expressions.
  // We detect lines containing (check ...) and add a displayln before them
  // showing the source expression being checked and its line number.
  const lines = testSource.split('\n');
  const instrumented: string[] = [];
  let hasFormat = false;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    // Check if :std/format is already imported
    if (/\bstd\/format\b/.test(line)) hasFormat = true;

    // Insert :std/format import after :std/test import if not present
    if (!hasFormat && /\bstd\/test\b/.test(line) && /\bimport\b/.test(line)) {
      instrumented.push(line);
      instrumented.push('(import :std/format)');
      hasFormat = true;
      continue;
    }

    // Detect lines containing (check ...) — add tracing before them
    const trimmed = line.trimStart();
    if (trimmed.startsWith('(check ') || trimmed.startsWith('(check-')) {
      const indent = line.substring(0, line.length - trimmed.length);
      const escapedExpr = trimmed
        .replace(/\\/g, '\\\\')
        .replace(/"/g, '\\"')
        .replace(/~~/g, '~~~~');
      instrumented.push(
        `${indent}(display (format "  [TRACE L${i + 1}] ~a~n" "${escapedExpr}") (current-error-port))`,
      );
    }

    instrumented.push(line);
  }

  // Write instrumented source to temp file
  const tmpPath = join(
    tmpdir(),
    `gerbil-verbose-test-${Date.now()}.ss`,
  );
  await writeFile(tmpPath, instrumented.join('\n'), 'utf-8');

  try {
    const testResult = await runGxiFile(tmpPath, {
      timeout: effectiveTimeout,
      env: Object.keys(env).length > 0 ? env : undefined,
    });

    if (testResult.timedOut) {
      return {
        content: [
          {
            type: 'text' as const,
            text: `Test execution timed out after ${Math.round(effectiveTimeout / 1000)} seconds.`,
          },
        ],
        isError: true,
      };
    }

    // Combine stdout and stderr for verbose output
    const fullOutput = [testResult.stdout, testResult.stderr]
      .filter(Boolean)
      .join('\n');

    const parsed = parseTestOutput(fullOutput);

    // Build verbose formatted output
    const sections: string[] = [];

    sections.push(
      `Result: ${parsed.passed ? 'PASSED' : 'FAILED'} (verbose mode)`,
    );
    sections.push('');

    if (parsed.summary.length > 0) {
      sections.push('Test Summary:');
      for (const line of parsed.summary) {
        sections.push(`  ${line}`);
      }
      sections.push('');
    }

    if (parsed.failures.length > 0) {
      sections.push(`Failures (${parsed.failures.length}):`);
      for (const failure of parsed.failures) {
        sections.push(`  ${failure}`);
      }
      sections.push('');
    }

    if (parsed.checkCount > 0) {
      sections.push(`Checks: ${parsed.checkCount} total`);
    }

    // In verbose mode, show ALL output including stderr trace lines
    sections.push('');
    sections.push('--- Full verbose output ---');
    sections.push(fullOutput.trim());

    // Include original source for cross-reference
    sections.push('');
    sections.push(`--- Source: ${filePath} ---`);
    const srcLines = testSource.split('\n');
    for (let i = 0; i < srcLines.length; i++) {
      sections.push(`${String(i + 1).padStart(4)}: ${srcLines[i]}`);
    }

    return {
      content: [
        {
          type: 'text' as const,
          text: sections.join('\n'),
        },
      ],
      isError: !parsed.passed,
    };
  } finally {
    try {
      await rm(tmpPath, { force: true });
    } catch {
      /* ignore cleanup errors */
    }
  }
}

interface TestParseResult {
  passed: boolean;
  failures: string[];
  summary: string[];
  checkCount: number;
}

function parseTestOutput(output: string): TestParseResult {
  const lines = output.split('\n');
  const failures: string[] = [];
  const summary: string[] = [];
  let checkCount = 0;
  let inSummary = false;

  for (const line of lines) {
    // Count successful checks
    const checkMatch = line.match(/\.\.\. (\d+) checks? OK/);
    if (checkMatch) {
      checkCount += parseInt(checkMatch[1], 10);
    }

    // Capture failures
    if (line.includes('*** FAILED:')) {
      failures.push(line.trim());
    }

    // Capture summary section
    if (line.includes('--- Test Summary')) {
      inSummary = true;
      continue;
    }
    if (inSummary) {
      const trimmed = line.trim();
      if (trimmed && trimmed !== 'OK' && trimmed !== 'FAILURE') {
        summary.push(trimmed);
      }
    }
  }

  // Determine pass/fail from final output line
  const trimmedOutput = output.trim();
  const passed =
    trimmedOutput.endsWith('OK') &&
    !trimmedOutput.endsWith('FAILURE') &&
    failures.length === 0;

  return { passed, failures, summary, checkCount };
}
