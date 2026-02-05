import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join } from 'node:path';
import { runGxiFile, runGerbilCmd, buildLoadpathEnv } from '../gxi.js';

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
      },
    },
    async ({ file_path, directory, filter, quiet, timeout, loadpath, project_path }) => {
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

      // Build effective loadpath from loadpath array and project_path
      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }

      return await runSingleFileTest(file_path!, timeout, effectiveLoadpath);
    },
  );
}

async function runSingleFileTest(filePath: string, timeout?: number, loadpath?: string[]) {
  const effectiveTimeout = timeout ?? 30_000;
  const env = loadpath && loadpath.length > 0 ? buildLoadpathEnv(loadpath) : undefined;
  const testResult = await runGxiFile(filePath, { timeout: effectiveTimeout, env });

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
