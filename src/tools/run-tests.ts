import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxiFile } from '../gxi.js';

export function registerRunTestsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_run_tests',
    {
      title: 'Run Gerbil Tests',
      description:
        'Run a Gerbil test file that uses :std/test and return structured results. ' +
        'The file should define test suites with test-suite/test-case/check-equal? etc., ' +
        'call (run-tests!) and (test-report-summary!). ' +
        'Returns pass/fail counts and failure details.',
      inputSchema: {
        file_path: z
          .string()
          .describe(
            'Path to a .ss test file that uses :std/test',
          ),
        timeout: z
          .number()
          .optional()
          .describe(
            'Timeout in milliseconds for test execution (default: 30000)',
          ),
      },
    },
    async ({ file_path, timeout }) => {
      const effectiveTimeout = timeout ?? 30_000;
      const testResult = await runGxiFile(file_path, { timeout: effectiveTimeout });

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
    },
  );
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
