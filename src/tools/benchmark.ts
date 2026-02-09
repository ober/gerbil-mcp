import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-BENCH:';

export function registerBenchmarkTool(server: McpServer): void {
  server.registerTool(
    'gerbil_benchmark',
    {
      title: 'Benchmark Expression',
      description:
        'Time a Gerbil Scheme expression\'s execution and return performance statistics. ' +
        'Reports wall-clock time, CPU time (user/system), GC time, GC count, ' +
        'and bytes allocated. Supports multiple iterations for averaging.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        expression: z
          .string()
          .describe('The Gerbil Scheme expression to benchmark'),
        imports: z
          .array(z.string())
          .optional()
          .describe(
            'Module paths to import before evaluation (e.g. [":std/text/json", ":std/iter"])',
          ),
        iterations: z
          .number()
          .optional()
          .describe(
            'Number of times to run the expression (default: 1). Results are totaled when > 1.',
          ),
      },
    },
    async ({ expression, imports, iterations }) => {
      const escaped = escapeSchemeString(expression);
      const n = iterations ?? 1;

      const exprs: string[] = [];
      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      exprs.push(buildBenchExpr(escaped, n));

      const result = await runGxi(exprs);

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Benchmark timed out after 30 seconds.',
            },
          ],
          isError: true,
        };
      }

      if (result.exitCode !== 0 && result.stderr) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Benchmark error:\n${result.stderr.trim()}`,
            },
          ],
          isError: true,
        };
      }

      const stdout = result.stdout;
      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        return {
          content: [
            {
              type: 'text' as const,
              text: `Benchmark error:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse result lines
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      const info: Record<string, string> = {};
      for (const line of lines) {
        const payload = line.slice(RESULT_MARKER.length);
        const tabIdx = payload.indexOf('\t');
        if (tabIdx !== -1) {
          info[payload.slice(0, tabIdx)] = payload.slice(tabIdx + 1).trim();
        }
      }

      if (Object.keys(info).length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'No benchmark results collected.',
            },
          ],
          isError: true,
        };
      }

      // Format output
      const wall = parseFloat(info['wall-time'] || '0');
      const user = parseFloat(info['user-time'] || '0');
      const sys = parseFloat(info['sys-time'] || '0');
      const gc = parseFloat(info['gc-time'] || '0');
      const gcCount = info['gc-count'] || '0';
      const bytes = parseFloat(info['bytes-alloc'] || '0');
      const iters = parseInt(info['iterations'] || '1', 10);

      const sections: string[] = [`Benchmark: ${expression}`, ''];

      if (iters > 1) {
        sections.push(`Iterations: ${iters}`);
        sections.push(`Total wall time: ${formatTime(wall)}`);
        sections.push(`Avg wall time: ${formatTime(wall / iters)}`);
      } else {
        sections.push(`Wall time: ${formatTime(wall)}`);
      }

      sections.push(`User time: ${formatTime(user)}`);
      sections.push(`System time: ${formatTime(sys)}`);
      sections.push(`GC time: ${formatTime(gc)}`);
      sections.push(`GC count: ${gcCount}`);
      sections.push(`Bytes allocated: ${formatBytes(bytes)}`);

      if (info['result']) {
        sections.push('');
        sections.push(`Result: ${info['result']}`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

function formatTime(seconds: number): string {
  if (seconds < 0.001) return `${(seconds * 1_000_000).toFixed(1)}us`;
  if (seconds < 1) return `${(seconds * 1000).toFixed(2)}ms`;
  return `${seconds.toFixed(3)}s`;
}

function formatBytes(b: number): string {
  if (b < 1024) return `${Math.round(b)} B`;
  if (b < 1024 * 1024) return `${(b / 1024).toFixed(1)} KB`;
  return `${(b / (1024 * 1024)).toFixed(1)} MB`;
}

function buildBenchExpr(escaped: string, iterations: number): string {
  return [
    '(with-catch',
    '  (lambda (e)',
    `    (display "${ERROR_MARKER}\\n")`,
    '    (display-exception e (current-output-port)))',
    '  (lambda ()',
    `    (let* ((expr-thunk (lambda () (eval (read (open-input-string "${escaped}")))))`,
    `           (n ${iterations})`,
    '           (s0 (##process-statistics))',
    '           (t0 (##current-time-point)))',
    '      (let loop ((i 0) (last-result (void)))',
    '        (if (< i n)',
    '          (loop (+ i 1) (expr-thunk))',
    '          (let* ((t1 (##current-time-point))',
    '                 (s1 (##process-statistics)))',
    `            (display "${RESULT_MARKER}wall-time\\t")`,
    '            (display (- t1 t0))',
    '            (newline)',
    `            (display "${RESULT_MARKER}user-time\\t")`,
    '            (display (- (f64vector-ref s1 0) (f64vector-ref s0 0)))',
    '            (newline)',
    `            (display "${RESULT_MARKER}sys-time\\t")`,
    '            (display (- (f64vector-ref s1 1) (f64vector-ref s0 1)))',
    '            (newline)',
    `            (display "${RESULT_MARKER}gc-time\\t")`,
    '            (display (- (f64vector-ref s1 5) (f64vector-ref s0 5)))',
    '            (newline)',
    `            (display "${RESULT_MARKER}gc-count\\t")`,
    '            (display (inexact->exact (- (f64vector-ref s1 6) (f64vector-ref s0 6))))',
    '            (newline)',
    `            (display "${RESULT_MARKER}bytes-alloc\\t")`,
    '            (display (inexact->exact (round (- (f64vector-ref s1 7) (f64vector-ref s0 7)))))',
    '            (newline)',
    `            (display "${RESULT_MARKER}iterations\\t")`,
    '            (display n)',
    '            (newline)',
    '            (unless (void? last-result)',
    `              (display "${RESULT_MARKER}result\\t")`,
    '              (write last-result)',
    '              (newline))))))))',
  ].join(' ');
}
