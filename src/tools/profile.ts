import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-PROF:';

export function registerProfileTool(server: McpServer): void {
  server.registerTool(
    'gerbil_profile',
    {
      title: 'Profile Function Performance',
      description:
        'Instrument specific functions with call counting and timing while running an expression. ' +
        'Reports per-function call count, cumulative time, average time, and percentage of wall time. ' +
        'Also reports overall wall time, CPU time, GC time, and bytes allocated. ' +
        'Instruments top-level bindings via set!; does not work on lexical bindings.',
      inputSchema: {
        expression: z
          .string()
          .describe('The Gerbil Scheme expression to profile'),
        functions: z
          .array(z.string())
          .describe(
            'Function names to instrument with call counting and timing (e.g. ["read-json", "hash-put!"])',
          ),
        imports: z
          .array(z.string())
          .optional()
          .describe(
            'Module paths to import before evaluation (e.g. [":std/text/json"])',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution',
          ),
      },
    },
    async ({ expression, functions, imports, loadpath }) => {
      const escaped = escapeSchemeString(expression);

      const exprs: string[] = [];
      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      exprs.push(buildProfileExpr(escaped, functions));

      const env = loadpath && loadpath.length > 0
        ? buildLoadpathEnv(loadpath)
        : undefined;

      const result = await runGxi(exprs, { timeout: 120_000, env });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Profile timed out after 120 seconds.',
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
              text: `Profile error:\n${result.stderr.trim()}`,
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
              text: `Profile error:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse result lines
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      const warnings: string[] = [];
      const fnStats: Array<{
        name: string;
        calls: number;
        time: number;
      }> = [];
      const overall: Record<string, string> = {};
      let resultValue: string | undefined;

      for (const line of lines) {
        const payload = line.slice(RESULT_MARKER.length);
        const tabIdx = payload.indexOf('\t');
        if (tabIdx === -1) continue;
        const key = payload.slice(0, tabIdx);
        const val = payload.slice(tabIdx + 1).trim();

        if (key === '__warn') {
          warnings.push(val);
        } else if (key === '__result') {
          resultValue = val;
        } else if (key.startsWith('__')) {
          overall[key] = val;
        } else {
          // Function stat: "name\tcalls\ttime"
          const parts = val.split('\t');
          if (parts.length >= 2) {
            fnStats.push({
              name: key,
              calls: parseInt(parts[0], 10),
              time: parseFloat(parts[1]),
            });
          }
        }
      }

      // Sort by time descending
      fnStats.sort((a, b) => b.time - a.time);

      const wallTime = parseFloat(overall['__wall'] || '0');
      const userTime = parseFloat(overall['__user'] || '0');
      const sysTime = parseFloat(overall['__sys'] || '0');
      const gcTime = parseFloat(overall['__gc'] || '0');
      const gcCount = overall['__gc-count'] || '0';
      const bytesAlloc = parseFloat(overall['__alloc'] || '0');

      // Format output
      const sections: string[] = [`Profile: ${expression}`, ''];

      sections.push(
        `Wall time: ${formatTime(wallTime)} | User: ${formatTime(userTime)} | ` +
          `Sys: ${formatTime(sysTime)} | GC: ${formatTime(gcTime)} (${gcCount} collections) | ` +
          `Alloc: ${formatBytes(bytesAlloc)}`,
      );
      sections.push('');

      if (warnings.length > 0) {
        for (const w of warnings) {
          sections.push(`Warning: ${w}`);
        }
        sections.push('');
      }

      if (fnStats.length > 0) {
        const maxNameLen = Math.max(
          8,
          ...fnStats.map((f) => f.name.length),
        );
        sections.push(
          `${'Function'.padEnd(maxNameLen)}  ${'Calls'.padStart(12)}  ${'Time'.padStart(10)}  ${'Avg'.padStart(10)}  ${'%'.padStart(6)}`,
        );
        for (const f of fnStats) {
          const pct =
            wallTime > 0 ? ((f.time / wallTime) * 100).toFixed(1) : '0.0';
          const avg = f.calls > 0 ? f.time / f.calls : 0;
          sections.push(
            `${f.name.padEnd(maxNameLen)}  ${f.calls.toString().padStart(12)}  ${formatTime(f.time).padStart(10)}  ${formatTime(avg).padStart(10)}  ${(pct + '%').padStart(6)}`,
          );
        }
      } else if (functions.length === 0) {
        sections.push('No functions specified for profiling.');
      } else {
        sections.push('No instrumented functions were called.');
      }

      if (resultValue !== undefined) {
        sections.push('');
        sections.push(`Result: ${resultValue}`);
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

function safeName(fn: string): string {
  return fn.replace(/[^a-zA-Z0-9]/g, '_');
}

function buildProfileExpr(
  escapedExpr: string,
  functions: string[],
): string {
  const parts: string[] = [];

  parts.push('(with-catch');
  parts.push('  (lambda (e)');
  parts.push(`    (display "${ERROR_MARKER}\\n")`);
  parts.push('    (display-exception e (current-output-port)))');
  parts.push('  (lambda ()');
  parts.push('    (begin');

  // Instrument each function with counting + timing
  for (const fn of functions) {
    const safeFn = escapeSchemeString(fn);
    const safe = safeName(fn);
    parts.push(`      (with-catch`);
    parts.push(`        (lambda (e)`);
    parts.push(
      `          (display "${RESULT_MARKER}__warn\\t${safeFn} not bound or not a procedure\\n"))`,
    );
    parts.push(`        (lambda ()`);
    parts.push(`          (let ((v (eval (quote ${fn}))))`);
    parts.push(`            (when (procedure? v)`);
    parts.push(
      `              (eval (quote (begin` +
        ` (def __pf_${safe}_count 0)` +
        ` (def __pf_${safe}_time 0.0)` +
        ` (def __pf_${safe}_orig ${fn})` +
        ` (set! ${fn} (lambda args` +
        ` (set! __pf_${safe}_count (+ __pf_${safe}_count 1))` +
        ` (let ((t0 (##current-time-point)))` +
        ` (let ((r (apply __pf_${safe}_orig args)))` +
        ` (set! __pf_${safe}_time (+ __pf_${safe}_time (- (##current-time-point) t0)))` +
        ` r)))))))))))`,
    );
  }

  // Capture overall stats and run expression
  parts.push(
    `      (let* ((s0 (##process-statistics))`,
  );
  parts.push(`             (t0 (##current-time-point))`);
  parts.push(
    `             (result (eval (read (open-input-string "${escapedExpr}"))))`,
  );
  parts.push(`             (t1 (##current-time-point))`);
  parts.push(`             (s1 (##process-statistics)))`);

  // Output overall stats
  parts.push(`        (display "${RESULT_MARKER}__wall\\t")`);
  parts.push(`        (display (- t1 t0)) (newline)`);
  parts.push(`        (display "${RESULT_MARKER}__user\\t")`);
  parts.push(
    `        (display (- (f64vector-ref s1 0) (f64vector-ref s0 0))) (newline)`,
  );
  parts.push(`        (display "${RESULT_MARKER}__sys\\t")`);
  parts.push(
    `        (display (- (f64vector-ref s1 1) (f64vector-ref s0 1))) (newline)`,
  );
  parts.push(`        (display "${RESULT_MARKER}__gc\\t")`);
  parts.push(
    `        (display (- (f64vector-ref s1 5) (f64vector-ref s0 5))) (newline)`,
  );
  parts.push(`        (display "${RESULT_MARKER}__gc-count\\t")`);
  parts.push(
    `        (display (inexact->exact (- (f64vector-ref s1 6) (f64vector-ref s0 6)))) (newline)`,
  );
  parts.push(`        (display "${RESULT_MARKER}__alloc\\t")`);
  parts.push(
    `        (display (inexact->exact (round (- (f64vector-ref s1 7) (f64vector-ref s0 7))))) (newline)`,
  );

  // Output per-function stats: "fnname\tcalls\ttime"
  for (const fn of functions) {
    const safeFn = escapeSchemeString(fn);
    const safe = safeName(fn);
    parts.push(`        (with-catch (lambda (e) (void))`);
    parts.push(`          (lambda ()`);
    parts.push(
      `            (let ((count (eval (quote __pf_${safe}_count)))`,
    );
    parts.push(
      `                  (time (eval (quote __pf_${safe}_time))))`,
    );
    parts.push(
      `              (display "${RESULT_MARKER}${safeFn}\\t")`,
    );
    parts.push(`              (display count) (display "\\t")`);
    parts.push(`              (display time)`);
    parts.push(`              (newline))))`);
  }

  // Output result
  parts.push(`        (unless (void? result)`);
  parts.push(`          (display "${RESULT_MARKER}__result\\t")`);
  parts.push(`          (write result)`);
  parts.push(`          (newline))`);
  parts.push('      ))))');

  return parts.join(' ');
}
