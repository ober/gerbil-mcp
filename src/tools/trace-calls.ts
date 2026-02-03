import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-TRACE:';

export function registerTraceCallsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_trace_calls',
    {
      title: 'Trace Function Calls',
      description:
        'Count how many times specified functions are called while running an expression. ' +
        'Lightweight instrumentation with minimal overhead (no timing). ' +
        'Instruments top-level bindings via set!; does not work on lexical bindings.',
      inputSchema: {
        expression: z
          .string()
          .describe('The Gerbil Scheme expression to run'),
        functions: z
          .array(z.string())
          .describe(
            'Function names to count calls for (e.g. ["read-json", "hash-put!"])',
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

      exprs.push(buildTraceExpr(escaped, functions));

      const env = loadpath && loadpath.length > 0
        ? buildLoadpathEnv(loadpath)
        : undefined;

      const result = await runGxi(exprs, { timeout: 120_000, env });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Trace timed out after 120 seconds.',
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
              text: `Trace error:\n${result.stderr.trim()}`,
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
              text: `Trace error:\n${errorMsg}`,
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
      const fnCounts: Array<{ name: string; calls: number }> = [];
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
        } else {
          fnCounts.push({ name: key, calls: parseInt(val, 10) });
        }
      }

      // Sort by call count descending
      fnCounts.sort((a, b) => b.calls - a.calls);

      // Format output
      const sections: string[] = [`Call Trace: ${expression}`, ''];

      if (warnings.length > 0) {
        for (const w of warnings) {
          sections.push(`Warning: ${w}`);
        }
        sections.push('');
      }

      if (fnCounts.length > 0) {
        const maxNameLen = Math.max(
          8,
          ...fnCounts.map((f) => f.name.length),
        );
        sections.push(
          `${'Function'.padEnd(maxNameLen)}  ${'Calls'.padStart(12)}`,
        );
        for (const f of fnCounts) {
          sections.push(
            `${f.name.padEnd(maxNameLen)}  ${f.calls.toString().padStart(12)}`,
          );
        }
      } else if (functions.length === 0) {
        sections.push('No functions specified for tracing.');
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

function safeName(fn: string): string {
  return fn.replace(/[^a-zA-Z0-9]/g, '_');
}

function buildTraceExpr(
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

  // Instrument each function
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
      `              (eval (quote (begin (def __tc_${safe}_count 0) (def __tc_${safe}_orig ${fn}) (set! ${fn} (lambda args (set! __tc_${safe}_count (+ __tc_${safe}_count 1)) (apply __tc_${safe}_orig args))))))))))`,
    );
  }

  // Run expression and report
  parts.push(
    `      (let ((result (eval (read (open-input-string "${escapedExpr}")))))`,
  );

  // Output per-function counts
  for (const fn of functions) {
    const safeFn = escapeSchemeString(fn);
    const safe = safeName(fn);
    parts.push(`        (with-catch (lambda (e) (void))`);
    parts.push(`          (lambda ()`);
    parts.push(
      `            (let ((count (eval (quote __tc_${safe}_count))))`,
    );
    parts.push(
      `              (display "${RESULT_MARKER}${safeFn}\\t")`,
    );
    parts.push(`              (display count)`);
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
