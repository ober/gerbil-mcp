import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-HEAP:';

export function registerHeapProfileTool(server: McpServer): void {
  server.registerTool(
    'gerbil_heap_profile',
    {
      title: 'Heap Memory Profile',
      description:
        'Capture GC heap metrics before and after running an expression. ' +
        'Reports heap size, allocation, live objects, movable objects, and still objects. ' +
        'Forces garbage collection before each snapshot for accurate measurements. ' +
        'Uses :std/debug/heap memory-usage function.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        expression: z
          .string()
          .describe('The Gerbil Scheme expression to profile'),
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
    async ({ expression, imports, loadpath }) => {
      const escaped = escapeSchemeString(expression);

      const exprs: string[] = [];
      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      exprs.push('(import :std/debug/heap)');
      exprs.push(buildHeapProfileExpr(escaped));

      const env = loadpath && loadpath.length > 0
        ? buildLoadpathEnv(loadpath)
        : undefined;

      const result = await runGxi(exprs, { timeout: 120_000, env });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Heap profile timed out after 120 seconds.',
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
              text: `Heap profile error:\n${result.stderr.trim()}`,
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
              text: `Heap profile error:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse result lines
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      const metrics: Array<{
        name: string;
        before: number;
        after: number;
      }> = [];
      let resultValue: string | undefined;

      for (const line of lines) {
        const payload = line.slice(RESULT_MARKER.length);
        const tabIdx = payload.indexOf('\t');
        if (tabIdx === -1) continue;
        const key = payload.slice(0, tabIdx);
        const val = payload.slice(tabIdx + 1).trim();

        if (key === '__result') {
          resultValue = val;
        } else {
          const parts = val.split('\t');
          if (parts.length >= 2) {
            metrics.push({
              name: key,
              before: parseFloat(parts[0]),
              after: parseFloat(parts[1]),
            });
          }
        }
      }

      if (metrics.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'No heap metrics collected. The :std/debug/heap module may not be available.',
            },
          ],
          isError: true,
        };
      }

      // Format output
      const sections: string[] = [`Heap Profile: ${expression}`, ''];

      const maxNameLen = Math.max(
        6,
        ...metrics.map((m) => m.name.length),
      );
      sections.push(
        `${'Metric'.padEnd(maxNameLen)}  ${'Before'.padStart(12)}  ${'After'.padStart(12)}  ${'Delta'.padStart(12)}`,
      );
      for (const m of metrics) {
        const delta = m.after - m.before;
        const sign = delta >= 0 ? '+' : '';
        sections.push(
          `${m.name.padEnd(maxNameLen)}  ${formatBytes(m.before).padStart(12)}  ${formatBytes(m.after).padStart(12)}  ${(sign + formatBytes(delta)).padStart(12)}`,
        );
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

function formatBytes(b: number): string {
  const abs = Math.abs(b);
  const sign = b < 0 ? '-' : '';
  if (abs < 1024) return `${sign}${Math.round(abs)} B`;
  if (abs < 1024 * 1024) return `${sign}${(abs / 1024).toFixed(1)} KB`;
  if (abs < 1024 * 1024 * 1024) return `${sign}${(abs / (1024 * 1024)).toFixed(1)} MB`;
  return `${sign}${(abs / (1024 * 1024 * 1024)).toFixed(2)} GB`;
}

function buildHeapProfileExpr(escapedExpr: string): string {
  return [
    '(with-catch',
    '  (lambda (e)',
    `    (display "${ERROR_MARKER}\\n")`,
    '    (display-exception e (current-output-port)))',
    '  (lambda ()',
    '    (##gc)',
    '    (let ((before (memory-usage)))',
    `      (let ((result (eval (read (open-input-string "${escapedExpr}")))))`,
    '        (##gc)',
    '        (let ((after (memory-usage)))',
    '          (for-each',
    '            (lambda (entry)',
    '              (let* ((key (car entry))',
    '                     (bval (cdr entry))',
    '                     (apair (assoc key after))',
    '                     (aval (if apair (cdr apair) 0)))',
    `                (display "${RESULT_MARKER}")`,
    '                (display key)',
    '                (display "\\t")',
    '                (display bval)',
    '                (display "\\t")',
    '                (display aval)',
    '                (newline)))',
    '            before)',
    '          (unless (void? result)',
    `            (display "${RESULT_MARKER}__result\\t")`,
    '            (write result)',
    '            (newline)))))))',
  ].join(' ');
}
