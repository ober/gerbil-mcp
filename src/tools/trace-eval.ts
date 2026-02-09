import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join } from 'node:path';
import { runGxi, escapeSchemeString, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-TRACE:';

export function registerTraceEvalTool(server: McpServer): void {
  server.registerTool(
    'gerbil_trace_eval',
    {
      title: 'Trace Let Bindings',
      description:
        'Evaluate a Gerbil let*/let/letrec/letrec* expression and trace each binding step, ' +
        'showing the name, type summary, and value of each variable as it is bound. ' +
        'For non-let expressions, evaluates and reports the result. ' +
        'Useful for debugging complex binding chains.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        expression: z
          .string()
          .describe('The Gerbil Scheme expression to trace'),
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
        project_path: z
          .string()
          .optional()
          .describe(
            'Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib',
          ),
      },
    },
    async ({ expression, imports, loadpath, project_path }) => {
      const escaped = escapeSchemeString(expression);

      const exprs: string[] = [];
      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      exprs.push(buildTraceExpr(escaped));

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const env =
        effectiveLoadpath.length > 0
          ? buildLoadpathEnv(effectiveLoadpath)
          : undefined;

      const result = await runGxi(exprs, { timeout: 30_000, env });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Trace timed out after 30 seconds.',
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
        const sideOutput = stdout.slice(0, errorIdx).trim();
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        const parts: string[] = [];
        if (sideOutput) parts.push(`Output:\n${sideOutput}`);
        parts.push(`Error:\n${errorMsg}`);
        return {
          content: [
            { type: 'text' as const, text: parts.join('\n\n') },
          ],
          isError: true,
        };
      }

      // Parse trace lines
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      const bindings: Array<{
        name: string;
        type: string;
        value: string;
      }> = [];

      for (const line of lines) {
        const payload = line.slice(RESULT_MARKER.length);
        const parts = payload.split('\t');
        if (parts.length >= 3) {
          bindings.push({
            name: parts[0],
            type: parts[1],
            value: parts.slice(2).join('\t'),
          });
        }
      }

      if (bindings.length === 0) {
        // No trace lines — maybe side-output only
        const output = stdout.trim();
        return {
          content: [
            { type: 'text' as const, text: output || '(void)' },
          ],
        };
      }

      // Format as aligned table
      const maxName = Math.max(4, ...bindings.map((b) => b.name.length));
      const maxType = Math.max(4, ...bindings.map((b) => b.type.length));

      const header = `${'Name'.padEnd(maxName)}  ${'Type'.padEnd(maxType)}  Value`;
      const separator = `${'─'.repeat(maxName)}  ${'─'.repeat(maxType)}  ${'─'.repeat(30)}`;

      const rows = bindings.map((b) => {
        const prefix = b.name === '__result' ? '→ ' : '  ';
        const displayName =
          b.name === '__result' ? 'result' : b.name;
        return `${prefix}${displayName.padEnd(maxName)}  ${b.type.padEnd(maxType)}  ${b.value}`;
      });

      const sections = [
        `Trace: ${expression}`,
        '',
        `  ${header}`,
        `  ${separator}`,
        ...rows,
      ];

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

function buildTraceExpr(escapedExpr: string): string {
  const parts: string[] = [];

  // Helper: describe type of a value
  const describeType = `
    (def (__mcp_describe_type v)
      (cond
        ((string? v) (string-append "string (" (number->string (string-length v)) " chars)"))
        ((and (pair? v) (symbol? (car v)))
         (string-append "pair (" (symbol->string (car v))
           " with " (number->string (length (cdr v))) " children)"))
        ((pair? v) (string-append "list (" (number->string (length v)) " elems)"))
        ((hash-table? v)
         (string-append "hash-table (" (number->string (hash-length v)) " entries)"))
        ((vector? v) (string-append "vector (length " (number->string (vector-length v)) ")"))
        ((number? v) "number")
        ((boolean? v) "boolean")
        ((symbol? v) "symbol")
        ((void? v) "void")
        ((null? v) "null")
        ((procedure? v) "procedure")
        ((keyword? v) "keyword")
        (else (string-append "other: " (symbol->string (##type-name (##type v)))))))
  `.trim().replace(/\n\s*/g, ' ');

  // Helper: truncate repr
  const truncateRepr = `
    (def (__mcp_truncate_repr v)
      (let ((s (call-with-output-string (lambda (p) (write v p)))))
        (if (> (string-length s) 200)
          (string-append (substring s 0 200) "...")
          s)))
  `.trim().replace(/\n\s*/g, ' ');

  parts.push('(with-catch');
  parts.push('  (lambda (e)');
  parts.push(`    (display "${ERROR_MARKER}\\n")`);
  parts.push('    (display-exception e (current-output-port)))');
  parts.push('  (lambda ()');
  parts.push(`    ${describeType}`);
  parts.push(`    ${truncateRepr}`);

  // Read expression, check if it's a let-form, trace accordingly
  parts.push(`    (let ((form (read (open-input-string "${escapedExpr}"))))`);
  parts.push(`      (if (and (pair? form) (memq (car form) '(let* let letrec letrec*)))`);
  // It's a let-form: trace each binding
  parts.push(`        (let ((bindings (cadr form)) (body (cddr form)))`);
  parts.push(`          (for-each`);
  parts.push(`            (lambda (binding)`);
  parts.push(`              (let ((name (car binding)) (expr (cadr binding)))`);
  parts.push(`                (with-catch`);
  parts.push(`                  (lambda (e)`);
  parts.push(`                    (display "${RESULT_MARKER}")`);
  parts.push(`                    (display name) (display "\\t")`);
  parts.push(`                    (display "ERROR") (display "\\t")`);
  parts.push(`                    (display-exception e (current-output-port))`);
  parts.push(`                    (newline))`);
  parts.push(`                  (lambda ()`);
  parts.push(`                    (eval (list 'define name expr))`);
  parts.push(`                    (let ((val (eval name)))`);
  parts.push(`                      (display "${RESULT_MARKER}")`);
  parts.push(`                      (display name) (display "\\t")`);
  parts.push(`                      (display (__mcp_describe_type val)) (display "\\t")`);
  parts.push(`                      (display (__mcp_truncate_repr val))`);
  parts.push(`                      (newline))))))`);
  parts.push(`            bindings)`);
  // Evaluate body
  parts.push(`          (let ((result (eval (cons 'begin body))))`);
  parts.push(`            (display "${RESULT_MARKER}__result\\t")`);
  parts.push(`            (display (__mcp_describe_type result)) (display "\\t")`);
  parts.push(`            (display (__mcp_truncate_repr result))`);
  parts.push(`            (newline)))`);
  // Not a let-form: just eval
  parts.push(`        (let ((result (eval form)))`);
  parts.push(`          (display "${RESULT_MARKER}__result\\t")`);
  parts.push(`          (display (__mcp_describe_type result)) (display "\\t")`);
  parts.push(`          (display (__mcp_truncate_repr result))`);
  parts.push(`          (newline)))))))`);

  return parts.join(' ');
}
