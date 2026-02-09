import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join } from 'node:path';
import { runGxi, escapeSchemeString, RESULT_MARKER, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

export function registerDescribeTool(server: McpServer): void {
  server.registerTool(
    'gerbil_describe',
    {
      title: 'Describe Gerbil Value',
      description:
        "Evaluate a Gerbil Scheme expression and describe the resulting value's type, " +
        'structure, and key properties. Useful for understanding what a function returns ' +
        'or what a data structure contains.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        expression: z.string().describe('The Gerbil Scheme expression to evaluate and describe'),
        imports: z
          .array(z.string())
          .optional()
          .describe('Module paths to import before evaluation (e.g. [":std/text/json"])'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Directories to add to GERBIL_LOADPATH for project-local module resolution'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib'),
        env: z
          .record(z.string())
          .optional()
          .describe('Environment variables to pass to the gxi subprocess'),
      },
    },
    async ({ expression, imports, loadpath, project_path, env: extraEnv }) => {
      const escaped = escapeSchemeString(expression);

      const exprs: string[] = [];

      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      // Describe wrapper: evaluates the expression, then introspects the result.
      // Bug fix from PR#3: (length val) is only called inside the (list? val) branch
      // to avoid crashing on improper lists.
      const errMarker = ERROR_MARKER;
      const resMarker = RESULT_MARKER;
      const wrapper =
        '(with-catch' +
        '  (lambda (e)' +
        '    (display "' + errMarker + '\\n")' +
        '    (display-exception e (current-output-port)))' +
        '  (lambda ()' +
        '    (let ((val (eval (read (open-input-string "' + escaped + '")))))' +
        '      (display "' + resMarker + '")' +
        '      (cond' +
        '        ((hash-table? val)' +
        '         (displayln (string-append "hash-table (" (number->string (hash-length val)) " entries)"))' +
        '         (hash-for-each (lambda (k v) (display "  ") (write k) (display " => ") (write v) (newline)) val))' +
        '        ((list? val)' +
        '         (displayln (string-append "list (length " (number->string (length val)) ")"))' +
        '         (for-each (lambda (x) (display "  ") (write x) (newline)) val))' +
        '        ((pair? val)' +
        '         (displayln "pair")' +
        '         (display "  car: ") (write (car val)) (newline)' +
        '         (display "  cdr: ") (write (cdr val)) (newline))' +
        '        ((vector? val)' +
        '         (displayln (string-append "vector (length " (number->string (vector-length val)) ")"))' +
        '         (let loop ((i 0)) (when (< i (vector-length val)) (display "  [") (display i) (display "] ") (write (vector-ref val i)) (newline) (loop (+ i 1)))))' +
        '        ((string? val)' +
        '         (displayln (string-append "string (length " (number->string (string-length val)) ")"))' +
        '         (display "  ") (write val) (newline))' +
        '        ((number? val)' +
        '         (cond' +
        '           ((exact-integer? val) (displayln (string-append "exact integer: " (number->string val))))' +
        '           ((flonum? val) (displayln (string-append "flonum: " (number->string val))))' +
        '           ((rational? val) (displayln (string-append "rational: " (number->string val))))' +
        '           (else (displayln (string-append "number: " (number->string val))))))' +
        '        ((boolean? val)' +
        '         (displayln (string-append "boolean: " (if val "#t" "#f"))))' +
        '        ((symbol? val)' +
        '         (displayln (string-append "symbol: " (symbol->string val))))' +
        '        ((keyword? val)' +
        '         (displayln (string-append "keyword: " (keyword->string val))))' +
        '        ((char? val)' +
        '         (displayln (string-append "char: " (string val))))' +
        '        ((procedure? val)' +
        '         (displayln "procedure"))' +
        '        ((void? val)' +
        '         (displayln "void"))' +
        '        ((null? val)' +
        '         (displayln "null (empty list)"))' +
        '        ((eof-object? val)' +
        '         (displayln "eof-object"))' +
        '        (else' +
        '         (displayln (string-append "object: " (with-output-to-string (lambda () (write val))))))))))';

      exprs.push(wrapper);

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const loadpathEnv = effectiveLoadpath.length > 0 ? buildLoadpathEnv(effectiveLoadpath) : undefined;
      const env = { ...loadpathEnv, ...extraEnv };
      const result = await runGxi(exprs, { env: Object.keys(env).length > 0 ? env : undefined });

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Evaluation timed out.' }],
          isError: true,
        };
      }

      if (result.exitCode !== 0 && result.stderr) {
        return {
          content: [{ type: 'text' as const, text: 'Error:\n' + result.stderr.trim() }],
          isError: true,
        };
      }

      const stdout = result.stdout;

      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        return {
          content: [{ type: 'text' as const, text: 'Error:\n' + errorMsg }],
          isError: true,
        };
      }

      const resultIdx = stdout.indexOf(RESULT_MARKER);
      if (resultIdx !== -1) {
        const description = stdout.slice(resultIdx + RESULT_MARKER.length).trim();
        return {
          content: [{ type: 'text' as const, text: description }],
        };
      }

      return {
        content: [{ type: 'text' as const, text: stdout.trim() || '(no output)' }],
      };
    },
  );
}
