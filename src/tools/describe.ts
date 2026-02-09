import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join } from 'node:path';
import { runGxi, escapeSchemeString, ERROR_MARKER, RESULT_MARKER, buildLoadpathEnv } from '../gxi.js';

export function registerDescribeTool(server: McpServer): void {
  server.registerTool(
    'gerbil_describe',
    {
      title: 'Describe Gerbil Value',
      description:
        'Evaluate a Gerbil expression and return a description of its type and structure, ' +
        'not just its value. Useful for understanding what kind of data a function returns. ' +
        'Examples: hash tables show entry count and keys, vectors show length and type, ' +
        'structs/classes show type name and field count.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        expression: z
          .string()
          .describe('The Gerbil expression to evaluate and describe'),
        imports: z
          .array(z.string())
          .optional()
          .describe('Modules to import before evaluation'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Directories to add to GERBIL_LOADPATH'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory for auto-configuring GERBIL_LOADPATH'),
      },
    },
    async ({ expression, imports, loadpath, project_path }) => {
      const escaped = escapeSchemeString(expression);

      const exprs: string[] = [];

      // Import modules
      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      // Add describe function
      const describeFunc = [
        '(def (describe-value val)',
        '  (cond',
        // Primitive types
        '    ((eq? val #f) "boolean: #f")',
        '    ((eq? val #t) "boolean: #t")',
        '    ((void? val) "void")',
        '    ((eof-object? val) "eof-object")',
        // Numeric types
        '    ((number? val)',
        '     (cond',
        '       ((exact? val)',
        '        (if (integer? val)',
        '            (string-append "exact integer: " (number->string val))',
        '            (string-append "exact rational: " (number->string val))))',
        '       ((flonum? val) (string-append "flonum: " (number->string val)))',
        '       (else (string-append "number: " (number->string val)))))',
        // String
        '    ((string? val)',
        '     (let ((len (string-length val)))',
        '       (if (< len 40)',
        '           (string-append "string (length " (number->string len) "): \\"" val "\\"")',
        '           (string-append "string (length " (number->string len) "): \\"" (substring val 0 37) "...\\""))))',
        // Symbol
        '    ((symbol? val)',
        '     (string-append "symbol: " (symbol->string val)))',
        // Keyword
        '    ((keyword? val)',
        '     (string-append "keyword: " (keyword->string val)))',
        // Character
        '    ((char? val)',
        '     (string-append "character: #\\\\" (make-string 1 val)))',
        // Null
        '    ((null? val) "empty list: ()")',
        // Pair/List
        '    ((pair? val)',
        '     (let ((len (length val)))',
        '       (if (list? val)',
        '           (string-append "list (length " (number->string len) ")")',
        '           "improper list (dotted pair)")))',
        // Vector
        '    ((vector? val)',
        '     (let ((len (vector-length val)))',
        '       (string-append "vector (length " (number->string len) ")")))',
        // U8vector
        '    ((u8vector? val)',
        '     (let ((len (u8vector-length val)))',
        '       (string-append "u8vector/bytes (length " (number->string len) ")")))',
        // Hash table
        '    ((hash-table? val)',
        '     (let* ((len (hash-length val))',
        '            (keys (hash-keys val))',
        '            (key-str (if (< len 6)',
        '                         (apply string-append',
        '                                (map (lambda (k) (string-append " " (with-output-to-string (lambda () (write k)))))',
        '                                     keys))',
        '                         (apply string-append',
        '                                (append',
        '                                 (map (lambda (k) (string-append " " (with-output-to-string (lambda () (write k)))))',
        '                                      (take keys 5))',
        '                                 (list " ..."))))))',
        '       (string-append "hash-table (" (number->string len) " entries, keys:" key-str ")")))',
        // Procedure
        '    ((procedure? val) "procedure")',
        // Struct/Class instance
        '    ((##structure? val)',
        '     (let* ((type (##structure-type val))',
        '            (type-name (##type-name type))',
        '            (field-count (##type-field-count type)))',
        '       (string-append',
        '         "struct/class instance: " (symbol->string type-name)',
        '         " (" (number->string field-count) " fields)")))',
        // Unknown
        '    (else "unknown type")))',
      ].join('\n');

      exprs.push(describeFunc);

      // Eval wrapper with describe
      const wrapper = [
        '(with-catch',
        '  (lambda (e)',
        `    (display "${ERROR_MARKER}\\n")`,
        '    (display-exception e (current-output-port)))',
        '  (lambda ()',
        `    (let ((val (eval (read (open-input-string "${escaped}")))))`,
        '      (let ((desc (describe-value val)))',
        `        (display "${RESULT_MARKER}")`,
        '        (displayln desc)))))',
      ].join(' ');

      exprs.push(wrapper);

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const loadpathEnv = effectiveLoadpath.length > 0 ? buildLoadpathEnv(effectiveLoadpath) : undefined;
      const env = loadpathEnv ? { ...loadpathEnv } : undefined;
      const result = await runGxi(exprs, { env: Object.keys(env ?? {}).length > 0 ? env : undefined });

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Description timed out after 30 seconds.' }],
          isError: true,
        };
      }

      // Import failures produce non-zero exit with stderr
      if (result.exitCode !== 0 && result.stderr) {
        return {
          content: [{ type: 'text' as const, text: `Import/load error:\n${result.stderr.trim()}` }],
          isError: true,
        };
      }

      const stdout = result.stdout;

      // Check for error marker
      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        return {
          content: [{ type: 'text' as const, text: `Error:\n${errorMsg}` }],
          isError: true,
        };
      }

      // Check for result marker
      const resultIdx = stdout.indexOf(RESULT_MARKER);
      if (resultIdx !== -1) {
        const description = stdout.slice(resultIdx + RESULT_MARKER.length).trim();
        return {
          content: [{ type: 'text' as const, text: description }],
        };
      }

      // No markers — unexpected output
      const output = stdout.trim();
      return {
        content: [{ type: 'text' as const, text: output || 'No description available.' }],
      };
    },
  );
}
