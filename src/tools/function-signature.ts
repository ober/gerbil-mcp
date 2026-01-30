import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-SIG:';

export function registerFunctionSignatureTool(server: McpServer): void {
  server.registerTool(
    'gerbil_function_signature',
    {
      title: 'Inspect Function Signatures',
      description:
        'Get arity and type info for exported symbols in a Gerbil module. ' +
        'Shows whether each export is a procedure (with arity), macro/syntax, or value. ' +
        'Example: module_path ":std/text/json" returns all exports with their arities. ' +
        'Optionally filter to a single symbol.',
      inputSchema: {
        module_path: z
          .string()
          .describe('Module path (e.g. ":std/text/json", ":std/iter")'),
        symbol: z
          .string()
          .optional()
          .describe(
            'Specific symbol to inspect. If omitted, inspects all exports.',
          ),
      },
    },
    async ({ module_path, symbol }) => {
      const modPath = module_path.startsWith(':')
        ? module_path
        : `:${module_path}`;

      const filterExpr = symbol
        ? `(let ((target "${escapeSchemeString(symbol)}")) (lambda (name) (string=? (symbol->string name) target)))`
        : '(lambda (name) #t)';

      const exprs = [
        `(import :gerbil/expander)`,
        `(import ${modPath})`,
        [
          '(with-catch',
          '  (lambda (e)',
          `    (display "${ERROR_MARKER}\\n")`,
          '    (display-exception e (current-output-port)))',
          '  (lambda ()',
          `    (let* ((mod (import-module (quote ${modPath}) #f #t))`,
          '           (exports (module-context-export mod))',
          `           (filter-fn ${filterExpr}))`,
          '      (for-each',
          '        (lambda (e)',
          '          (let ((name (module-export-name e)))',
          '            (when (filter-fn name)',
          '              (with-catch',
          '                (lambda (ex)',
          `                  (display "${RESULT_MARKER}")`,
          '                  (display name)',
          '                  (display "\\tmacro/syntax\\t\\t")',
          '                  (newline))',
          '                (lambda ()',
          '                  (let ((val (eval name)))',
          '                    (cond',
          '                      ((procedure? val)',
          `                       (display "${RESULT_MARKER}")`,
          '                       (display name)',
          '                       (display "\\tprocedure\\t")',
          '                       (display (##subprocedure-nb-parameters val))',
          '                       (display "\\t")',
          '                       (display (##procedure-name val))',
          '                       (newline))',
          '                      (else',
          `                       (display "${RESULT_MARKER}")`,
          '                       (display name)',
          '                       (display "\\tvalue\\t\\t")',
          '                       (display (type-of val))',
          '                       (newline))))))',
          '            )))',
          '        exports))))',
        ].join(' '),
      ];

      const result = await runGxi(exprs);

      if (result.timedOut) {
        return {
          content: [
            { type: 'text' as const, text: 'Signature introspection timed out.' },
          ],
          isError: true,
        };
      }

      if (result.exitCode !== 0 && result.stderr) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to load module ${modPath}:\n${result.stderr.trim()}`,
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
              text: `Error inspecting ${modPath}:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse result lines
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      if (lines.length === 0) {
        const msg = symbol
          ? `Symbol "${symbol}" not found in ${modPath} exports.`
          : `No exports found in ${modPath}.`;
        return { content: [{ type: 'text' as const, text: msg }] };
      }

      const entries = lines.map((line) => {
        const parts = line.slice(RESULT_MARKER.length).split('\t');
        const name = parts[0] || '';
        const kind = parts[1] || '';
        const arity = parts[2] || '';
        const qualifiedName = parts[3] || '';
        return { name, kind, arity, qualifiedName };
      });

      const formatted = [
        `${modPath} — ${entries.length} export(s):`,
        '',
        ...entries.map((e) => {
          if (e.kind === 'procedure') {
            return `  ${e.name}  procedure  arity:${e.arity}  (${e.qualifiedName})`;
          } else if (e.kind === 'macro/syntax') {
            return `  ${e.name}  macro/syntax`;
          } else {
            return `  ${e.name}  ${e.kind}  ${e.qualifiedName}`;
          }
        }),
      ].join('\n');

      return { content: [{ type: 'text' as const, text: formatted }] };
    },
  );
}
