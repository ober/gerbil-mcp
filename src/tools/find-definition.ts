import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-FIND:';

export function registerFindDefinitionTool(server: McpServer): void {
  server.registerTool(
    'gerbil_find_definition',
    {
      title: 'Find Symbol Definition',
      description:
        'Find where a Gerbil symbol is defined. Returns the qualified name, ' +
        'module file path, source file path (if available), kind (procedure/macro/value), ' +
        'and arity for procedures. Uses runtime introspection and module resolution.',
      inputSchema: {
        symbol: z
          .string()
          .describe(
            'Symbol name to look up (e.g. "read-json", "map", "defstruct")',
          ),
        module_path: z
          .string()
          .optional()
          .describe(
            'Module to import for context (e.g. ":std/text/json"). If omitted, searches current environment.',
          ),
      },
    },
    async ({ symbol, module_path }) => {
      const escapedSym = escapeSchemeString(symbol);

      const exprs: string[] = ['(import :gerbil/expander)'];

      if (module_path) {
        const modPath = module_path.startsWith(':')
          ? module_path
          : `:${module_path}`;
        exprs.push(`(import ${modPath})`);
      }

      exprs.push(buildFindExpr(escapedSym, module_path));

      const result = await runGxi(exprs);

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Definition lookup timed out.',
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
              text: `Error looking up ${symbol}:\n${result.stderr.trim()}`,
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
              text: `Error looking up ${symbol}:\n${errorMsg}`,
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
              text: `Symbol "${symbol}" not found.`,
            },
          ],
          isError: true,
        };
      }

      // Build formatted output
      const sections: string[] = [`Symbol: ${symbol}`, ''];

      if (info['kind']) {
        sections.push(`Kind: ${info['kind']}`);
      }

      if (info['qualified']) {
        sections.push(`Qualified name: ${info['qualified']}`);
      }

      if (info['arity']) {
        sections.push(`Arity: ${info['arity']}`);
      }

      if (info['type']) {
        sections.push(`Type: ${info['type']}`);
      }

      if (module_path) {
        sections.push(
          `Module: ${module_path.startsWith(':') ? module_path : `:${module_path}`}`,
        );
      }

      if (info['ssi-path']) {
        sections.push(`Module file: ${info['ssi-path']}`);
      }

      if (info['source-path']) {
        sections.push(`Source file: ${info['source-path']}`);
      } else if (info['ssi-path']) {
        sections.push('Source file: (not available — compiled module)');
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

function buildFindExpr(escapedSym: string, modulePath?: string): string {
  const modResolution = modulePath
    ? buildModuleResolution(modulePath)
    : '(void)';

  return [
    '(with-catch',
    '  (lambda (e)',
    `    (display "${ERROR_MARKER}\\n")`,
    '    (display-exception e (current-output-port)))',
    '  (lambda ()',
    `    (let ((sym (string->symbol "${escapedSym}")))`,
    '      (with-catch',
    '        (lambda (ex)',
    // eval failed → likely macro/syntax
    `          (display "${RESULT_MARKER}kind\\tmacro/syntax\\n")`,
    `          (display "${RESULT_MARKER}name\\t${escapedSym}\\n")`,
    `          ${modResolution})`,
    '        (lambda ()',
    '          (let ((val (eval sym)))',
    '            (cond',
    '              ((procedure? val)',
    `               (display "${RESULT_MARKER}kind\\tprocedure\\n")`,
    '               (let* ((pname (##procedure-name val))',
    '                      (pname-str (symbol->string pname)))',
    `                 (display "${RESULT_MARKER}qualified\\t")`,
    '                 (display pname-str)',
    '                 (newline)',
    `                 (display "${RESULT_MARKER}arity\\t")`,
    '                 (display (##subprocedure-nb-parameters val))',
    '                 (newline)',
    '                 (let ((hash-idx (string-index pname-str #\\#)))',
    '                   (if hash-idx',
    '                     (let* ((mod-part (substring pname-str 0 hash-idx))',
    '                            (mod-sym (string->symbol (string-append ":" mod-part))))',
    '                       (with-catch',
    '                         (lambda (e2) (void))',
    '                         (lambda ()',
    '                           (let ((resolved (core-resolve-library-module-path mod-sym)))',
    `                             (display "${RESULT_MARKER}ssi-path\\t")`,
    '                             (display resolved)',
    '                             (newline)',
    '                             (let ((ss-path (string-append (path-strip-extension resolved) ".ss")))',
    '                               (when (file-exists? ss-path)',
    `                                 (display "${RESULT_MARKER}source-path\\t")`,
    '                                 (display ss-path)',
    '                                 (newline)))))))',
    `                     ${modResolution})))`,
    '              (else',
    `               (display "${RESULT_MARKER}kind\\tvalue\\n")`,
    `               (display "${RESULT_MARKER}name\\t${escapedSym}\\n")`,
    `               (display "${RESULT_MARKER}type\\t")`,
    '               (display (type-of val))',
    '               (newline)',
    `               ${modResolution}))))))))`,
  ].join(' ');
}

function buildModuleResolution(modulePath: string): string {
  const modPath = modulePath.startsWith(':')
    ? modulePath
    : `:${modulePath}`;

  return [
    '(with-catch (lambda (e3) (void))',
    '  (lambda ()',
    `    (let ((resolved (core-resolve-library-module-path (quote ${modPath}))))`,
    `      (display "${RESULT_MARKER}ssi-path\\t")`,
    '      (display resolved)',
    '      (newline)',
    '      (let ((ss-path (string-append (path-strip-extension resolved) ".ss")))',
    '        (when (file-exists? ss-path)',
    `          (display "${RESULT_MARKER}source-path\\t")`,
    '          (display ss-path)',
    '          (newline))))))',
  ].join(' ');
}
