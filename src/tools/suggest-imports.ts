import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-IMPORT:';

// Curated list of commonly-used standard library modules to scan.
// Kept as a Scheme list literal to avoid per-module subprocess overhead.
export const STD_MODULES = [
  ':std/iter',
  ':std/sugar',
  ':std/test',
  ':std/error',
  ':std/format',
  ':std/sort',
  ':std/text/json',
  ':std/text/csv',
  ':std/text/utf8',
  ':std/pregexp',
  ':std/net/request',
  ':std/net/httpd',
  ':std/net/websocket',
  ':std/net/ssl',
  ':std/io',
  ':std/os/socket',
  ':std/misc/channel',
  ':std/misc/threads',
  ':std/misc/string',
  ':std/misc/list',
  ':std/misc/list-builder',
  ':std/misc/queue',
  ':std/misc/deque',
  ':std/misc/rbtree',
  ':std/misc/pqueue',
  ':std/misc/sync',
  ':std/coroutine',
  ':std/generic',
  ':std/foreign',
  ':std/crypto',
  ':std/actor',
  ':std/db/postgresql',
  ':std/db/sqlite',
  ':std/srfi/1',
  ':std/srfi/13',
];

export function registerSuggestImportsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_suggest_imports',
    {
      title: 'Suggest Imports',
      description:
        'Find which standard library module exports a given symbol. ' +
        'Scans common :std/* modules and reports matching import statements. ' +
        'For less common modules, use gerbil_apropos + gerbil_module_exports as a fallback.',
      inputSchema: {
        symbol: z
          .string()
          .describe(
            'Symbol to find the import for (e.g. "for/collect", "read-json", "hash-ref")',
          ),
      },
    },
    async ({ symbol }) => {
      const escapedSym = escapeSchemeString(symbol);
      const expr = buildSuggestExpr(escapedSym);

      const result = await runGxi(['(import :gerbil/expander)', expr], { timeout: 60_000 });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Import search timed out after 60 seconds.',
            },
          ],
          isError: true,
        };
      }

      if (result.exitCode === 127) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'gxi not found. Ensure Gerbil is installed and gxi is in PATH.',
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
              text: `Error searching for "${symbol}":\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse GERBIL-MCP-IMPORT:module-path lines
      const importLines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      const modules = importLines.map((l) =>
        l.slice(RESULT_MARKER.length).trim(),
      );

      if (modules.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text:
                `Symbol "${symbol}" not found in common standard library modules.\n\n` +
                `Try using gerbil_apropos to search more broadly, then gerbil_module_exports to check specific modules.`,
            },
          ],
        };
      }

      const sections: string[] = [
        `Symbol "${symbol}" is exported by:`,
        '',
      ];
      for (const mod of modules) {
        sections.push(`  (import ${mod})`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

function buildSuggestExpr(escapedSym: string): string {
  const moduleList = STD_MODULES.map((m) => `(quote ${m})`).join(' ');

  return [
    '(with-catch',
    '  (lambda (e)',
    `    (display "${ERROR_MARKER}\\n")`,
    '    (display-exception e (current-output-port)))',
    '  (lambda ()',
    `    (let* ((target (string->symbol "${escapedSym}"))`,
    `           (modules (list ${moduleList})))`,
    '      (for-each',
    '        (lambda (mod-sym)',
    '          (with-catch',
    '            (lambda (e) (void))',
    '            (lambda ()',
    '              (let* ((mod (import-module mod-sym #f #t))',
    '                     (exports (module-context-export mod)))',
    '                (for-each',
    '                  (lambda (ex)',
    '                    (when (eq? (module-export-name ex) target)',
    `                      (display "${RESULT_MARKER}")`,
    '                      (display mod-sym)',
    '                      (newline)))',
    '                  exports)))))',
    '        modules))))',
  ].join(' ');
}
