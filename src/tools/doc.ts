import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

const INFO_MARKER = 'GERBIL-MCP-INFO:';
const APROPOS_MARKER = 'GERBIL-MCP-APROPOS-START';

export function registerDocTool(server: McpServer): void {
  server.registerTool(
    'gerbil_doc',
    {
      title: 'Symbol Documentation',
      description:
        'Look up comprehensive info about a Gerbil/Gambit symbol: its type ' +
        '(procedure/macro/value), arity, qualified name, and related symbols. ' +
        'Optionally import a module to bring the symbol into scope. ' +
        'Example: symbol "read-json" with module_path ":std/text/json".',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        symbol: z.string().describe('Symbol name to look up (e.g. "map", "read-json")'),
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

      const exprs: string[] = [];

      // Import module if specified
      if (module_path) {
        const modPath = module_path.startsWith(':')
          ? module_path
          : `:${module_path}`;
        exprs.push(`(import ${modPath})`);
      }

      // Build introspection expression — info first, then apropos separately
      const schemeExpr = [
        '(with-catch',
        '  (lambda (e)',
        `    (display "${ERROR_MARKER}\\n")`,
        '    (display-exception e (current-output-port)))',
        '  (lambda ()',
        `    (let ((sym (string->symbol "${escapedSym}")))`,
        '      (with-catch',
        '        (lambda (ex)',
        `          (display "${INFO_MARKER}kind\\tmacro/syntax\\n")`,
        `          (display "${INFO_MARKER}name\\t${escapedSym}\\n"))`,
        '        (lambda ()',
        '          (let ((val (eval sym)))',
        '            (cond',
        '              ((procedure? val)',
        `               (display "${INFO_MARKER}kind\\tprocedure\\n")`,
        `               (display "${INFO_MARKER}name\\t")`,
        '               (display (##procedure-name val))',
        '               (newline)',
        `               (display "${INFO_MARKER}arity\\t")`,
        '               (display (##subprocedure-nb-parameters val))',
        '               (newline)',
        '               (let ((loc (##procedure-locat val)))',
        '                 (when loc',
        `                   (display "${INFO_MARKER}location\\t")`,
        '                   (display loc)',
        '                   (newline))))',
        '              (else',
        `               (display "${INFO_MARKER}kind\\tvalue\\n")`,
        `               (display "${INFO_MARKER}name\\t${escapedSym}\\n")`,
        `               (display "${INFO_MARKER}type\\t")`,
        '               (display (type-of val))',
        '               (newline)',
        `               (display "${INFO_MARKER}value\\t")`,
        '               (write val)',
        '               (newline))))))',
        // Apropos output after all markers, with a clear delimiter
        `      (display "\\n${APROPOS_MARKER}\\n")`,
        `      (##apropos "${escapedSym}"))))`,
      ].join(' ');

      exprs.push(schemeExpr);

      const result = await runGxi(exprs);

      if (result.timedOut) {
        return {
          content: [
            { type: 'text' as const, text: 'Symbol lookup timed out.' },
          ],
          isError: true,
        };
      }

      if (result.exitCode !== 0 && result.stderr) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Error:\n${result.stderr.trim()}`,
            },
          ],
          isError: true,
        };
      }

      const stdout = result.stdout;

      // Check for top-level error
      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        return {
          content: [
            {
              type: 'text' as const,
              text: `Error looking up "${symbol}":\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse info lines (only from the pre-apropos section)
      const aproposIdx = stdout.indexOf(APROPOS_MARKER);
      const infoSection = aproposIdx !== -1 ? stdout.slice(0, aproposIdx) : stdout;
      const aproposSection = aproposIdx !== -1
        ? stdout.slice(aproposIdx + APROPOS_MARKER.length).trim()
        : '';

      const infoLines = infoSection
        .split('\n')
        .filter((l) => l.startsWith(INFO_MARKER));

      if (infoLines.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No information found for symbol "${symbol}".`,
            },
          ],
        };
      }

      const info: Record<string, string> = {};
      for (const line of infoLines) {
        const payload = line.slice(INFO_MARKER.length);
        const tabIdx = payload.indexOf('\t');
        if (tabIdx === -1) continue;
        const key = payload.slice(0, tabIdx);
        const val = payload.slice(tabIdx + 1).trim();
        info[key] = val;
      }

      // Format output
      const sections: string[] = [`Symbol: ${symbol}`, ''];

      if (info['kind']) sections.push(`Kind: ${info['kind']}`);
      if (info['name']) sections.push(`Qualified name: ${info['name']}`);
      if (info['arity']) sections.push(`Arity: ${info['arity']}`);
      if (info['type']) sections.push(`Type: ${info['type']}`);
      if (info['value']) sections.push(`Value: ${info['value']}`);
      if (info['location']) sections.push(`Location: ${info['location']}`);
      if (module_path) sections.push(`Module: ${module_path}`);

      if (aproposSection) {
        sections.push('');
        sections.push('Related symbols:');
        sections.push(aproposSection);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
