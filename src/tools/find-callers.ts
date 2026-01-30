import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-CALLER:';

export function registerFindCallersTool(server: McpServer): void {
  server.registerTool(
    'gerbil_find_callers',
    {
      title: 'Find Symbol Callers',
      description:
        'Find all files that reference a given symbol. Recursively scans .ss files ' +
        'in a directory for occurrences and reports file paths with line numbers. ' +
        'Optionally verifies that the file imports the expected module.',
      inputSchema: {
        symbol: z.string().describe('Symbol name to find usages of'),
        directory: z
          .string()
          .describe('Directory to search in (absolute path)'),
        module_path: z
          .string()
          .optional()
          .describe(
            'Module the symbol comes from, for import verification (e.g. ":std/text/json")',
          ),
      },
    },
    async ({ symbol, directory, module_path }) => {
      const escapedSym = escapeSchemeString(symbol);
      const escapedDir = escapeSchemeString(directory);
      const escapedMod = module_path
        ? escapeSchemeString(module_path)
        : '';

      const expr = buildFindCallersExpr(
        escapedSym,
        escapedDir,
        escapedMod,
      );
      const result = await runGxi([expr], { timeout: 60_000 });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Search timed out after 60 seconds. Try a smaller directory.',
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
              text: `Error searching for ${symbol}:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse GERBIL-MCP-CALLER:filepath\tline1,line2,...
      const callerLines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      if (callerLines.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No references to "${symbol}" found in ${directory}.`,
            },
          ],
        };
      }

      const sections: string[] = [
        `References to "${symbol}" (${callerLines.length} file${callerLines.length === 1 ? '' : 's'}):`,
        '',
      ];

      for (const line of callerLines) {
        const payload = line.slice(RESULT_MARKER.length);
        const tabIdx = payload.indexOf('\t');
        if (tabIdx !== -1) {
          const filePath = payload.slice(0, tabIdx);
          const lineNums = payload
            .slice(tabIdx + 1)
            .replace(/,$/, '')
            .trim();
          sections.push(`  ${filePath}`);
          if (lineNums) {
            sections.push(`    lines: ${lineNums}`);
          }
        } else {
          sections.push(`  ${payload.trim()}`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

function buildFindCallersExpr(
  escapedSym: string,
  escapedDir: string,
  escapedMod: string,
): string {
  // Build the optional import-check clause
  const importCheck = escapedMod
    ? [
        `(let ((mod-str "${escapedMod}"))`,
        '  (or (string-contains content mod-str)',
        `      (string-contains content (let ((s mod-str)) (if (string-prefix? ":" s) (substring s 1 (string-length s)) s)))))`,
      ].join(' ')
    : '#t';

  return [
    '(with-catch',
    '  (lambda (e)',
    `    (display "${ERROR_MARKER}\\n")`,
    '    (display-exception e (current-output-port)))',
    '  (lambda ()',
    `    (let ((target "${escapedSym}")`,
    `          (root "${escapedDir}"))`,
    '      (define (scan-dir dir)',
    '        (for-each',
    '          (lambda (entry)',
    '            (let ((path (path-expand entry dir)))',
    '              (with-catch (lambda (e) (void))',
    '                (lambda ()',
    '                  (cond',
    '                    ((string-prefix? "." entry) (void))',
    '                    ((file-info-directory? (file-info path #t))',
    '                     (scan-dir path))',
    '                    ((string-suffix? ".ss" entry)',
    '                     (scan-file path)))))))',
    '          (directory-files dir)))',
    '      (define (scan-file path)',
    '        (with-catch (lambda (e) (void))',
    '          (lambda ()',
    '            (let ((content (call-with-input-file path',
    '                    (lambda (p)',
    '                      (let loop ((acc ""))',
    '                        (let ((line (read-line p)))',
    '                          (if (eof-object? line) acc',
    '                            (loop (string-append acc line "\\n")))))))))',
    '              (when (and (string-contains content target)',
    `                         ${importCheck})`,
    // Find line numbers
    '                (let ((line-nums',
    '                       (call-with-input-file path',
    '                         (lambda (p)',
    '                           (let loop ((line (read-line p)) (n 1) (matches (list)))',
    '                             (if (eof-object? line)',
    '                               (reverse matches)',
    '                               (loop (read-line p) (+ n 1)',
    '                                 (if (string-contains line target)',
    '                                   (cons n matches) matches))))))))',
    `                  (display "${RESULT_MARKER}")`,
    '                  (display path)',
    '                  (display "\\t")',
    '                  (for-each (lambda (n) (display n) (display ",")) line-nums)',
    '                  (newline)))))))',
    '      (scan-dir root))))',
  ].join(' ');
}
