import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER, VALID_MARKER } from '../gxi.js';

const LOC_MARKER = 'GERBIL-MCP-LOC:';

/**
 * Parse location info from check_syntax output.
 * Extracts line/column from:
 *   - LOC_MARKER prefix: "GERBIL-MCP-LOC:line:col"
 *   - Gambit "@LINE.COL" format in error text
 */
function extractLocation(errorMsg: string): { line: number | null; column: number | null; cleanMsg: string } {
  // Check for our explicit LOC_MARKER
  const locMatch = errorMsg.match(new RegExp(`^${LOC_MARKER.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}(\\d+):(\\d+)\\n`));
  if (locMatch) {
    return {
      line: parseInt(locMatch[1], 10),
      column: parseInt(locMatch[2], 10),
      cleanMsg: errorMsg.slice(locMatch[0].length),
    };
  }

  // Check for Gambit "@LINE.COL" format in error text
  const atMatch = errorMsg.match(/"[^"]*"@(\d+)\.(\d+)/);
  if (atMatch) {
    return {
      line: parseInt(atMatch[1], 10),
      column: parseInt(atMatch[2], 10),
      cleanMsg: errorMsg,
    };
  }

  return { line: null, column: null, cleanMsg: errorMsg };
}

export function registerCheckSyntaxTool(server: McpServer): void {
  server.registerTool(
    'gerbil_check_syntax',
    {
      title: 'Check Gerbil Syntax',
      description:
        'Check if Gerbil Scheme code is syntactically valid without evaluating it. ' +
        'Uses the Gerbil expander to verify the code can be expanded. ' +
        'Reports line and column numbers for reader/parser errors.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        code: z.string().describe('The Gerbil Scheme code to check'),
        imports: z
          .array(z.string())
          .optional()
          .describe('Modules to import for macro context (e.g. [":std/sugar"])'),
      },
    },
    async ({ code, imports }) => {
      const escaped = escapeSchemeString(code);

      const exprs: string[] = ['(import :gerbil/expander)'];

      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      // The error handler extracts line/column from reader errors (datum-parsing-exception)
      // by inspecting the readenv's port position. This gives precise location for
      // parse errors like unbalanced parens, bad character literals, etc.
      const wrapper = [
        '(with-catch',
        '  (lambda (e)',
        '    (let* ((re (if (RuntimeException? e) (RuntimeException-exception e) e))',
        '           (loc (with-catch (lambda (_) "")',
        '                  (lambda ()',
        '                    (if (datum-parsing-exception? re)',
        '                      (let* ((renv (datum-parsing-exception-readenv re))',
        '                             (port (##vector-ref renv 1)))',
        `                        (string-append "${LOC_MARKER}"`,
        '                          (number->string (##input-port-line port)) ":"',
        '                          (number->string (##input-port-column port)) "\\n"))',
        '                      "")))))',
        `      (display "${ERROR_MARKER}\\n")`,
        '      (display loc)',
        '      (display-exception e (current-output-port))))',
        '  (lambda ()',
        `    (core-expand (read (open-input-string "${escaped}")))`,
        `    (displayln "${VALID_MARKER}")))`,
      ].join(' ');

      exprs.push(wrapper);

      const result = await runGxi(exprs);

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Syntax check timed out.' }],
          isError: true,
        };
      }

      if (result.exitCode !== 0 && result.stderr) {
        return {
          content: [{ type: 'text' as const, text: `Error:\n${result.stderr.trim()}` }],
          isError: true,
        };
      }

      const stdout = result.stdout;

      if (stdout.includes(VALID_MARKER)) {
        return {
          content: [{ type: 'text' as const, text: 'Syntax is valid.' }],
        };
      }

      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        const { line, column, cleanMsg } = extractLocation(errorMsg);

        const locPrefix = line !== null
          ? `line ${line}, column ${column}: `
          : '';

        return {
          content: [{ type: 'text' as const, text: `Syntax error:\n${locPrefix}${cleanMsg}` }],
          isError: true,
        };
      }

      return {
        content: [{ type: 'text' as const, text: `Unexpected output:\n${stdout.trim()}` }],
        isError: true,
      };
    },
  );
}
