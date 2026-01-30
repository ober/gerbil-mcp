import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER, VALID_MARKER } from '../gxi.js';

export function registerCheckSyntaxTool(server: McpServer): void {
  server.registerTool(
    'gerbil_check_syntax',
    {
      title: 'Check Gerbil Syntax',
      description:
        'Check if Gerbil Scheme code is syntactically valid without evaluating it. ' +
        'Uses the Gerbil expander to verify the code can be expanded.',
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

      const wrapper = [
        '(with-catch',
        '  (lambda (e)',
        `    (display "${ERROR_MARKER}\\n")`,
        '    (display-exception e (current-output-port)))',
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
        return {
          content: [{ type: 'text' as const, text: `Syntax error:\n${errorMsg}` }],
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
