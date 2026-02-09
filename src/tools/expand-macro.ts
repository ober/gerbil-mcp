import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

export function registerExpandMacroTool(server: McpServer): void {
  server.registerTool(
    'gerbil_expand_macro',
    {
      title: 'Expand Gerbil Macro',
      description:
        'Show the macro expansion of a Gerbil Scheme expression. ' +
        'Returns the core-expanded form using syntax->datum. ' +
        'Example: "(when #t (displayln \\"hi\\"))" expands to a %#if form.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        expression: z.string().describe('The Gerbil expression to expand'),
        imports: z
          .array(z.string())
          .optional()
          .describe('Modules to import for macro definitions'),
      },
    },
    async ({ expression, imports }) => {
      const escaped = escapeSchemeString(expression);

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
        `    (let ((expanded (syntax->datum (core-expand (read (open-input-string "${escaped}"))))))`,
        '      (write expanded)',
        '      (newline))))',
      ].join(' ');

      exprs.push(wrapper);

      const result = await runGxi(exprs);

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Macro expansion timed out.' }],
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
      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        return {
          content: [{ type: 'text' as const, text: `Expansion error:\n${errorMsg}` }],
          isError: true,
        };
      }

      const expanded = stdout.trim();
      if (!expanded) {
        return {
          content: [{ type: 'text' as const, text: 'Expression expanded to void.' }],
        };
      }

      return {
        content: [{ type: 'text' as const, text: `Expanded form:\n${expanded}` }],
      };
    },
  );
}
