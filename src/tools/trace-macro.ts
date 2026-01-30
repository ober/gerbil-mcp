import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

const STEP_MARKER = 'GERBIL-MCP-STEP:';

export function registerTraceMacroTool(server: McpServer): void {
  server.registerTool(
    'gerbil_trace_macro',
    {
      title: 'Trace Macro Expansion',
      description:
        'Show step-by-step macro expansion of a Gerbil expression using core-expand1 iteratively. ' +
        'Each step shows one level of expansion, from the original sugar form down to core forms (%#if, %#call, etc.). ' +
        'Useful for understanding what macros like when, if-let, defstruct, etc. actually expand to. ' +
        'Example: "(when #t (displayln 42))" shows when→if→%#if step by step.',
      inputSchema: {
        expression: z
          .string()
          .describe('The Gerbil expression to trace expansion of'),
        imports: z
          .array(z.string())
          .optional()
          .describe(
            'Module paths to import for macro definitions (e.g. [":std/sugar"])',
          ),
        max_steps: z
          .number()
          .optional()
          .describe(
            'Maximum number of expansion steps (default: 10)',
          ),
      },
    },
    async ({ expression, imports, max_steps }) => {
      const escaped = escapeSchemeString(expression);
      const maxSteps = max_steps ?? 10;

      const exprs: string[] = [];

      // Import expander and any requested modules
      exprs.push('(import :gerbil/expander)');
      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      // Build the trace loop
      const traceExpr = [
        '(with-catch',
        '  (lambda (e)',
        `    (display "${ERROR_MARKER}\\n")`,
        '    (display-exception e (current-output-port)))',
        '  (lambda ()',
        `    (let ((input-stx (read (open-input-string "${escaped}"))))`,
        // Show the original expression as step 0
        `      (display "${STEP_MARKER}0\\t")`,
        '      (write (syntax->datum input-stx))',
        '      (newline)',
        `      (let loop ((stx input-stx) (step 1))`,
        `        (when (<= step ${maxSteps})`,
        '          (let ((expanded (core-expand1 stx)))',
        '            (let ((prev-datum (syntax->datum stx))',
        '                  (expanded-datum (syntax->datum expanded)))',
        '              (unless (equal? prev-datum expanded-datum)',
        `                (display "${STEP_MARKER}")`,
        '                (display step)',
        '                (display "\\t")' ,
        '                (write expanded-datum)',
        '                (newline)',
        '                (loop expanded (+ step 1))))))))))',
      ].join(' ');

      exprs.push(traceExpr);

      const result = await runGxi(exprs);

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Macro trace timed out after 30 seconds.',
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
              text: `Error during macro trace:\n${result.stderr.trim()}`,
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
              text: `Error tracing macro expansion:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse step lines
      const stepLines = stdout
        .split('\n')
        .filter((l) => l.startsWith(STEP_MARKER));

      if (stepLines.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'No expansion steps produced.',
            },
          ],
        };
      }

      const steps = stepLines.map((line) => {
        const payload = line.slice(STEP_MARKER.length);
        const tabIdx = payload.indexOf('\t');
        const stepNum = payload.slice(0, tabIdx);
        const form = payload.slice(tabIdx + 1);
        return { step: stepNum, form };
      });

      const formatted = [
        `Macro expansion trace for: ${expression}`,
        `(${steps.length} steps)`,
        '',
        ...steps.map((s) => {
          const label = s.step === '0' ? 'input' : `step ${s.step}`;
          return `  [${label}]  ${s.form}`;
        }),
      ].join('\n');

      return { content: [{ type: 'text' as const, text: formatted }] };
    },
  );
}
