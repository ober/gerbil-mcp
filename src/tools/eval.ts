import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, RESULT_MARKER, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

export function registerEvalTool(server: McpServer): void {
  server.registerTool(
    'gerbil_eval',
    {
      title: 'Evaluate Gerbil Expression',
      description:
        'Evaluate a Gerbil Scheme expression using gxi and return the result. ' +
        'Use the imports parameter to make module bindings available. ' +
        'Example: expression "(json-object->string (hash (\\"a\\" 1)))" with imports [":std/text/json"].',
      inputSchema: {
        expression: z.string().describe('The Gerbil Scheme expression to evaluate'),
        imports: z
          .array(z.string())
          .optional()
          .describe(
            'Module paths to import before evaluation (e.g. [":std/text/json", ":std/iter"])',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution ' +
            '(e.g. ["/path/to/project/.gerbil/lib"])',
          ),
      },
    },
    async ({ expression, imports, loadpath }) => {
      const escaped = escapeSchemeString(expression);

      const exprs: string[] = [];

      // Import modules
      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      // Eval wrapper with error handling
      const wrapper = [
        '(with-catch',
        '  (lambda (e)',
        `    (display "${ERROR_MARKER}\\n")`,
        '    (display-exception e (current-output-port)))',
        '  (lambda ()',
        `    (let ((val (eval (read (open-input-string "${escaped}")))))`
        ,
        '      (unless (void? val)',
        `        (display "${RESULT_MARKER}")`,
        '        (write val)',
        '        (newline)))))',
      ].join(' ');

      exprs.push(wrapper);

      const env = loadpath && loadpath.length > 0 ? buildLoadpathEnv(loadpath) : undefined;
      const result = await runGxi(exprs, { env });

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Evaluation timed out after 30 seconds.' }],
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
        const sideOutput = stdout.slice(0, errorIdx).trim();
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        const parts: string[] = [];
        if (sideOutput) parts.push(`Output:\n${sideOutput}`);
        parts.push(`Error:\n${errorMsg}`);
        return {
          content: [{ type: 'text' as const, text: parts.join('\n\n') }],
          isError: true,
        };
      }

      // Check for result marker
      const resultIdx = stdout.indexOf(RESULT_MARKER);
      if (resultIdx !== -1) {
        const sideOutput = stdout.slice(0, resultIdx).trim();
        const value = stdout.slice(resultIdx + RESULT_MARKER.length).trim();
        const parts: string[] = [];
        if (sideOutput) parts.push(`Output:\n${sideOutput}`);
        if (value) parts.push(`Result: ${value}`);
        return {
          content: [{ type: 'text' as const, text: parts.join('\n\n') || '(void)' }],
        };
      }

      // No markers — expression returned void but may have produced output
      const output = stdout.trim();
      return {
        content: [{ type: 'text' as const, text: output || '(void)' }],
      };
    },
  );
}
