import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join } from 'node:path';
import { runGxi, escapeSchemeString, RESULT_MARKER, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

const STDOUT_MARKER = 'GERBIL-MCP-STDOUT:';

export function registerEvalTool(server: McpServer): void {
  server.registerTool(
    'gerbil_eval',
    {
      title: 'Evaluate Gerbil Expression',
      description:
        'Evaluate a Gerbil Scheme expression using gxi and return the result. ' +
        'Captures stdout output (displayln, etc.) separately from the return value. ' +
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
        project_path: z
          .string()
          .optional()
          .describe(
            'Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib',
          ),
      },
    },
    async ({ expression, imports, loadpath, project_path }) => {
      const escaped = escapeSchemeString(expression);

      const exprs: string[] = [];

      // Import modules
      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      // Eval wrapper with stdout capture via parameterize
      const wrapper = [
        '(with-catch',
        '  (lambda (e)',
        `    (display "${ERROR_MARKER}\\n")`,
        '    (display-exception e (current-output-port)))',
        '  (lambda ()',
        '    (let* ((capture-port (open-output-string))',
        `           (val (parameterize ((current-output-port capture-port))`,
        `                  (eval (read (open-input-string "${escaped}")))))`,
        '           (captured (get-output-string capture-port)))',
        '      (when (not (equal? captured ""))',
        `        (display "${STDOUT_MARKER}")`,
        '        (display captured)',
        `        (display "\\n"))`,
        '      (unless (void? val)',
        `        (display "${RESULT_MARKER}")`,
        '        (write val)',
        '        (newline)))))',
      ].join(' ');

      exprs.push(wrapper);

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const env = effectiveLoadpath.length > 0 ? buildLoadpathEnv(effectiveLoadpath) : undefined;
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

      // Extract captured stdout
      const stdoutIdx = stdout.indexOf(STDOUT_MARKER);
      const resultIdx = stdout.indexOf(RESULT_MARKER);

      let capturedOutput = '';
      if (stdoutIdx !== -1) {
        const afterMarker = stdout.slice(stdoutIdx + STDOUT_MARKER.length);
        // Captured output extends until RESULT_MARKER or end of string
        const endIdx = afterMarker.indexOf(RESULT_MARKER);
        capturedOutput = (endIdx !== -1
          ? afterMarker.slice(0, endIdx)
          : afterMarker
        ).trim();
      }

      // Check for result marker
      if (resultIdx !== -1) {
        const value = stdout.slice(resultIdx + RESULT_MARKER.length).trim();
        const parts: string[] = [];
        if (capturedOutput) parts.push(`Output:\n${capturedOutput}`);
        if (value) parts.push(`Result: ${value}`);
        return {
          content: [{ type: 'text' as const, text: parts.join('\n\n') || '(void)' }],
        };
      }

      // No result marker — expression returned void
      if (capturedOutput) {
        return {
          content: [{ type: 'text' as const, text: `Output:\n${capturedOutput}` }],
        };
      }

      // No markers at all — check for any raw output
      const output = stdout.trim();
      return {
        content: [{ type: 'text' as const, text: output || '(void)' }],
      };
    },
  );
}
