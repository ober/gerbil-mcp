import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join } from 'node:path';
import { runGxi, escapeSchemeString, RESULT_MARKER, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

export function registerPortFdInspectorTool(server: McpServer): void {
  server.registerTool(
    'gerbil_port_fd_inspector',
    {
      title: 'Inspect Port File Descriptor',
      description:
        'Extract the internal file descriptor number and properties from a Gambit port object. ' +
        'Evaluates the given expression (which must return a port), then reports: fd number, ' +
        'fd type, input/output direction, and tty status. Useful for debugging fd/port dual-layer ' +
        'issues (redirect offset divergence, pipe lifecycle, dup2 effects).',
      annotations: {
        readOnlyHint: true,
        idempotentHint: false,
      },
      inputSchema: {
        expression: z.string().describe('Gerbil expression that evaluates to a port object'),
        imports: z
          .array(z.string())
          .optional()
          .describe('Module paths to import before evaluation (e.g. [":std/os/fd"])'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Directories to add to GERBIL_LOADPATH for project-local module resolution'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib'),
        env: z
          .record(z.string())
          .optional()
          .describe('Environment variables to pass to the gxi subprocess'),
      },
    },
    async ({ expression, imports, loadpath, project_path, env: extraEnv }) => {
      const escaped = escapeSchemeString(expression);

      const exprs: string[] = [];

      // Always import :std/os/fd for fd-e and fd-type
      exprs.push('(import :std/os/fd)');

      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      const errMarker = ERROR_MARKER;
      const resMarker = RESULT_MARKER;
      const wrapper =
        '(with-catch' +
        '  (lambda (e)' +
        '    (display "' + errMarker + '\\n")' +
        '    (display-exception e (current-output-port)))' +
        '  (lambda ()' +
        '    (let ((val (eval (read (open-input-string "' + escaped + '")))))' +
        '      (if (not (port? val))' +
        '        (begin' +
        '          (display "' + errMarker + '\\n")' +
        '          (displayln "Expression did not return a port. Got: "' +
        '            (with-output-to-string (lambda () (write val)))))' +
        '        (begin' +
        '          (display "' + resMarker + '")' +
        '          (displayln (string-append "input-port: " (if (input-port? val) "#t" "#f")))' +
        '          (displayln (string-append "output-port: " (if (output-port? val) "#t" "#f")))' +
        '          (displayln (string-append "tty: " (if (with-catch (lambda (e) #f) (lambda () (tty? val))) "#t" "#f")))' +
        '          (let ((fd-num (with-catch (lambda (e) #f) (lambda () (fd-e val)))))' +
        '            (if fd-num' +
        '              (begin' +
        '                (displayln (string-append "fd: " (number->string fd-num)))' +
        '                (let ((ft (with-catch (lambda (e) #f) (lambda () (fd-type fd-num)))))' +
        '                  (when ft (displayln (string-append "fd-type: " (symbol->string ft))))))' +
        '              (displayln "fd: N/A (not a file-descriptor-backed port)"))))))))';

      exprs.push(wrapper);

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const loadpathEnv = effectiveLoadpath.length > 0 ? buildLoadpathEnv(effectiveLoadpath) : undefined;
      const env = { ...loadpathEnv, ...extraEnv };
      const result = await runGxi(exprs, { env: Object.keys(env).length > 0 ? env : undefined });

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Evaluation timed out.' }],
          isError: true,
        };
      }

      if (result.exitCode !== 0 && result.stderr) {
        return {
          content: [{ type: 'text' as const, text: 'Error:\n' + result.stderr.trim() }],
          isError: true,
        };
      }

      const stdout = result.stdout;

      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        return {
          content: [{ type: 'text' as const, text: 'Error:\n' + errorMsg }],
          isError: true,
        };
      }

      const resultIdx = stdout.indexOf(RESULT_MARKER);
      if (resultIdx !== -1) {
        const description = stdout.slice(resultIdx + RESULT_MARKER.length).trim();
        return {
          content: [{ type: 'text' as const, text: description }],
        };
      }

      return {
        content: [{ type: 'text' as const, text: stdout.trim() || '(no output)' }],
      };
    },
  );
}
