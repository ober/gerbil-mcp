import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

export function registerFormatTool(server: McpServer): void {
  server.registerTool(
    'gerbil_format',
    {
      title: 'Format Gerbil Code',
      description:
        'Pretty-print/format Gerbil Scheme expressions using Gambit\'s pretty-print. ' +
        'Reads all expressions from the input and returns them properly indented.',
      inputSchema: {
        code: z
          .string()
          .describe('Gerbil Scheme code to format/pretty-print'),
      },
    },
    async ({ code }) => {
      const escaped = escapeSchemeString(code);

      const wrapper = [
        '(with-catch',
        '  (lambda (e)',
        `    (display "${ERROR_MARKER}\\n")`,
        '    (display-exception e (current-output-port)))',
        '  (lambda ()',
        `    (let ((in (open-input-string "${escaped}"))`,
        '          (out (open-output-string)))',
        '      (let loop ((form (read in)) (first #t))',
        '        (if (eof-object? form)',
        '          (display (get-output-string out))',
        '          (begin',
        '            (unless first (newline out))',
        '            (pretty-print form out)',
        '            (loop (read in) #f)))))))',
      ].join(' ');

      const result = await runGxi([wrapper]);

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Format operation timed out.',
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
              text: `Format error:\n${result.stderr.trim()}`,
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
              text: `Format error:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      const formatted = stdout.trimEnd();
      if (!formatted) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'No expressions found in input.',
            },
          ],
        };
      }

      return {
        content: [{ type: 'text' as const, text: formatted }],
      };
    },
  );
}
