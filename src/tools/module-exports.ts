import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, ERROR_MARKER } from '../gxi.js';

export function registerModuleExportsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_module_exports',
    {
      title: 'List Module Exports',
      description:
        'List all exported symbols from a Gerbil module. ' +
        'Example: module_path ":std/text/json" returns read-json, write-json, etc.',
      inputSchema: {
        module_path: z
          .string()
          .describe('Module path (e.g. ":std/text/json", ":std/sugar", ":std/iter")'),
      },
    },
    async ({ module_path }) => {
      const modPath = module_path.startsWith(':') ? module_path : `:${module_path}`;

      const exprs = [
        '(import :gerbil/expander)',
        [
          '(with-catch',
          '  (lambda (e)',
          `    (display "${ERROR_MARKER}\\n")`,
          '    (display-exception e (current-output-port)))',
          '  (lambda ()',
          `    (let ((mod (import-module (quote ${modPath}) #f #t)))`,
          '      (let ((exports (module-context-export mod)))',
          '        (for-each',
          '          (lambda (e)',
          '            (displayln (module-export-name e)))',
          '          exports)))))',
        ].join(' '),
      ];

      const result = await runGxi(exprs);

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Module introspection timed out.' }],
          isError: true,
        };
      }

      if (result.exitCode !== 0 && result.stderr) {
        return {
          content: [
            { type: 'text' as const, text: `Failed to load module ${modPath}:\n${result.stderr.trim()}` },
          ],
          isError: true,
        };
      }

      const stdout = result.stdout;
      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        return {
          content: [{ type: 'text' as const, text: `Error introspecting ${modPath}:\n${errorMsg}` }],
          isError: true,
        };
      }

      const symbols = stdout
        .trim()
        .split('\n')
        .map((s) => s.trim())
        .filter(Boolean);

      if (symbols.length === 0) {
        return {
          content: [{ type: 'text' as const, text: `Module ${modPath} exports no symbols (or could not be introspected).` }],
        };
      }

      const formatted = [
        `Module ${modPath} exports ${symbols.length} symbol(s):`,
        '',
        ...symbols.map((s) => `  ${s}`),
      ].join('\n');

      return {
        content: [{ type: 'text' as const, text: formatted }],
      };
    },
  );
}
