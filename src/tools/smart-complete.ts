import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join } from 'node:path';
import { runGxi, escapeSchemeString, RESULT_MARKER, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

export function registerSmartCompleteTool(server: McpServer): void {
  server.registerTool(
    'gerbil_smart_complete',
    {
      title: 'Smart Completion',
      description:
        'Given a partial expression context (e.g. "hash-" or "for/"), return valid completions ' +
        'from the currently imported modules. Combines apropos search with module export filtering ' +
        'to provide context-aware completions. Dramatically reduces guessing when writing Gerbil code.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        prefix: z.string().describe('The partial symbol to complete (e.g. "hash-", "for/", "string-")'),
        modules: z
          .array(z.string())
          .optional()
          .describe(
            'Module paths to scope completions to (e.g. [":std/text/json", ":std/iter"]). ' +
            'If omitted, searches all available symbols via apropos.',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Directories to add to GERBIL_LOADPATH for project-local module resolution'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib'),
      },
    },
    async ({ prefix, modules, loadpath, project_path }) => {
      const escaped = escapeSchemeString(prefix);
      const resMarker = RESULT_MARKER;
      const errMarker = ERROR_MARKER;

      const exprs: string[] = ['(import :gerbil/expander)'];

      if (modules && modules.length > 0) {
        // Scope completions to specific modules — get their exports and filter by prefix
        for (const mod of modules) {
          exprs.push(`(import ${mod})`);
        }

        const moduleExprs = modules.map((mod) => {
          const modPath = mod.startsWith(':') ? mod : `:${mod}`;
          return `(quote ${modPath})`;
        });

        exprs.push(
          [
            '(with-catch',
            '  (lambda (e)',
            `    (display "${errMarker}\\n")`,
            '    (display-exception e (current-output-port)))',
            '  (lambda ()',
            `    (display "${resMarker}\\n")`,
            '    (for-each',
            '      (lambda (mod-path)',
            '        (let ((mod (import-module mod-path #f #t)))',
            '          (let ((exports (module-context-export mod)))',
            '            (for-each',
            '              (lambda (e)',
            '                (let ((name (symbol->string (module-export-name e))))',
            `                  (when (string-prefix? "${escaped}" name)`,
            '                    (displayln (string-append name " (" (symbol->string mod-path) ")")))))',
            '              exports))))',
            `      (list ${moduleExprs.join(' ')}))))`,
          ].join(' '),
        );
      } else {
        // No modules specified — use apropos for broad search
        exprs.push(
          [
            '(with-catch',
            '  (lambda (e)',
            `    (display "${errMarker}\\n")`,
            '    (display-exception e (current-output-port)))',
            '  (lambda ()',
            `    (display "${resMarker}\\n")`,
            `    (##apropos "${escaped}")))`,
          ].join(' '),
        );
      }

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const env = effectiveLoadpath.length > 0 ? buildLoadpathEnv(effectiveLoadpath) : undefined;
      const result = await runGxi(exprs, { env });

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Completion search timed out.' }],
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
        const completions = stdout.slice(resultIdx + RESULT_MARKER.length).trim();
        if (!completions) {
          return {
            content: [{ type: 'text' as const, text: `No completions found for prefix "${prefix}".` }],
          };
        }
        const lines = completions.split('\n').filter(Boolean);
        return {
          content: [{
            type: 'text' as const,
            text: `${lines.length} completion(s) for "${prefix}":\n\n${completions}`,
          }],
        };
      }

      // Fallback for apropos output (no marker)
      const output = stdout.trim();
      if (!output) {
        return {
          content: [{ type: 'text' as const, text: `No completions found for prefix "${prefix}".` }],
        };
      }
      return {
        content: [{ type: 'text' as const, text: `Completions for "${prefix}":\n\n${output}` }],
      };
    },
  );
}
