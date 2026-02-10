import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString } from '../gxi.js';

export function registerModuleQuickstartTool(server: McpServer): void {
  server.registerTool(
    'gerbil_module_quickstart',
    {
      title: 'Module Quickstart',
      description:
        'Generate a working example file that exercises a module\'s main exports. ' +
        'Introspects the module to discover exports, arities, and types, then generates ' +
        'a runnable .ss file demonstrating usage. Useful for undocumented stdlib modules.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: true,
      },
      inputSchema: {
        module_path: z
          .string()
          .describe('Module path (e.g. :std/text/json, :std/misc/concurrent-plan)'),
        max_exports: z
          .number()
          .optional()
          .describe('Maximum number of exports to include (default: 10)'),
      },
    },
    async ({ module_path, max_exports }) => {
      const limit = max_exports || 10;
      const escapedMod = escapeSchemeString(module_path);

      // Step 1: Get module exports with arity info
      const exprs = [
        '(import :gerbil/expander)',
        '(import :std/misc/repr)',
        '(import :std/sort)',
        `(import ${module_path})`,
        [
          `(let* ((mod (import-module (quote ${module_path}) #t #t))`,
          '       (export-list (module-context-export mod))',
          '       (names (map module-export-name export-list))',
          "       (results '()))",
          '  (for-each (lambda (name)',
          '    (with-catch (lambda (e) #f)',
          '      (lambda ()',
          '        (let* ((resolved (eval name))',
          '               (info (cond ((procedure? resolved)',
          '                            (let ((arity (with-catch (lambda (e) -1)',
          '                                           (lambda () (##procedure-info resolved)))))',
          '                              (list name "procedure" arity)))',
          '                           (else (list name "value" 0)))))',
          '          (set! results (cons info results))))))',
          '    names)',
          `  (let* ((sorted (sort results (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))))`,
          `         (limited (if (> (length sorted) ${limit}) (take sorted ${limit}) sorted)))`,
          '    (display "EXPORTS:\\n")',
          '    (for-each (lambda (entry) (display (repr entry)) (display "\\n")) limited)',
          '    (display "TOTAL:") (display (length results)) (display "\\n")))',
        ].join(' '),
      ];

      const result = await runGxi(exprs, { timeout: 15000 });
      const output = result.stdout + result.stderr;

      // Parse export info
      const exportLines = output.split('\n').filter((l) => l.startsWith('('));
      const total = output.match(/TOTAL:(\d+)/)?.[1] || '?';

      if (exportLines.length === 0) {
        // Fallback: simpler approach
        const simpleCode = `
(import ${module_path})
(display "Module loaded: ${module_path}\\n")
`;
        const simpleResult = await runGxi([simpleCode], { timeout: 10000 });
        const loadOk = !simpleResult.stderr.includes('ERROR');

        return {
          content: [
            {
              type: 'text' as const,
              text: `## Module Quickstart: ${module_path}\n\n` +
                `Module ${loadOk ? 'loads successfully' : 'failed to load'}.\n\n` +
                `Could not introspect exports. Use gerbil_module_exports for detailed export listing.\n\n` +
                '```scheme\n' +
                `(import ${module_path})\n` +
                `;; Use gerbil_module_exports to discover available functions\n` +
                '```',
            },
          ],
        };
      }

      // Generate example file
      const sections: string[] = [
        `## Module Quickstart: ${module_path}`,
        '',
        `Total exports: ${total} (showing top ${Math.min(exportLines.length, limit)})`,
        '',
        '```scheme',
        `;;; Quickstart example for ${module_path}`,
        `(import ${module_path})`,
        '',
      ];

      const procs: string[] = [];
      const values: string[] = [];

      for (const line of exportLines) {
        // Parse (name "type" arity)
        const match = line.match(/\((\S+)\s+"(\w+)"\s+(.+)\)/);
        if (match) {
          const name = match[1];
          const type = match[2];
          if (type === 'procedure') {
            procs.push(name);
            sections.push(`;;; ${name} — procedure`);
            sections.push(`; (${name} ...)`);
            sections.push('');
          } else {
            values.push(name);
            sections.push(`;;; ${name} — value`);
            sections.push(`; ${name}`);
            sections.push('');
          }
        }
      }

      sections.push('```');
      sections.push('');
      sections.push('### Available Procedures');
      if (procs.length > 0) {
        sections.push(procs.map((p) => `- \`${p}\``).join('\n'));
      } else {
        sections.push('None found');
      }

      if (values.length > 0) {
        sections.push('');
        sections.push('### Available Values');
        sections.push(values.map((v) => `- \`${v}\``).join('\n'));
      }

      sections.push('');
      sections.push(
        '**Tip**: Use `gerbil_function_signature` to check arities before calling, ' +
        'and `gerbil_doc` for documentation on specific exports.',
      );

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
