import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join } from 'node:path';
import { runGxi, escapeSchemeString, RESULT_MARKER, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

export function registerGenerateApiDocsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_generate_api_docs',
    {
      title: 'Generate API Documentation',
      description:
        'Generate markdown API documentation from a Gerbil module\'s exports. ' +
        'Introspects the module to discover exported procedures (with arities), ' +
        'macros, and values, producing a complete API reference document.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        module_path: z.string().describe('Module path to document (e.g. ":std/text/json", ":myproject/handler")'),
        title: z
          .string()
          .optional()
          .describe('Title for the documentation (default: derived from module path)'),
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
    async ({ module_path, title, loadpath, project_path }) => {
      const modPath = module_path.startsWith(':') ? module_path : `:${module_path}`;
      const escapedMod = escapeSchemeString(modPath);
      const resMarker = RESULT_MARKER;
      const errMarker = ERROR_MARKER;

      // Generate a combined introspection expression that returns exports with their types and arities.
      // We classify each export as PROC (procedure), MACRO (eval fails — likely syntax/macro), or VALUE.
      const expr = [
        '(import :gerbil/expander)',
        `(import ${modPath})`,
        [
          '(with-catch',
          '  (lambda (e)',
          `    (display "${errMarker}\\n")`,
          '    (display-exception e (current-output-port)))',
          '  (lambda ()',
          `    (display "${resMarker}\\n")`,
          `    (let ((mod (import-module (quote ${modPath}) #f #t)))`,
          '      (let ((exports (module-context-export mod)))',
          '        (for-each',
          '          (lambda (e)',
          '            (let* ((name (module-export-name e))',
          '                   (val (with-catch (lambda (_) \'__macro__)',
          '                          (lambda () (eval name)))))',
          '              (cond',
          '                ((eq? val \'__macro__)',
          '                 (display "MACRO|")',
          '                 (display name)',
          '                 (display "|")',
          '                 (newline))',
          '                ((procedure? val)',
          '                 (let ((arity (with-catch (lambda (_) "?")',
          '                                (lambda () (##procedure-info val)))))',
          '                   (display "PROC|")',
          '                   (display name)',
          '                   (display "|")',
          '                   (display arity)',
          '                   (newline)))',
          '                (else',
          '                 (display "VALUE|")',
          '                 (display name)',
          '                 (display "|")',
          '                 (display (with-catch (lambda (_) "?") (lambda () (with-output-to-string (lambda () (write val))))))',
          '                 (newline)))))',
          '          exports)))))',
        ].join(' '),
      ];

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const env = effectiveLoadpath.length > 0 ? buildLoadpathEnv(effectiveLoadpath) : undefined;
      const result = await runGxi(expr, { env });

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Module introspection timed out.' }],
          isError: true,
        };
      }

      const stdout = result.stdout;
      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        return {
          content: [{ type: 'text' as const, text: `Error loading module:\n${errorMsg}` }],
          isError: true,
        };
      }

      const resultIdx = stdout.indexOf(RESULT_MARKER);
      if (resultIdx === -1) {
        return {
          content: [{ type: 'text' as const, text: 'No output from module introspection.' }],
          isError: true,
        };
      }

      const lines = stdout
        .slice(resultIdx + RESULT_MARKER.length)
        .trim()
        .split('\n')
        .filter(Boolean);

      // Parse into categories
      const procedures: Array<{ name: string; info: string }> = [];
      const macros: Array<{ name: string }> = [];
      const values: Array<{ name: string; repr: string }> = [];

      for (const line of lines) {
        const parts = line.split('|');
        if (parts.length < 2) continue;
        const kind = parts[0];
        const name = parts[1];
        const extra = parts.slice(2).join('|');

        switch (kind) {
          case 'PROC':
            procedures.push({ name, info: extra });
            break;
          case 'MACRO':
            macros.push({ name });
            break;
          case 'VALUE':
            values.push({ name, repr: extra });
            break;
        }
      }

      // Generate markdown
      const docTitle = title ?? `API Reference: ${modPath}`;
      const sections: string[] = [];

      sections.push(`# ${docTitle}\n`);
      sections.push(`Module: \`${modPath}\`\n`);
      sections.push(`\`\`\`scheme\n(import ${modPath})\n\`\`\`\n`);

      const total = procedures.length + macros.length + values.length;
      sections.push(`**${total} exports**: ${procedures.length} procedures, ${macros.length} macros, ${values.length} values\n`);
      sections.push('---\n');

      if (procedures.length > 0) {
        sections.push('## Procedures\n');
        procedures.sort((a, b) => a.name.localeCompare(b.name));
        for (const p of procedures) {
          sections.push(`### \`${p.name}\`\n`);
          if (p.info && p.info !== '?') {
            sections.push(`Info: \`${p.info}\`\n`);
          }
        }
      }

      if (macros.length > 0) {
        sections.push('## Macros\n');
        macros.sort((a, b) => a.name.localeCompare(b.name));
        for (const m of macros) {
          sections.push(`### \`${m.name}\`\n`);
        }
      }

      if (values.length > 0) {
        sections.push('## Values\n');
        values.sort((a, b) => a.name.localeCompare(b.name));
        for (const v of values) {
          sections.push(`### \`${v.name}\`\n`);
          if (v.repr && v.repr !== '?') {
            const repr = v.repr.length > 200 ? v.repr.slice(0, 200) + '...' : v.repr;
            sections.push(`Value: \`${repr}\`\n`);
          }
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
