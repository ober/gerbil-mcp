import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-STUB:';

export function registerGenerateModuleStubTool(server: McpServer): void {
  server.registerTool(
    'gerbil_generate_module_stub',
    {
      title: 'Generate Module Stub',
      description:
        'Generate a module skeleton by introspecting an existing module\'s exports and signatures. ' +
        'Produces (def ...) stubs for procedures, (defrules ...) for macros, and (def ...) for values. ' +
        'Does not write to disk — returns the generated text.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        module_path: z
          .string()
          .describe(
            'Module to generate a stub from (e.g. ":std/text/json", ":myproject/handler")',
          ),
        package_prefix: z
          .string()
          .optional()
          .describe('Package prefix for the generated module'),
        imports: z
          .array(z.string())
          .optional()
          .describe('Additional import paths to include'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution',
          ),
      },
    },
    async ({ module_path, package_prefix, imports, loadpath }) => {
      const modPath = module_path.startsWith(':')
        ? module_path
        : `:${module_path}`;

      const env = loadpath ? buildLoadpathEnv(loadpath) : undefined;

      // Introspect the module to get exports with kind and arity
      const exprs = [
        `(import :gerbil/expander)`,
        `(import ${modPath})`,
        [
          '(with-catch',
          '  (lambda (e)',
          `    (display "${ERROR_MARKER}\\n")`,
          '    (display-exception e (current-output-port)))',
          '  (lambda ()',
          `    (let* ((mod (import-module (quote ${modPath}) #f #t))`,
          '           (exports (module-context-export mod)))',
          '      (for-each',
          '        (lambda (e)',
          '          (let ((name (module-export-name e)))',
          '            (with-catch',
          '              (lambda (ex)',
          `                (display "${RESULT_MARKER}")`,
          '                (display name)',
          '                (display "\\tmacro\\t0")',
          '                (newline))',
          '              (lambda ()',
          '                (let ((val (eval name)))',
          '                  (cond',
          '                    ((procedure? val)',
          `                     (display "${RESULT_MARKER}")`,
          '                     (display name)',
          '                     (display "\\tprocedure\\t")',
          '                     (display (##subprocedure-nb-parameters val))',
          '                     (newline))',
          '                    (else',
          `                     (display "${RESULT_MARKER}")`,
          '                     (display name)',
          '                     (display "\\tvalue\\t0")',
          '                     (newline))))))))',
          '        exports))))',
        ].join(' '),
      ];

      const result = await runGxi(exprs, { env });

      if (result.timedOut) {
        return {
          content: [
            { type: 'text' as const, text: 'Module introspection timed out.' },
          ],
          isError: true,
        };
      }

      if (result.exitCode !== 0) {
        const errorOutput = (result.stderr || result.stdout).trim();
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to load module ${modPath}:\n${errorOutput}`,
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
              text: `Error loading module ${modPath}:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse result lines
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      const entries = lines.map((line) => {
        const parts = line.slice(RESULT_MARKER.length).split('\t');
        return {
          name: parts[0] || '',
          kind: parts[1] || '',
          arity: parseInt(parts[2] || '0', 10),
        };
      });

      // Build import list
      const importPaths = [modPath];
      if (imports) {
        for (const imp of imports) {
          const normalized = imp.startsWith(':') ? imp : `:${imp}`;
          importPaths.push(normalized);
        }
      }

      // Build export list
      const exportNames = entries.map((e) => e.name);

      // Generate stubs
      const stubs: string[] = [];
      for (const entry of entries) {
        if (entry.kind === 'procedure') {
          const args =
            entry.arity > 0
              ? ' ' + Array.from({ length: entry.arity }, (_, i) => `arg${i + 1}`).join(' ')
              : '';
          stubs.push(`(def (${entry.name}${args})`);
          stubs.push(`  ...)`);
          stubs.push('');
        } else if (entry.kind === 'macro') {
          stubs.push(`(defrules ${entry.name} ()`);
          stubs.push(`  ((_ args ...)`);
          stubs.push(`   ...))`);
          stubs.push('');
        } else {
          // value
          stubs.push(`(def ${entry.name} ...)`);
          stubs.push('');
        }
      }

      // Assemble the module file
      const sections: string[] = [];

      if (package_prefix) {
        sections.push(`(package: ${package_prefix})`);
        sections.push('');
      }

      sections.push(`(import ${importPaths.join(' ')})`);

      if (exportNames.length > 0) {
        sections.push(`(export ${exportNames.join(' ')})`);
      }

      sections.push('');

      if (stubs.length > 0) {
        sections.push(...stubs);
      } else {
        sections.push(`;; No exports found in ${modPath}`);
      }

      return {
        content: [
          { type: 'text' as const, text: sections.join('\n') },
        ],
      };
    },
  );
}
