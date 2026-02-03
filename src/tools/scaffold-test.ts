import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-SCAFFOLD-TEST:';

export function registerScaffoldTestTool(server: McpServer): void {
  server.registerTool(
    'gerbil_scaffold_test',
    {
      title: 'Scaffold Test File',
      description:
        'Generate a :std/test skeleton from a module\'s exports. ' +
        'Introspects the module to discover exported procedures, macros, and values, ' +
        'then produces a ready-to-fill test file. Does not write to disk — returns the generated text.',
      inputSchema: {
        module_path: z
          .string()
          .describe(
            'Module to generate tests for (e.g. ":std/text/json", ":myproject/handler")',
          ),
        suite_name: z
          .string()
          .optional()
          .describe(
            'Override the test suite name (default: derived from module path)',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution',
          ),
      },
    },
    async ({ module_path, suite_name, loadpath }) => {
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

      // Derive suite name from module path
      const derivedSuiteName =
        suite_name ?? deriveSuiteName(modPath);

      // Derive display name for test-suite string
      const displayName = modPath.startsWith(':')
        ? modPath.slice(1)
        : modPath;

      if (lines.length === 0) {
        // Module has no exports — generate minimal skeleton
        const skeleton = [
          `(import :std/test ${modPath})`,
          `(export ${derivedSuiteName})`,
          '',
          `(def ${derivedSuiteName}`,
          `  (test-suite "${escapeSchemeString(displayName)}"`,
          `    ;; No exports found in ${modPath}`,
          `    ))`,
        ].join('\n');

        return {
          content: [{ type: 'text' as const, text: skeleton }],
        };
      }

      const entries = lines.map((line) => {
        const parts = line.slice(RESULT_MARKER.length).split('\t');
        return {
          name: parts[0] || '',
          kind: parts[1] || '',
          arity: parseInt(parts[2] || '0', 10),
        };
      });

      // Generate test cases
      const testCases: string[] = [];
      for (const entry of entries) {
        if (entry.kind === 'procedure') {
          const argPlaceholders =
            entry.arity > 0
              ? ' ' + Array.from({ length: entry.arity }, (_, i) => `arg${i + 1}`).join(' ')
              : '';
          testCases.push(
            `    (test-case "${escapeSchemeString(entry.name)}"`,
            `      (check (${entry.name}${argPlaceholders}) => ...))`,
          );
        } else if (entry.kind === 'macro') {
          testCases.push(
            `    (test-case "${escapeSchemeString(entry.name)}"`,
            `      (check (${entry.name} ...) => ...))`,
          );
        } else {
          // value
          testCases.push(
            `    (test-case "${escapeSchemeString(entry.name)}"`,
            `      (check ${entry.name} ? values))`,
          );
        }
      }

      const testFile = [
        `(import :std/test ${modPath})`,
        `(export ${derivedSuiteName})`,
        '',
        `(def ${derivedSuiteName}`,
        `  (test-suite "${escapeSchemeString(displayName)}"`,
        ...testCases,
        `    ))`,
      ].join('\n');

      return {
        content: [{ type: 'text' as const, text: testFile }],
      };
    },
  );
}

function deriveSuiteName(modulePath: string): string {
  // :std/text/json → json-test
  // :myproject/handler → handler-test
  const path = modulePath.startsWith(':')
    ? modulePath.slice(1)
    : modulePath;
  const segments = path.split('/');
  const last = segments[segments.length - 1] || 'module';
  return `${last}-test`;
}
