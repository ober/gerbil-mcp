import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString } from '../gxi.js';

export function registerTestFixtureGenTool(server: McpServer): void {
  server.registerTool(
    'gerbil_test_fixture_gen',
    {
      title: 'Test Fixture Generator',
      description:
        'Generate test fixtures and mock modules for :std/test. Given a module path, ' +
        'produces mock implementations, factory functions for test instances, and ' +
        'test setup/teardown patterns with parameterize for dependency injection.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        module_path: z
          .string()
          .describe('Module to generate test fixtures for (e.g. :std/db/dbi)'),
        symbols: z
          .array(z.string())
          .optional()
          .describe('Specific symbols to mock (default: all exported procedures)'),
      },
    },
    async ({ module_path, symbols }) => {
      // Get module exports
      const escapedMod = escapeSchemeString(module_path);
      let exports: Array<{ name: string; kind: string }> = [];

      try {
        const exprs = [
          '(import :gerbil/expander)',
          '(import :std/misc/repr)',
          `(import ${module_path})`,
          [
            `(let* ((mod (import-module (quote ${module_path}) #t #t))`,
            '       (export-list (module-context-export mod))',
            '       (names (map module-export-name export-list)))',
            '  (for-each (lambda (name)',
            '    (with-catch (lambda (e) #f)',
            '      (lambda ()',
            '        (let ((resolved (eval name)))',
            '          (cond ((procedure? resolved)',
            '                 (display (repr (list name "procedure"))) (newline))',
            '                ((boolean? resolved)',
            '                 (display (repr (list name "value"))) (newline))',
            '                (else (display (repr (list name "value"))) (newline)))))))',
            '    names))',
          ].join(' '),
        ];
        const result = await runGxi(exprs, { timeout: 10000 });
        const lines = result.stdout.split('\n').filter((l) => l.startsWith('('));
        for (const line of lines) {
          const match = line.match(/\((\S+)\s+"(\w+)"\)/);
          if (match) {
            exports.push({ name: match[1], kind: match[2] });
          }
        }
      } catch { /* fallback to empty */ }

      // Filter to requested symbols
      if (symbols && symbols.length > 0) {
        const symbolSet = new Set(symbols);
        exports = exports.filter((e) => symbolSet.has(e.name));
      }

      const procs = exports.filter((e) => e.kind === 'procedure');
      const values = exports.filter((e) => e.kind === 'value');

      const sections: string[] = [
        `## Test Fixture Generator: ${module_path}`,
        '',
        `Exports found: ${exports.length} (${procs.length} procedures, ${values.length} values)`,
        '',
        '### Mock Module',
        '',
        '```scheme',
        `;;; mock-${module_path.replace(/:/g, '').replace(/\//g, '-')}.ss`,
        `;;; Mock implementation of ${module_path} for testing`,
        '',
        '(export',
      ];

      for (const proc of procs) {
        sections.push(`  ${proc.name}`);
      }
      sections.push(')');
      sections.push('');

      // Generate mock implementations
      for (const proc of procs) {
        const name = proc.name;
        sections.push(`;;; Mock: ${name}`);
        if (name.includes('make-') || name.includes('create')) {
          // Constructor — return a hash table
          sections.push(`(def (${name} . args)`);
          sections.push(`  (hash ("_mock" '${name}) ("args" args)))`);
        } else if (name.endsWith('?')) {
          // Predicate — return #t
          sections.push(`(def (${name} . args) #t)`);
        } else if (name.endsWith('!') || name.startsWith('set-')) {
          // Mutator — return void
          sections.push(`(def (${name} . args) (void))`);
        } else if (name.startsWith('get-') || name.includes('-ref')) {
          // Accessor — return a default value
          sections.push(`(def (${name} . args) #f)`);
        } else {
          // Generic — return #f
          sections.push(`(def (${name} . args) #f)`);
        }
        sections.push('');
      }
      sections.push('```');

      // Test setup with parameterize
      sections.push('');
      sections.push('### Test Setup with Parameterize');
      sections.push('');
      sections.push('```scheme');
      sections.push(';;; test-setup.ss — Dependency injection via parameterize');
      sections.push('(import :std/test');
      sections.push(`        ${module_path})`);
      sections.push('');
      sections.push(';;; Define parameters for dependency injection');

      for (const proc of procs.slice(0, 5)) {
        sections.push(`(def current-${proc.name} (make-parameter ${proc.name}))`);
      }

      sections.push('');
      sections.push(';;; Test with mocked dependencies');
      sections.push('(def test-suite');
      sections.push('  (test-suite "mock tests"');
      sections.push('    (test-case "with mock"');
      sections.push('      (parameterize');
      sections.push('        (');
      for (const proc of procs.slice(0, 3)) {
        sections.push(`         (current-${proc.name} (lambda args \'mock-result))`);
      }
      sections.push('        )');
      sections.push(`        ;; Test code here — uses (current-${procs[0]?.name || 'fn'}) instead of direct calls`);
      sections.push('        (check-equal? ((current-' + (procs[0]?.name || 'fn') + ')) \'mock-result)');
      sections.push('      ))))');
      sections.push('```');

      // Factory functions
      if (procs.some((p) => p.name.includes('make-') || p.name.includes('create'))) {
        sections.push('');
        sections.push('### Factory Functions');
        sections.push('');
        sections.push('```scheme');
        sections.push(';;; Test data factories');
        for (const proc of procs.filter((p) => p.name.includes('make-') || p.name.includes('create'))) {
          sections.push(`(def (test-${proc.name} . overrides)`);
          sections.push(`  ;; Create a test instance with sensible defaults`);
          sections.push(`  (apply ${proc.name} overrides))`);
          sections.push('');
        }
        sections.push('```');
      }

      sections.push('');
      sections.push('**Tip**: Use `gerbil_function_signature` to check exact arities for mock implementations.');

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
