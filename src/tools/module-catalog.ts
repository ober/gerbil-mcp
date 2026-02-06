import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join } from 'node:path';
import { runGxi, escapeSchemeString, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-CATALOG:';

/**
 * Curated descriptions for well-known modules.
 * Key: "module_path:symbol_name" or just "symbol_name" for common symbols.
 */
const KNOWN_DESCRIPTIONS: Record<string, Record<string, string>> = {
  ':std/sugar': {
    'chain': 'Pipeline combinator — thread a value through transforms using <> slot',
    'is': 'Predicate factory — (is accessor predicate?) creates a composed predicate',
    'if-let': 'Conditional bind — (if-let (x expr) then else) binds x if expr is not #f',
    'when-let': 'One-armed conditional bind — (when-let ((x expr)) body ...)',
    'awhen': 'Anaphoric when — binds result of test to `it`',
    'let-hash': 'Hash table destructuring — .key for hash-ref, .?key for hash-get, .$key for string key',
    'with-id': 'Macro helper — generate identifiers from format strings',
    'with-destroy': 'Ensure cleanup — calls (destroy obj) on exit',
    'while': 'While loop — (while cond body ...)',
    'until': 'Until loop — (until cond body ...)',
    'hash': 'Hash table literal — (hash (key val) ...)',
    'hash-eq': 'Hash table with eq? — (hash-eq (key val) ...)',
    'hash-eqv': 'Hash table with eqv? — (hash-eqv (key val) ...)',
    'try': 'Exception handling — (try body (catch (pred? e) handler) (finally cleanup))',
    'defrule': 'Single-clause syntax-rules macro',
    'defrules': 'Multi-clause syntax-rules macro',
    'defmethod': 'Define method on a class',
    'cut': 'Partial application — (cut fn <> arg) creates (lambda (x) (fn x arg))',
    'assert!': 'Runtime assertion — (assert! expr) or (assert! expr message)',
    'syntax-eval': 'Evaluate syntax-time expression',
    'defsyntax-call': 'Define syntax from a transformer procedure',
  },
  ':std/iter': {
    'for': 'General iteration — (for (x iterable) body) or (for (x iterable when pred) body)',
    'for*': 'Sequential multi-binding iteration (nested loops)',
    'for/collect': 'Build a list — (for/collect (x iterable) expr) => list of results',
    'for/fold': 'Reduce — (for/fold (acc init) (x iterable) body) => accumulated value',
    'for/hash': 'Build a hash table from iteration',
    'in-range': 'Iterate over integer range — (in-range start end) or (in-range start end step)',
    'in-iota': 'Iterate over iota sequence — (in-iota count start step)',
    'in-naturals': 'Infinite iterator over natural numbers from start',
    'in-hash': 'Iterate over hash key-value pairs — (for ((k v) (in-hash ht)))',
    'in-hash-keys': 'Iterate over hash table keys',
    'in-hash-values': 'Iterate over hash table values',
    'in-indexed': 'Iterate with index — (for ((x i) (in-indexed lst)))',
    'in-input-lines': 'Iterate over lines from an input port',
    'in-input-chars': 'Iterate over characters from an input port',
    'in-input-bytes': 'Iterate over bytes from an input port',
    'in-coroutine': 'Iterate over coroutine yields',
    'in-cothread': 'Iterate over cothread yields',
    'iter': 'Create an iterator from a collection',
    'iter-fini!': 'Finalize (close) an iterator',
    'yield': 'Yield a value from within a coroutine/generator',
  },
};

export function registerModuleCatalogTool(server: McpServer): void {
  server.registerTool(
    'gerbil_module_catalog',
    {
      title: 'Module Catalog',
      description:
        'Generate a compact catalog of all exports from a Gerbil module, showing each symbol\'s ' +
        'kind (procedure/macro/value), arity, and a brief description. ' +
        'Has curated descriptions for :std/sugar, :std/iter, and other well-known modules. ' +
        'Replaces multiple gerbil_doc calls with a single compact reference.',
      inputSchema: {
        module_path: z
          .string()
          .describe(
            'Module to catalog (e.g. ":std/sugar", ":std/iter", ":std/text/json")',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution',
          ),
        project_path: z
          .string()
          .optional()
          .describe(
            'Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib',
          ),
      },
    },
    async ({ module_path, loadpath, project_path }) => {
      const modPath = module_path.startsWith(':')
        ? module_path
        : `:${module_path}`;

      const finalLoadpath = project_path
        ? [join(project_path, '.gerbil/lib'), ...(loadpath || [])]
        : loadpath;
      const env = finalLoadpath ? buildLoadpathEnv(finalLoadpath) : undefined;

      // Introspect the module
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

      const stdout = result.stdout;
      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1 || result.exitCode !== 0) {
        const errorMsg = errorIdx !== -1
          ? stdout.slice(errorIdx + ERROR_MARKER.length).trim()
          : (result.stderr || result.stdout).trim();
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to load module ${modPath}:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      if (lines.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Module ${modPath} has no exports.`,
            },
          ],
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

      // Sort: macros first, then procedures, then values; alphabetical within
      const kindOrder: Record<string, number> = {
        macro: 0,
        procedure: 1,
        value: 2,
      };
      entries.sort((a, b) => {
        const ka = kindOrder[a.kind] ?? 3;
        const kb = kindOrder[b.kind] ?? 3;
        if (ka !== kb) return ka - kb;
        return a.name.localeCompare(b.name);
      });

      // Look up descriptions from curated data
      const descriptions = KNOWN_DESCRIPTIONS[modPath] || {};

      const sections: string[] = [
        `Module: ${modPath}`,
        `Exports: ${entries.length}`,
        '',
      ];

      let currentKind = '';
      for (const entry of entries) {
        if (entry.kind !== currentKind) {
          if (currentKind) sections.push('');
          const label =
            entry.kind === 'macro'
              ? 'Macros/Syntax'
              : entry.kind === 'procedure'
                ? 'Procedures'
                : 'Values';
          sections.push(`${label}:`);
          currentKind = entry.kind;
        }

        const desc = descriptions[entry.name];
        if (entry.kind === 'procedure') {
          const arityStr = entry.arity > 0 ? `/${entry.arity}` : '';
          sections.push(
            `  ${entry.name}${arityStr}${desc ? ` — ${desc}` : ''}`,
          );
        } else {
          sections.push(
            `  ${entry.name}${desc ? ` — ${desc}` : ''}`,
          );
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
