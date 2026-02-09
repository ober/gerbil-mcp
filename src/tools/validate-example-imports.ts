/**
 * gerbil_validate_example_imports — Validate that imported modules actually export
 * the symbols used in example/doc files. Hybrid static+runtime analysis.
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { runGxi, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';
import {
  parseDefinitions,
  extractModulePaths,
  extractCallSites,
  scanSchemeFiles,
} from './parse-utils.js';

const RESULT_MARKER = 'GERBIL-MCP-VEI:';

/** Common Scheme/Gerbil builtins that don't come from any import */
const BUILTINS = new Set([
  // Core forms
  'def', 'def*', 'define', 'lambda', 'let', 'let*', 'letrec', 'letrec*',
  'if', 'cond', 'when', 'unless', 'and', 'or', 'not', 'begin', 'do',
  'set!', 'quote', 'quasiquote', 'unquote', 'unquote-splicing',
  'case', 'match', 'with', 'try', 'catch', 'finally',
  // Type constructors / predicates
  'cons', 'car', 'cdr', 'list', 'vector', 'string', 'hash',
  'pair?', 'null?', 'list?', 'number?', 'string?', 'boolean?', 'symbol?',
  'vector?', 'procedure?', 'void?', 'eof-object?',
  'equal?', 'eq?', 'eqv?',
  // Arithmetic
  '+', '-', '*', '/', '<', '>', '<=', '>=', '=', 'zero?',
  'min', 'max', 'abs', 'modulo', 'remainder', 'quotient',
  'exact->inexact', 'inexact->exact', 'number->string', 'string->number',
  // String ops
  'string-append', 'string-length', 'string-ref', 'substring',
  'string=?', 'string<?', 'string>?',
  // List ops
  'map', 'filter', 'for-each', 'apply', 'append', 'reverse', 'length',
  'fold', 'foldl', 'foldr', 'assoc', 'assq', 'member', 'memq',
  'car', 'cdr', 'caar', 'cadr', 'cdar', 'cddr',
  // I/O
  'display', 'displayln', 'write', 'read', 'newline', 'print', 'println',
  'read-line', 'read-char', 'write-char',
  'open-input-file', 'open-output-file', 'close-input-port', 'close-output-port',
  'call-with-input-file', 'call-with-output-file',
  'call-with-input-string', 'call-with-output-string',
  'open-input-string', 'open-output-string', 'get-output-string',
  'current-input-port', 'current-output-port', 'current-error-port',
  'eof-object',
  // Control
  'values', 'call-with-values', 'call-with-current-continuation', 'call/cc',
  'dynamic-wind', 'parameterize',
  // Errors
  'error', 'raise', 'with-catch', 'ignore-errors',
  'error-message', 'error-exception?',
  // Hash
  'make-hash-table', 'hash-ref', 'hash-get', 'hash-put!', 'hash-set!',
  'hash-remove!', 'hash-key?', 'hash->list', 'hash-merge',
  // Misc
  'void', 'gensym', 'object->string', 'eval',
  'import', 'export', 'require',
  'spawn', 'spawn/name', 'thread-join!', 'thread-sleep!',
  'make-mutex', 'mutex-lock!', 'mutex-unlock!',
  // Struct/class
  'defstruct', 'defclass', 'defmethod', 'defrules', 'defsyntax',
  'make-hash-table-eq', 'hash-eq',
  // Boolean
  '#t', '#f', 'else',
]);

export function registerValidateExampleImportsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_validate_example_imports',
    {
      title: 'Validate Example Imports',
      description:
        'Validate that imported modules actually export the symbols used in a Gerbil source file. ' +
        'Detects symbols used but not available from any import or local definition.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Single .ss file to validate.'),
        directory: z
          .string()
          .optional()
          .describe('Directory to scan for .ss files (recursive).'),
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
    async ({ file_path, directory, loadpath, project_path }) => {
      if (!file_path && !directory) {
        return {
          content: [
            { type: 'text' as const, text: 'Either file_path or directory must be provided.' },
          ],
          isError: true,
        };
      }

      // Build loadpath
      const finalLoadpath = project_path
        ? [join(project_path, '.gerbil/lib'), ...(loadpath || [])]
        : loadpath;
      const env = finalLoadpath ? buildLoadpathEnv(finalLoadpath) : undefined;

      // Gather files
      let filePaths: string[] = [];
      if (file_path) {
        filePaths.push(file_path);
      }
      if (directory) {
        filePaths.push(...await scanSchemeFiles(directory));
      }

      if (filePaths.length === 0) {
        return {
          content: [{ type: 'text' as const, text: 'No .ss files found.' }],
        };
      }

      const allSections: string[] = [];
      let totalIssues = 0;

      for (const fp of filePaths) {
        let content: string;
        try {
          content = await readFile(fp, 'utf-8');
        } catch {
          continue;
        }

        const analysis = parseDefinitions(content);
        const modPaths = new Set<string>();
        for (const imp of analysis.imports) {
          for (const mp of extractModulePaths(imp.raw)) {
            modPaths.add(mp.startsWith(':') ? mp : `:${mp}`);
          }
        }

        // Get exports from each imported module
        const availableSymbols = new Set<string>(BUILTINS);

        // Add local definitions
        for (const def of analysis.definitions) {
          availableSymbols.add(def.name);
        }

        // Query runtime for each module's exports
        for (const modPath of modPaths) {
          const exprs = [
            `(import :gerbil/expander)`,
            `(with-catch (lambda (e) (void))`,
            `  (lambda ()`,
            `    (import ${modPath})`,
            `    (let* ((mod (import-module (quote ${modPath}) #f #t))`,
            '           (exports (module-context-export mod)))',
            '      (for-each',
            '        (lambda (e)',
            `          (display "${RESULT_MARKER}")`,
            '          (display (module-export-name e))',
            '          (newline))',
            '        exports))))',
          ];

          const result = await runGxi(exprs, { env, timeout: 10_000 });
          if (result.exitCode === 0) {
            const names = result.stdout
              .split('\n')
              .filter((l) => l.startsWith(RESULT_MARKER))
              .map((l) => l.slice(RESULT_MARKER.length).trim())
              .filter((n) => n.length > 0);
            for (const n of names) {
              availableSymbols.add(n);
            }
          }
        }

        // Extract all call sites from the file
        const callSites = extractCallSites(content);
        const usedSymbols = new Set(callSites.map((cs) => cs.symbol));

        // Find symbols used but not available
        const missing: string[] = [];
        for (const sym of usedSymbols) {
          if (!availableSymbols.has(sym)) {
            missing.push(sym);
          }
        }

        if (missing.length > 0) {
          totalIssues += missing.length;
          allSections.push(`\n${fp}:`);
          allSections.push(`  Imports: ${Array.from(modPaths).join(' ')}`);
          allSections.push(`  Potentially undefined symbols (${missing.length}):`);
          for (const sym of missing.sort()) {
            allSections.push(`    - ${sym}`);
          }
        }
      }

      if (totalIssues === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Validated ${filePaths.length} file(s) — no import issues found.`,
            },
          ],
        };
      }

      const header = `Validated ${filePaths.length} file(s) — ${totalIssues} potential issue(s):`;
      return {
        content: [
          { type: 'text' as const, text: header + '\n' + allSections.join('\n') },
        ],
      };
    },
  );
}
