import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { runGxi, buildLoadpathEnv } from '../gxi.js';
import { parseDefinitions, extractModulePaths } from './parse-utils.js';

/**
 * Known process-related functions that are commonly used in build.ss
 * and their expected source modules.
 */
const PROCESS_FUNCTIONS: Record<string, string> = {
  'run-process': ':std/misc/process',
  'run-process/batch': ':std/misc/process',
  'open-process': ':std/os/socket', // Gambit primitive, but often needs import
  'shell-command': ':std/misc/process',
  'invoke': ':std/misc/process',
  'invoke/quiet': ':std/misc/process',
};

/**
 * Common functions used in build.ss that need specific imports.
 */
const BUILD_FUNCTIONS: Record<string, string> = {
  ...PROCESS_FUNCTIONS,
  'read-file-string': ':std/misc/ports',
  'read-all-as-string': ':std/misc/ports',
  'string-trim-eol': ':std/text/char-set',
  'path-expand': ':std/os/path',
  'path-normalize': ':std/os/path',
  'path-directory': ':std/os/path',
  'path-extension': ':std/os/path',
  'file-exists?': ':std/os/path',
  'create-directory*': ':std/os/path',
  'getenv': ':std/os/env',
  'setenv': ':std/os/env',
};

interface AuditIssue {
  symbol: string;
  line: number;
  inWithCatch: boolean;
  expectedModule: string | null;
  severity: 'error' | 'warning';
  message: string;
}

/**
 * Find function calls within with-catch blocks.
 * Returns a map of symbol name to line numbers where it appears inside with-catch.
 */
function findWithCatchCalls(content: string): Map<string, number[]> {
  const result = new Map<string, number[]>();
  const lines = content.split('\n');

  // Track with-catch nesting depth
  let withCatchDepth = 0;
  const withCatchStack: number[] = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    // Skip comments
    const commentIdx = line.indexOf(';');
    const effective = commentIdx >= 0 ? line.slice(0, commentIdx) : line;

    // Count with-catch openings
    const wcMatches = effective.match(/\(\s*with-catch\b/g);
    if (wcMatches) {
      withCatchDepth += wcMatches.length;
      for (let j = 0; j < wcMatches.length; j++) {
        withCatchStack.push(i);
      }
    }

    // If we're inside a with-catch, look for function calls
    if (withCatchDepth > 0) {
      // Find all function calls (opening paren followed by identifier)
      // But skip lambda parameter lists: (lambda (x y) ...) — the (x y) is not a call
      const callPattern = /\(\s*([a-zA-Z_!?<>=+\-*/][a-zA-Z0-9_!?<>=+\-*/.:#~]*)/g;
      let match;
      while ((match = callPattern.exec(effective)) !== null) {
        const sym = match[1];
        // Check if this is a lambda parameter list by looking at context before the match
        const before = effective.slice(0, match.index).trimEnd();
        if (before.endsWith('lambda') || before.match(/lambda\s*$/)) continue;
        // Skip single-character identifiers (likely parameters, loop vars)
        if (sym.length === 1) continue;
        // Skip special forms and common builtins
        if (['with-catch', 'lambda', 'let', 'let*', 'letrec', 'if', 'when',
          'unless', 'begin', 'cond', 'case', 'and', 'or', 'not', 'set!',
          'define', 'def', 'display', 'displayln', 'newline', 'string-append',
          'string-split', 'string-join', 'string-contains', 'string-prefix?',
          'list', 'cons', 'car', 'cdr', 'map', 'filter', 'for-each',
          'append', 'reverse', 'length', 'null?', 'pair?', 'string?',
          'number?', 'boolean?', 'symbol?', 'vector?', 'equal?', 'eq?',
          'error', 'values', 'call-with-values', 'apply', 'void',
          'import', 'export', 'quote', 'quasiquote', 'unquote',
          '#t', '#f',
        ].includes(sym)) continue;

        if (!result.has(sym)) {
          result.set(sym, []);
        }
        result.get(sym)!.push(i + 1); // 1-based line numbers
      }
    }

    // Count closing parens to track nesting (rough heuristic)
    let depth = 0;
    for (const ch of effective) {
      if (ch === '(') depth++;
      if (ch === ')') depth--;
    }
    if (depth < 0 && withCatchDepth > 0) {
      withCatchDepth = Math.max(0, withCatchDepth + depth);
    }
  }

  return result;
}

/**
 * Find all function call symbols in the entire file.
 */
function findAllCalls(content: string): Map<string, number[]> {
  const result = new Map<string, number[]>();
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const commentIdx = line.indexOf(';');
    const effective = commentIdx >= 0 ? line.slice(0, commentIdx) : line;

    const callPattern = /\(\s*([a-zA-Z_!?<>=+\-*/][a-zA-Z0-9_!?<>=+\-*/.:#~]*)/g;
    let match;
    while ((match = callPattern.exec(effective)) !== null) {
      const sym = match[1];
      if (!result.has(sym)) {
        result.set(sym, []);
      }
      result.get(sym)!.push(i + 1);
    }
  }

  return result;
}

export function registerBuildSsAuditTool(server: McpServer): void {
  server.registerTool(
    'gerbil_build_ss_audit',
    {
      title: 'Build.ss Import Audit',
      description:
        'Audit a build.ss file to detect function calls that may silently fail due to missing imports. ' +
        'Parses the file, identifies function calls (especially process-related ones like run-process), ' +
        'and cross-references against imports. Calls inside with-catch blocks are especially dangerous ' +
        'because missing imports cause unbound identifier exceptions that with-catch silently swallows, ' +
        'returning fallback values. This makes the actual bug invisible — downstream build failures ' +
        'appear as missing libraries or linker errors instead of the real cause: a missing import.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .describe('Path to the build.ss file to audit'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for module resolution',
          ),
      },
    },
    async ({ file_path, loadpath }) => {
      if (!file_path) {
        return {
          content: [
            { type: 'text' as const, text: 'The "file_path" parameter is required.' },
          ],
          isError: true,
        };
      }

      let content: string;
      try {
        content = await readFile(file_path, 'utf-8');
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            { type: 'text' as const, text: `Failed to read file: ${msg}` },
          ],
          isError: true,
        };
      }

      const analysis = parseDefinitions(content);
      const localDefs = new Set(analysis.definitions.map((d) => d.name));

      // Get all imported module paths
      const importedModPaths = new Set<string>();
      for (const imp of analysis.imports) {
        const paths = extractModulePaths(imp.raw);
        for (const p of paths) {
          importedModPaths.add(p);
        }
      }

      // Resolve imports to get exported symbols
      const env = loadpath && loadpath.length > 0
        ? buildLoadpathEnv(loadpath)
        : undefined;

      const modList = [...importedModPaths].filter((p) => !p.startsWith('./'));
      let importedSymbols = new Set<string>();

      if (modList.length > 0) {
        const exprs = [
          '(import :gerbil/expander)',
          [
            '(for-each',
            '  (lambda (mod-sym)',
            '    (with-catch',
            '      (lambda (e) (void))',
            '      (lambda ()',
            '        (let ((mod (import-module mod-sym #f #t)))',
            '          (for-each',
            '            (lambda (e) (display "SYM:") (displayln (module-export-name e)))',
            '            (module-context-export mod))))))',
            `  '(${modList.join(' ')}))`,
          ].join(' '),
        ];

        const result = await runGxi(exprs, { env, timeout: 30_000 });
        if (!result.timedOut) {
          for (const line of result.stdout.split('\n')) {
            const trimmed = line.trim();
            if (trimmed.startsWith('SYM:')) {
              const sym = trimmed.slice('SYM:'.length).trim();
              if (sym) importedSymbols.add(sym);
            }
          }
        }
      }

      // Find with-catch calls and all calls
      const withCatchCalls = findWithCatchCalls(content);
      const allCalls = findAllCalls(content);

      const issues: AuditIssue[] = [];

      // Check known build functions
      for (const [sym, expectedModule] of Object.entries(BUILD_FUNCTIONS)) {
        const wcLines = withCatchCalls.get(sym);
        const allLines = allCalls.get(sym);

        if (!allLines || allLines.length === 0) continue;

        // Is the symbol available?
        const isImported = importedSymbols.has(sym);
        const isLocal = localDefs.has(sym);
        const isAvailable = isImported || isLocal;

        if (!isAvailable) {
          const inWC = wcLines && wcLines.length > 0;
          issues.push({
            symbol: sym,
            line: allLines[0],
            inWithCatch: !!inWC,
            expectedModule,
            severity: inWC ? 'error' : 'warning',
            message: inWC
              ? `"${sym}" is used inside with-catch but NOT imported — ` +
                `the unbound identifier exception is silently caught, returning fallback value. ` +
                `Add (import ${expectedModule}) to fix.`
              : `"${sym}" is used but NOT imported. Add (import ${expectedModule}).`,
          });
        }
      }

      // Check all with-catch calls for unbound symbols (not just known ones)
      for (const [sym, lines] of withCatchCalls) {
        if (BUILD_FUNCTIONS[sym]) continue; // already checked
        const isImported = importedSymbols.has(sym);
        const isLocal = localDefs.has(sym);
        if (!isImported && !isLocal) {
          issues.push({
            symbol: sym,
            line: lines[0],
            inWithCatch: true,
            expectedModule: null,
            severity: 'warning',
            message: `"${sym}" is used inside with-catch but may not be imported. ` +
              `If unbound, the with-catch will silently swallow the error.`,
          });
        }
      }

      if (issues.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Build.ss audit: ${file_path}\n` +
                `No issues found. ${importedModPaths.size} module(s) imported, ` +
                `${importedSymbols.size} symbol(s) available.`,
            },
          ],
        };
      }

      // Sort: errors first, then by line
      issues.sort((a, b) => {
        if (a.severity !== b.severity) {
          return a.severity === 'error' ? -1 : 1;
        }
        return a.line - b.line;
      });

      const errors = issues.filter((i) => i.severity === 'error');
      const warnings = issues.filter((i) => i.severity === 'warning');

      const sections: string[] = [
        `Build.ss audit: ${file_path}`,
        `  ${errors.length} error(s), ${warnings.length} warning(s)`,
        '',
      ];

      if (errors.length > 0) {
        sections.push('SILENT FAILURES (inside with-catch — errors are swallowed):');
        sections.push('');
        for (const e of errors) {
          sections.push(`  [ERROR] line ${e.line}: ${e.message}`);
        }
        sections.push('');
      }

      if (warnings.length > 0) {
        sections.push('POTENTIAL ISSUES:');
        sections.push('');
        for (const w of warnings) {
          sections.push(`  [WARNING] line ${w.line}: ${w.message}`);
        }
        sections.push('');
      }

      // Suggest missing imports
      const missingImports = new Set<string>();
      for (const issue of issues) {
        if (issue.expectedModule && !importedModPaths.has(issue.expectedModule)) {
          missingImports.add(issue.expectedModule);
        }
      }
      if (missingImports.size > 0) {
        sections.push('Suggested imports to add:');
        for (const mod of missingImports) {
          sections.push(`  (import ${mod})`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: errors.length > 0,
      };
    },
  );
}
