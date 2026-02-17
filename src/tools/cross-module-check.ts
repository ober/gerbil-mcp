import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFile } from 'node:fs/promises';
import { join, relative, basename } from 'node:path';
import { z } from 'zod';
import { runGxi, buildLoadpathEnv } from '../gxi.js';
import {
  parseDefinitions,
  scanSchemeFiles,
  extractModulePaths,
  extractCallSites,
} from './parse-utils.js';

interface UnboundSymbol {
  symbol: string;
  file: string;
  line: number;
  definedIn?: string;  // file that defines this symbol
}

/**
 * Batch-resolve module exports via a single gxi subprocess.
 * Returns a map from module path to list of exported symbol names.
 */
async function batchResolveStdlibExports(
  modPaths: string[],
  env?: Record<string, string>,
): Promise<Map<string, string[]>> {
  const results = new Map<string, string[]>();
  if (modPaths.length === 0) return results;

  const modList = modPaths.join(' ');

  const exprs = [
    '(import :gerbil/expander)',
    [
      '(for-each',
      '  (lambda (mod-sym)',
      '    (with-catch',
      '      (lambda (e) (display "MODULE-ERROR:") (displayln mod-sym))',
      '      (lambda ()',
      '        (let ((mod (import-module mod-sym #f #t)))',
      '          (display "MODULE:") (displayln mod-sym)',
      '          (for-each',
      '            (lambda (e) (display "  SYM:") (displayln (module-export-name e)))',
      '            (module-context-export mod))))))',
      `  '(${modList}))`,
    ].join(' '),
  ];

  const result = await runGxi(exprs, { env, timeout: 30_000 });
  if (result.timedOut) return results;

  let currentModule: string | null = null;
  let currentExports: string[] = [];

  for (const line of result.stdout.split('\n')) {
    const trimmed = line.trim();
    if (trimmed.startsWith('MODULE-ERROR:')) continue;
    if (trimmed.startsWith('MODULE:')) {
      if (currentModule) {
        results.set(currentModule, currentExports);
      }
      currentModule = trimmed.slice('MODULE:'.length).trim();
      currentExports = [];
    } else if (trimmed.startsWith('SYM:') && currentModule) {
      const sym = trimmed.slice('SYM:'.length).trim();
      if (sym) currentExports.push(sym);
    }
  }

  if (currentModule) {
    results.set(currentModule, currentExports);
  }

  return results;
}

/**
 * Extract exported symbol names from an export form statically.
 */
function extractExportedSymbols(exportText: string): string[] {
  const symbols: string[] = [];
  // Remove (export ...) wrapper
  const inner = exportText.replace(/^\(export\s+/, '').replace(/\)\s*$/, '');
  // Extract bare symbols and #t-prefixed symbols
  const tokenPattern = /(?:^|\s)([a-zA-Z_!?*+\-/<>=][a-zA-Z0-9_!?*+\-/<>=.:]*)/g;
  let match;
  while ((match = tokenPattern.exec(inner)) !== null) {
    const sym = match[1];
    // Skip keywords like #t, only-in:, etc.
    if (!sym.startsWith('#') && !sym.endsWith(':') && !sym.startsWith('(')) {
      symbols.push(sym);
    }
  }
  return symbols;
}

/** Set of built-in Gerbil/Scheme symbols that don't need importing */
const BUILTINS = new Set([
  // Core forms
  'def', 'define', 'lambda', 'let', 'let*', 'letrec', 'letrec*',
  'if', 'cond', 'case', 'when', 'unless', 'begin', 'begin0',
  'set!', 'quote', 'quasiquote', 'unquote', 'unquote-splicing',
  'and', 'or', 'not', 'do', 'match', 'match*',
  'try', 'catch', 'finally', 'raise', 'error',
  'import', 'export', 'require',
  'defstruct', 'defclass', 'defrule', 'defrules', 'defsyntax', 'defmethod',
  'definterface', 'deftype', 'defalias', 'defconst', 'definline', 'defvalues',
  'def*', 'def/c',
  // Basic procedures
  '+', '-', '*', '/', '=', '<', '>', '<=', '>=',
  'eq?', 'eqv?', 'equal?', 'null?', 'pair?', 'list?', 'number?',
  'string?', 'symbol?', 'boolean?', 'char?', 'vector?', 'procedure?',
  'car', 'cdr', 'cons', 'list', 'append', 'reverse', 'length',
  'map', 'for-each', 'filter', 'foldl', 'foldr', 'apply',
  'string-append', 'string-length', 'string-ref', 'substring',
  'number->string', 'string->number', 'symbol->string', 'string->symbol',
  'display', 'displayln', 'newline', 'write', 'read',
  'values', 'call-with-values', 'receive',
  'void', '#t', '#f', '#!void',
  'make-hash-table', 'hash-put!', 'hash-get', 'hash-ref', 'hash-key?',
  'with-catch', 'with-exception-handler',
  'parameterize', 'make-parameter',
  'current-input-port', 'current-output-port', 'current-error-port',
  'open-input-file', 'open-output-file', 'close-port',
  'read-line', 'read-char', 'write-char',
  'for', 'for/collect', 'for/fold', 'in-range', 'in-list',
  'using', 'interface',
  'spawn', 'spawn/name', 'thread-sleep!',
  'make-mutex', 'mutex-lock!', 'mutex-unlock!',
]);

export function registerCrossModuleCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_cross_module_check',
    {
      title: 'Cross-Module Symbol Check',
      description:
        'Detect cross-module unbound symbol references before compilation. ' +
        'Scans project files, resolves imports (both stdlib and project-local), ' +
        'and reports symbols referenced but not available from any import or local ' +
        'definition. Critical when splitting large modules into sub-modules — ' +
        'catches all missing references in one pass instead of compile-fix cycles.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        project_path: z
          .string()
          .describe('Path to the Gerbil project directory'),
        files: z
          .array(z.string())
          .optional()
          .describe('Specific files to check (default: all .ss files in project)'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Additional GERBIL_LOADPATH entries for module resolution'),
      },
    },
    async ({ project_path, files, loadpath }) => {
      // Discover files
      let filesToCheck: string[];
      if (files && files.length > 0) {
        filesToCheck = files;
      } else {
        filesToCheck = (await scanSchemeFiles(project_path))
          .filter(f => f.endsWith('.ss'));
      }

      if (filesToCheck.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: 'No .ss files found to check.',
          }],
        };
      }

      // Parse all files
      const fileAnalyses = new Map<string, {
        content: string;
        defs: Set<string>;
        importPaths: string[];
        exports: string[];
        callSites: Array<{ symbol: string; line: number }>;
      }>();

      // Build project-wide definition map
      const projectDefMap = new Map<string, string>(); // symbol -> file

      for (const file of filesToCheck) {
        let content: string;
        try {
          content = await readFile(file, 'utf-8');
        } catch {
          continue;
        }

        const analysis = parseDefinitions(content);
        const defs = new Set(analysis.definitions.map(d => d.name));
        const importPaths: string[] = [];
        for (const imp of analysis.imports) {
          importPaths.push(...extractModulePaths(imp.raw));
        }

        const exports = analysis.exports.flatMap(e => extractExportedSymbols(e.raw));

        const callSites = extractCallSites(content).map(cs => ({
          symbol: cs.symbol,
          line: cs.line,
        }));

        fileAnalyses.set(file, { content, defs, importPaths, exports, callSites });

        // Add exported symbols to project-wide map
        for (const exp of exports) {
          if (!projectDefMap.has(exp)) {
            projectDefMap.set(exp, file);
          }
        }
        // Also add all definitions
        for (const def of defs) {
          if (!projectDefMap.has(def)) {
            projectDefMap.set(def, file);
          }
        }
      }

      // Collect all stdlib imports to resolve in batch
      const stdlibImports = new Set<string>();
      const projectLocalImports = new Map<string, string[]>(); // file -> local import paths

      for (const [file, data] of fileAnalyses) {
        const stdPaths: string[] = [];
        const localPaths: string[] = [];

        for (const path of data.importPaths) {
          if (path.startsWith(':')) {
            stdPaths.push(path);
          } else if (path.startsWith('./')) {
            localPaths.push(path);
          }
        }

        for (const p of stdPaths) {
          stdlibImports.add(p);
        }
        projectLocalImports.set(file, localPaths);
      }

      // Resolve stdlib exports in batch
      const env = loadpath ? buildLoadpathEnv(loadpath) : {};
      const stdlibExports = await batchResolveStdlibExports([...stdlibImports], env);

      // For each file, compute available symbols and find unbound references
      const allUnbound: UnboundSymbol[] = [];

      for (const [file, data] of fileAnalyses) {
        const available = new Set<string>(data.defs);

        // Add stdlib imports
        for (const path of data.importPaths) {
          if (path.startsWith(':')) {
            const exports = stdlibExports.get(path);
            if (exports) {
              for (const sym of exports) {
                available.add(sym);
              }
            }
          }
        }

        // Add project-local imports
        const localPaths = projectLocalImports.get(file) || [];
        for (const localPath of localPaths) {
          // Resolve ./foo to find the file
          const resolvedFile = join(project_path, localPath.slice(2) + '.ss');
          const localData = fileAnalyses.get(resolvedFile);
          if (localData) {
            // Add exported symbols from local module
            for (const sym of localData.exports) {
              available.add(sym);
            }
            // If no explicit exports, add all definitions
            if (localData.exports.length === 0) {
              for (const sym of localData.defs) {
                available.add(sym);
              }
            }
          }
        }

        // Check each call site for unbound symbols
        const seen = new Set<string>();
        for (const site of data.callSites) {
          if (seen.has(site.symbol)) continue;
          seen.add(site.symbol);

          if (!available.has(site.symbol) && !BUILTINS.has(site.symbol)) {
            const definedIn = projectDefMap.get(site.symbol);
            // Only report if it's not defined in the same file
            if (definedIn !== file) {
              allUnbound.push({
                symbol: site.symbol,
                file,
                line: site.line,
                definedIn,
              });
            }
          }
        }
      }

      // Report results
      const sections: string[] = [];
      sections.push(`Checked ${filesToCheck.length} file(s) in ${basename(project_path)}`);
      sections.push('');

      if (allUnbound.length === 0) {
        sections.push('All symbol references resolved. No unbound symbols detected.');
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      // Group by file
      const byFile = new Map<string, UnboundSymbol[]>();
      for (const ub of allUnbound) {
        const list = byFile.get(ub.file) || [];
        list.push(ub);
        byFile.set(ub.file, list);
      }

      sections.push(`Found ${allUnbound.length} potentially unbound symbol(s):`);
      sections.push('');

      for (const [file, symbols] of byFile) {
        const relPath = relative(project_path, file);
        sections.push(`  ${relPath}:`);
        for (const sym of symbols) {
          const source = sym.definedIn
            ? ` (defined in ${relative(project_path, sym.definedIn)})`
            : '';
          sections.push(`    line ${sym.line}: ${sym.symbol}${source}`);
        }
        sections.push('');
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: true,
      };
    },
  );
}
