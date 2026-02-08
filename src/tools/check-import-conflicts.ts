import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join, relative, dirname, resolve as resolvePath } from 'node:path';
import { runGxi, buildLoadpathEnv } from '../gxi.js';
import {
  parseDefinitions,
  scanSchemeFiles,
  extractModulePaths,
  type FileAnalysis,
} from './parse-utils.js';

interface ConflictDiagnostic {
  file: string;
  line: number | null;
  severity: 'error' | 'warning';
  code: string;
  message: string;
}

/**
 * Batch-resolve module exports via a single gxi subprocess.
 * Returns a map from module path to list of exported symbol names.
 */
async function batchResolveExports(
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
 * Parse only-in filters from an import form.
 * Returns a map from module path to the set of symbols allowed.
 */
function parseOnlyInFilters(
  importText: string,
): Map<string, Set<string>> {
  const filters = new Map<string, Set<string>>();
  const pattern =
    /\(\s*only-in\s+(:[a-zA-Z][a-zA-Z0-9/_-]*)((?:\s+[a-zA-Z_!?<>=+\-*/][a-zA-Z0-9_!?<>=+\-*/.:#~]*)*)\s*\)/g;
  let match;
  while ((match = pattern.exec(importText)) !== null) {
    const modPath = match[1];
    const symsText = match[2]?.trim() || '';
    const syms = symsText ? symsText.split(/\s+/).filter(Boolean) : [];
    filters.set(modPath, new Set(syms));
  }
  return filters;
}

/**
 * Check if a module path appears inside a filter form other than only-in
 * (e.g. except-in, rename-in, prefix-in). We can't reliably determine
 * which symbols are imported for these, so we skip them (conservative).
 */
function isInOtherFilter(importText: string, modPath: string): boolean {
  const escaped = modPath.replace(/[/]/g, '\\/');
  const filterKeywords = [
    'except-in',
    'except-out',
    'rename-in',
    'prefix-in',
    'prefix-out',
    'rename-out',
    'group-in',
  ];
  for (const kw of filterKeywords) {
    const re = new RegExp(`\\(\\s*${kw}\\s+${escaped}\\b`);
    if (re.test(importText)) return true;
  }
  return false;
}

/**
 * Extract exported symbol names from export forms (static analysis).
 * Returns null if (export #t) is found (means "export everything").
 */
function extractStaticExports(
  exportForms: Array<{ raw: string; line: number }>,
): string[] | null {
  const symbols: string[] = [];
  let exportAll = false;

  for (const exp of exportForms) {
    if (exp.raw.includes('#t')) {
      exportAll = true;
      continue;
    }

    const inner = exp.raw
      .replace(/^\s*\(export\s+/, '')
      .replace(/\)\s*$/, '')
      .trim();

    if (!inner) continue;

    let pos = 0;
    while (pos < inner.length) {
      while (pos < inner.length && /\s/.test(inner[pos])) pos++;
      if (pos >= inner.length) break;

      if (inner[pos] === '(') {
        let depth = 1;
        pos++;
        while (pos < inner.length && depth > 0) {
          if (inner[pos] === '(') depth++;
          else if (inner[pos] === ')') depth--;
          pos++;
        }
      } else {
        const start = pos;
        while (pos < inner.length && !/[\s()[\]{}]/.test(inner[pos])) pos++;
        const sym = inner.slice(start, pos);
        if (sym && sym !== '#t' && sym !== '#f' && !sym.startsWith(';')) {
          symbols.push(sym);
        }
      }
    }
  }

  return exportAll ? null : symbols;
}

export function registerCheckImportConflictsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_check_import_conflicts',
    {
      title: 'Import Conflict Detector',
      description:
        'Detect import conflicts before build. Checks if locally defined symbols ' +
        'conflict with imported module exports (causing cryptic "Bad binding; import ' +
        'conflict" errors), and if multiple imports export the same symbol. ' +
        'Resolves standard library exports at runtime and project-local exports statically. ' +
        'Handles only-in filtered imports. Provide either file_path or project_path.',
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Single file to check for import conflicts'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory to check all .ss files'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for module resolution',
          ),
      },
    },
    async ({ file_path, project_path, loadpath }) => {
      if (!file_path && !project_path) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Either file_path or project_path is required.',
            },
          ],
          isError: true,
        };
      }

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const env =
        effectiveLoadpath.length > 0
          ? buildLoadpathEnv(effectiveLoadpath)
          : undefined;

      // Determine files to check
      const filesToCheck: Array<{
        path: string;
        content: string;
        analysis: FileAnalysis;
      }> = [];

      if (file_path) {
        try {
          const content = await readFile(file_path, 'utf-8');
          filesToCheck.push({
            path: file_path,
            content,
            analysis: parseDefinitions(content),
          });
        } catch (err) {
          const msg =
            err instanceof Error ? err.message : 'Unknown error';
          return {
            content: [
              {
                type: 'text' as const,
                text: `Failed to read file: ${msg}`,
              },
            ],
            isError: true,
          };
        }
      } else if (project_path) {
        const files = await scanSchemeFiles(project_path);
        for (const f of files) {
          try {
            const content = await readFile(f, 'utf-8');
            filesToCheck.push({
              path: relative(project_path, f),
              content,
              analysis: parseDefinitions(content),
            });
          } catch {
            // Skip unreadable
          }
        }
      }

      if (filesToCheck.length === 0) {
        return {
          content: [
            { type: 'text' as const, text: 'No .ss files found to check.' },
          ],
        };
      }

      // Build project-local static export map
      const projectExports = new Map<string, string[]>();
      let packagePrefix = '';
      if (project_path) {
        try {
          const pkgContent = await readFile(
            join(project_path, 'gerbil.pkg'),
            'utf-8',
          );
          const match = pkgContent.match(/\(package:\s+([^\s)]+)\)/);
          if (match) packagePrefix = match[1];
        } catch {
          // No gerbil.pkg
        }

        if (packagePrefix) {
          const allFiles = await scanSchemeFiles(project_path);
          for (const f of allFiles) {
            try {
              const content = await readFile(f, 'utf-8');
              const analysis = parseDefinitions(content);
              const rel = relative(project_path, f)
                .replace(/\.ss$/, '')
                .replace(/\.scm$/, '');
              const modPath = `:${packagePrefix}/${rel}`;
              const exports = extractStaticExports(analysis.exports);
              const symbols =
                exports ?? analysis.definitions.map((d) => d.name);
              projectExports.set(modPath, symbols);
            } catch {
              // skip
            }
          }
        }
      }

      // Collect all unique non-relative module paths needing runtime resolution
      const needRuntime = new Set<string>();
      for (const f of filesToCheck) {
        for (const imp of f.analysis.imports) {
          const paths = extractModulePaths(imp.raw);
          for (const p of paths) {
            if (!p.startsWith('./') && !projectExports.has(p)) {
              needRuntime.add(p);
            }
          }
        }
      }

      // Batch resolve standard library / external modules via gxi
      const runtimeExports = await batchResolveExports(
        [...needRuntime],
        env,
      );

      // Merge runtime + project exports into a single lookup
      const allExports = new Map<string, string[]>();
      for (const [k, v] of projectExports) allExports.set(k, v);
      for (const [k, v] of runtimeExports) allExports.set(k, v);

      // Check each file for conflicts
      const diagnostics: ConflictDiagnostic[] = [];

      for (const f of filesToCheck) {
        const localDefs = new Set(
          f.analysis.definitions.map((d) => d.name),
        );
        const defLineMap = new Map(
          f.analysis.definitions.map((d) => [d.name, d.line]),
        );

        // Track all imported symbols for cross-import conflict detection
        const importedSymbols = new Map<string, string[]>();

        for (const imp of f.analysis.imports) {
          const modPaths = extractModulePaths(imp.raw);
          const onlyInFilters = parseOnlyInFilters(imp.raw);

          for (const modPath of modPaths) {
            let exports = allExports.get(modPath);
            if (!exports) {
              // Try resolving relative import for project mode
              if (modPath.startsWith('./') && project_path && packagePrefix) {
                const importingFileAbs =
                  f.path.startsWith('/')
                    ? f.path
                    : join(project_path, f.path);
                const importingDir = dirname(importingFileAbs);
                const targetAbs = resolvePath(
                  importingDir,
                  modPath.replace(/^\.\//, ''),
                );
                const targetRel = relative(project_path, targetAbs)
                  .replace(/\.ss$/, '')
                  .replace(/\.scm$/, '');
                const targetMod = `:${packagePrefix}/${targetRel}`;
                exports = allExports.get(targetMod);
              }
              if (!exports) continue;
            }

            // Apply only-in filter if present
            const onlyIn = onlyInFilters.get(modPath);
            let effectiveExports: string[];
            if (onlyIn) {
              effectiveExports = exports.filter((s) => onlyIn.has(s));
            } else if (isInOtherFilter(imp.raw, modPath)) {
              // Can't determine which symbols — skip (conservative)
              continue;
            } else {
              effectiveExports = exports;
            }

            for (const sym of effectiveExports) {
              // Track for cross-import detection
              if (!importedSymbols.has(sym)) {
                importedSymbols.set(sym, []);
              }
              importedSymbols.get(sym)!.push(modPath);

              // Check local definition conflict
              if (localDefs.has(sym)) {
                diagnostics.push({
                  file: f.path,
                  line: defLineMap.get(sym) ?? null,
                  severity: 'error',
                  code: 'import-conflict',
                  message: `Local definition "${sym}" conflicts with import from ${modPath}`,
                });
              }
            }
          }
        }

        // Cross-import conflicts (multiple imports provide the same symbol)
        for (const [sym, modules] of importedSymbols) {
          const unique = [...new Set(modules)];
          if (unique.length > 1) {
            diagnostics.push({
              file: f.path,
              line: f.analysis.imports[0]?.line ?? null,
              severity: 'warning',
              code: 'cross-import-conflict',
              message: `Symbol "${sym}" is exported by multiple imports: ${unique.join(', ')}`,
            });
          }
        }
      }

      if (diagnostics.length === 0) {
        const target = file_path || project_path;
        return {
          content: [
            {
              type: 'text' as const,
              text: `No import conflicts found in ${target}.`,
            },
          ],
        };
      }

      // Sort: errors first, then by file, then by line
      diagnostics.sort((a, b) => {
        const sa = a.severity === 'error' ? 0 : 1;
        const sb = b.severity === 'error' ? 0 : 1;
        if (sa !== sb) return sa - sb;
        if (a.file !== b.file) return a.file.localeCompare(b.file);
        return (a.line ?? 0) - (b.line ?? 0);
      });

      const errors = diagnostics.filter((d) => d.severity === 'error');
      const warnings = diagnostics.filter((d) => d.severity === 'warning');

      const sections: string[] = [
        `Import conflict check: ${file_path || project_path}`,
        `  ${errors.length} conflict(s), ${warnings.length} warning(s)`,
        '',
      ];

      for (const d of diagnostics) {
        const loc = d.line ? `${d.file}:${d.line}` : d.file;
        sections.push(
          `  [${d.severity.toUpperCase()}] ${loc} (${d.code}): ${d.message}`,
        );
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: errors.length > 0,
      };
    },
  );
}
