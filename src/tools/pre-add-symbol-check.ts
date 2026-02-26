import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join, relative, dirname, resolve as resolvePath } from 'node:path';
import { runGxi, buildLoadpathEnv } from '../gxi.js';
import {
  parseDefinitions,
  scanSchemeFiles,
  extractModulePaths,
} from './parse-utils.js';

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

export function registerPreAddSymbolCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_pre_add_symbol_check',
    {
      title: 'Pre-Add Symbol Conflict Check',
      description:
        'Check if adding a new definition with the given symbol name to a target file will cause ' +
        'import conflicts. Scans the file\'s imports and all modules in the project\'s import chain ' +
        'to detect if the symbol is already exported by any imported module. Prevents "Bad binding; ' +
        'import conflict" errors before you edit the file. Especially useful for projects with ' +
        '(export #t) module chains where multiple files re-export all symbols.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        symbol: z
          .string()
          .describe('The symbol name to check (e.g., "my-function", "*my-var*")'),
        file_path: z
          .string()
          .describe('Path to the file where you want to add the definition'),
        project_path: z
          .string()
          .optional()
          .describe(
            'Project root directory (enables cross-module checking via gerbil.pkg)',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for module resolution',
          ),
      },
    },
    async ({ symbol, file_path, project_path, loadpath }) => {
      if (!symbol) {
        return {
          content: [
            { type: 'text' as const, text: 'The "symbol" parameter is required.' },
          ],
          isError: true,
        };
      }
      if (!file_path) {
        return {
          content: [
            { type: 'text' as const, text: 'The "file_path" parameter is required.' },
          ],
          isError: true,
        };
      }

      // Read the target file
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

      // Check if symbol already exists locally
      const localDup = analysis.definitions.find((d) => d.name === symbol);

      // Build loadpath env
      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const env =
        effectiveLoadpath.length > 0
          ? buildLoadpathEnv(effectiveLoadpath)
          : undefined;

      // Collect all module paths from imports
      const allModPaths = new Set<string>();
      for (const imp of analysis.imports) {
        const paths = extractModulePaths(imp.raw);
        for (const p of paths) {
          allModPaths.add(p);
        }
      }

      // Build project-local export map if project_path provided
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
              const fc = await readFile(f, 'utf-8');
              const fa = parseDefinitions(fc);
              const rel = relative(project_path, f)
                .replace(/\.ss$/, '')
                .replace(/\.scm$/, '');
              const modPath = `:${packagePrefix}/${rel}`;
              const exports = extractStaticExports(fa.exports);
              const symbols = exports ?? fa.definitions.map((d) => d.name);
              projectExports.set(modPath, symbols);
            } catch {
              // skip
            }
          }
        }
      }

      // Separate runtime-resolvable from project-local
      const needRuntime = new Set<string>();
      for (const p of allModPaths) {
        if (!p.startsWith('./') && !projectExports.has(p)) {
          needRuntime.add(p);
        }
      }

      // Resolve relative imports to project module paths
      if (project_path && packagePrefix) {
        for (const p of allModPaths) {
          if (p.startsWith('./')) {
            const importingDir = dirname(file_path);
            const targetAbs = resolvePath(importingDir, p.replace(/^\.\//, ''));
            const targetRel = relative(project_path, targetAbs)
              .replace(/\.ss$/, '')
              .replace(/\.scm$/, '');
            const targetMod = `:${packagePrefix}/${targetRel}`;
            if (projectExports.has(targetMod)) {
              allModPaths.add(targetMod); // add resolved form
            }
          }
        }
      }

      // Batch resolve standard library exports
      const runtimeExports = await batchResolveExports(
        [...needRuntime],
        env,
      );

      // Merge all exports
      const allExports = new Map<string, string[]>();
      for (const [k, v] of projectExports) allExports.set(k, v);
      for (const [k, v] of runtimeExports) allExports.set(k, v);

      // Check the symbol against all imported modules
      const conflicts: Array<{ module: string; kind: 'imported' }> = [];
      for (const modPath of allModPaths) {
        // Try direct and resolved forms
        let exports = allExports.get(modPath);
        if (!exports && modPath.startsWith('./') && project_path && packagePrefix) {
          const importingDir = dirname(file_path);
          const targetAbs = resolvePath(importingDir, modPath.replace(/^\.\//, ''));
          const targetRel = relative(project_path, targetAbs)
            .replace(/\.ss$/, '')
            .replace(/\.scm$/, '');
          exports = allExports.get(`:${packagePrefix}/${targetRel}`);
        }
        if (!exports) continue;

        if (exports.includes(symbol)) {
          conflicts.push({ module: modPath, kind: 'imported' });
        }
      }

      // Also check other project modules that might be co-imported via facade
      // (e.g., if A imports B and C, and B already defines the symbol,
      //  adding it to C would cause a conflict in A)
      const coModuleConflicts: Array<{
        definingModule: string;
        importingFile: string;
        line: number | null;
      }> = [];

      if (project_path && packagePrefix) {
        // Determine the target file's module path
        const targetRel = relative(project_path, file_path)
          .replace(/\.ss$/, '')
          .replace(/\.scm$/, '');
        const targetModPath = `:${packagePrefix}/${targetRel}`;

        // Check if the target uses (export #t) — if so, the new symbol would be exported
        const targetExportAll = analysis.exports.some((e) => e.raw.includes('#t'));

        if (targetExportAll) {
          // Find all project files that import both the target module and another
          // module that already defines the symbol
          const allFiles = await scanSchemeFiles(project_path);
          for (const f of allFiles) {
            if (f === file_path) continue;
            try {
              const fc = await readFile(f, 'utf-8');
              const fa = parseDefinitions(fc);
              const importedMods = new Set<string>();
              for (const imp of fa.imports) {
                const paths = extractModulePaths(imp.raw);
                for (const p of paths) {
                  importedMods.add(p);
                  // Resolve relative
                  if (p.startsWith('./')) {
                    const fDir = dirname(f);
                    const tAbs = resolvePath(fDir, p.replace(/^\.\//, ''));
                    const tRel = relative(project_path, tAbs)
                      .replace(/\.ss$/, '')
                      .replace(/\.scm$/, '');
                    importedMods.add(`:${packagePrefix}/${tRel}`);
                  }
                }
              }

              // Does this file import the target module?
              if (!importedMods.has(targetModPath)) continue;

              // Does this file also import another module that exports the symbol?
              for (const mod of importedMods) {
                if (mod === targetModPath) continue;
                const exports = allExports.get(mod);
                if (exports && exports.includes(symbol)) {
                  coModuleConflicts.push({
                    definingModule: mod,
                    importingFile: relative(project_path, f),
                    line: fa.imports[0]?.line ?? null,
                  });
                }
              }
            } catch {
              // skip
            }
          }
        }
      }

      // Build response
      const issues: string[] = [];

      if (localDup) {
        issues.push(
          `LOCAL DUPLICATE: "${symbol}" is already defined in this file at line ${localDup.line} (${localDup.kind}).`,
        );
      }

      for (const c of conflicts) {
        issues.push(
          `IMPORT CONFLICT: "${symbol}" is already exported by ${c.module}, which is imported by this file. ` +
          `Adding a local definition will cause "Bad binding; import conflict".`,
        );
      }

      for (const c of coModuleConflicts) {
        issues.push(
          `CO-IMPORT CONFLICT: "${symbol}" is exported by ${c.definingModule}. ` +
          `Since this file uses (export #t), adding "${symbol}" here would conflict ` +
          `in ${c.importingFile} which imports both modules.`,
        );
      }

      if (issues.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Safe to add: "${symbol}" does not conflict with any imports in ${file_path}.`,
            },
          ],
        };
      }

      const sections: string[] = [
        `Symbol conflict check for "${symbol}" in ${file_path}:`,
        `  ${issues.length} issue(s) found:`,
        '',
        ...issues.map((i) => `  ${i}`),
        '',
        'Suggestions:',
      ];

      if (localDup) {
        sections.push(
          `  - Remove or rename the existing "${symbol}" definition before adding a new one.`,
        );
      }

      for (const c of conflicts) {
        sections.push(
          `  - Use (except-in ${c.module} ${symbol}) in the import to exclude the conflicting symbol.`,
        );
      }

      for (const c of coModuleConflicts) {
        sections.push(
          `  - Choose a different name, or use (except-in ...) in ${c.importingFile} to exclude "${symbol}" from one module.`,
        );
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: true,
      };
    },
  );
}
