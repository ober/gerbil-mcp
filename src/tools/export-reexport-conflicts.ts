import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { runGxi, buildLoadpathEnv } from '../gxi.js';
import {
  parseDefinitions,
  extractModulePaths,
} from './parse-utils.js';

interface ReexportConflict {
  symbol: string;
  modules: string[];
  suggestion: string;
}

/**
 * Resolve full export lists for modules, including transitive re-exports
 * from (export #t). Uses module-context-export which returns the complete
 * set of symbols a module makes available to importers.
 */
async function resolveFullExports(
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
 * Parse only-in filters from import text.
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
 * Check if a module appears in a filter form other than only-in.
 */
function isInOtherFilter(importText: string, modPath: string): boolean {
  const escaped = modPath.replace(/[/]/g, '\\/');
  const filterKeywords = [
    'except-in', 'except-out', 'rename-in',
    'prefix-in', 'prefix-out', 'rename-out', 'group-in',
  ];
  for (const kw of filterKeywords) {
    const re = new RegExp(`\\(\\s*${kw}\\s+${escaped}\\b`);
    if (re.test(importText)) return true;
  }
  return false;
}

export function registerExportReexportConflictsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_export_reexport_conflicts',
    {
      title: 'Export Re-export Conflict Detector',
      description:
        'Detect import conflicts caused by modules using (export #t) that re-export symbols ' +
        'from their own imports. When two imported modules transitively export the same symbol, ' +
        'building fails with "Bad binding; import conflict". Reports conflicts and suggests ' +
        '(only-in ...) fixes. Especially useful for third-party packages that use (export #t) liberally.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z.string().describe('Path to the Gerbil source file to check'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Directories to add to GERBIL_LOADPATH'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory for auto-configuring GERBIL_LOADPATH'),
      },
    },
    async ({ file_path, loadpath, project_path }) => {
      if (!file_path) {
        return {
          content: [{ type: 'text' as const, text: 'Error: file_path is required.' }],
          isError: true,
        };
      }

      let source: string;
      try {
        source = await readFile(file_path, 'utf-8');
      } catch {
        return {
          content: [{ type: 'text' as const, text: `Error: cannot read file ${file_path}` }],
          isError: true,
        };
      }

      const analysis = parseDefinitions(source);

      // Extract all imported module paths
      const importText = analysis.imports.map((i) => i.raw).join('\n');
      const modPaths = extractModulePaths(importText);

      if (modPaths.length < 2) {
        return {
          content: [{ type: 'text' as const, text: 'No potential conflicts: fewer than 2 module imports found.' }],
        };
      }

      // Parse only-in filters and other filters
      const onlyInFilters = parseOnlyInFilters(importText);

      // Build loadpath env
      let env: Record<string, string> | undefined;
      if (loadpath && loadpath.length > 0) {
        env = buildLoadpathEnv(loadpath);
      } else if (project_path) {
        const localLib = join(project_path, '.gerbil', 'lib');
        env = buildLoadpathEnv([localLib]);
      }

      // Resolve full exports for all imported modules
      const exportMap = await resolveFullExports(modPaths, env);

      // Apply only-in filters to narrow exported symbols
      const filteredExports = new Map<string, Set<string>>();
      for (const [mod, exports] of exportMap) {
        if (onlyInFilters.has(mod)) {
          // Only keep symbols listed in only-in
          const allowed = onlyInFilters.get(mod)!;
          filteredExports.set(mod, new Set(exports.filter((s) => allowed.has(s))));
        } else if (isInOtherFilter(importText, mod)) {
          // Skip modules with other filters — we can't reliably determine scope
          continue;
        } else {
          filteredExports.set(mod, new Set(exports));
        }
      }

      // Find symbols exported by multiple modules
      const symbolToModules = new Map<string, string[]>();
      for (const [mod, exports] of filteredExports) {
        for (const sym of exports) {
          if (!symbolToModules.has(sym)) {
            symbolToModules.set(sym, []);
          }
          symbolToModules.get(sym)!.push(mod);
        }
      }

      const conflicts: ReexportConflict[] = [];
      for (const [sym, mods] of symbolToModules) {
        if (mods.length > 1) {
          // Generate (only-in) suggestion: keep the symbol from the first module,
          // wrap others in only-in excluding this symbol
          const keepMod = mods[0];
          const fixMods = mods.slice(1);
          const suggestion = fixMods
            .map((m) => {
              // Get all symbols needed from this module (excluding the conflicting one)
              const needed = filteredExports.get(m);
              if (!needed || needed.size <= 1) {
                return `(only-in ${m} <your-needed-symbols>)`;
              }
              // Show a few non-conflicting symbols as examples
              const nonConflicting = [...needed].filter((s) => s !== sym).slice(0, 3);
              return `(only-in ${m} ${nonConflicting.join(' ')} ...)`;
            })
            .join(' or ');
          conflicts.push({
            symbol: sym,
            modules: mods,
            suggestion: `Keep from ${keepMod}, wrap others: ${suggestion}`,
          });
        }
      }

      if (conflicts.length === 0) {
        return {
          content: [{ type: 'text' as const, text: `No re-export conflicts found across ${filteredExports.size} imported modules.` }],
        };
      }

      // Sort by symbol name for stable output
      conflicts.sort((a, b) => a.symbol.localeCompare(b.symbol));

      const sections: string[] = [
        `Found ${conflicts.length} re-export conflict(s) across ${filteredExports.size} imported modules:`,
        '',
      ];

      // Group by module pair for readability
      for (const c of conflicts.slice(0, 50)) {
        sections.push(`  ${c.symbol}`);
        sections.push(`    exported by: ${c.modules.join(', ')}`);
        sections.push(`    fix: ${c.suggestion}`);
      }

      if (conflicts.length > 50) {
        sections.push(`  ... and ${conflicts.length - 50} more`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: true,
      };
    },
  );
}
