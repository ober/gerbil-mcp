import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join, relative } from 'node:path';
import {
  parseDefinitions,
  scanSchemeFiles,
  extractModulePaths,
  findSymbolOccurrences,
  type FileAnalysis,
} from './parse-utils.js';

interface ExportIssue {
  file: string;
  line: number | null;
  severity: 'error' | 'warning';
  message: string;
}

/**
 * Extract plain exported symbol names from export forms.
 * Handles: (export sym1 sym2), (export #t), (export (struct-out name)), etc.
 * Returns null for (export #t) since it means "export everything".
 */
function extractExportedSymbols(
  exportForms: Array<{ raw: string; line: number }>,
): { symbols: string[]; exportAll: boolean; lines: Map<string, number> } {
  const symbols: string[] = [];
  const lines = new Map<string, number>();
  let exportAll = false;

  for (const exp of exportForms) {
    const raw = exp.raw;

    // (export #t) means export everything
    if (raw.includes('#t')) {
      exportAll = true;
      continue;
    }

    // Strip the outer (export ...) wrapper
    const inner = raw
      .replace(/^\s*\(export\s+/, '')
      .replace(/\)\s*$/, '')
      .trim();

    if (!inner) continue;

    // Tokenize: split on whitespace, but skip sub-forms like (struct-out ...), (import: ...), (rename-out ...)
    let pos = 0;
    while (pos < inner.length) {
      // Skip whitespace
      while (pos < inner.length && /\s/.test(inner[pos])) pos++;
      if (pos >= inner.length) break;

      if (inner[pos] === '(') {
        // Skip sub-form entirely — these are special export directives
        let depth = 1;
        pos++;
        while (pos < inner.length && depth > 0) {
          if (inner[pos] === '(') depth++;
          else if (inner[pos] === ')') depth--;
          pos++;
        }
      } else {
        // Read a plain symbol token
        const start = pos;
        while (pos < inner.length && !/[\s()[\]{}]/.test(inner[pos])) pos++;
        const sym = inner.slice(start, pos);
        if (sym && sym !== '#t' && sym !== '#f') {
          symbols.push(sym);
          lines.set(sym, exp.line);
        }
      }
    }
  }

  return { symbols, exportAll, lines };
}

/**
 * Build a module path from a file path, package prefix, and project root.
 * E.g., project_path=/foo, pkg=myapp, file=/foo/bar/baz.ss => :myapp/bar/baz
 */
function fileToModulePath(
  filePath: string,
  projectPath: string,
  packagePrefix: string,
): string {
  const rel = relative(projectPath, filePath)
    .replace(/\.ss$/, '')
    .replace(/\.scm$/, '');
  return `:${packagePrefix}/${rel}`;
}

export function registerCheckExportsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_check_exports',
    {
      title: 'Cross-Module Export Checker',
      description:
        'Static analysis tool that checks export/import consistency across a Gerbil project. ' +
        'Detects: (1) symbols exported but not defined in the file, ' +
        '(2) cross-module import mismatches where file A imports from project module B ' +
        'but uses symbols that B does not export. ' +
        'Pure static analysis — no subprocess, fast.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        project_path: z
          .string()
          .describe('Path to the Gerbil project directory'),
      },
    },
    async ({ project_path }) => {
      // Read gerbil.pkg for package prefix
      let packagePrefix = '';
      try {
        const pkgContent = await readFile(
          join(project_path, 'gerbil.pkg'),
          'utf-8',
        );
        const match = pkgContent.match(/\(package:\s+([^\s)]+)\)/);
        if (match) packagePrefix = match[1];
      } catch {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No gerbil.pkg found in ${project_path}. Cannot determine package prefix.`,
            },
          ],
          isError: true,
        };
      }

      // Scan all .ss files
      const files = await scanSchemeFiles(project_path);
      if (files.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No .ss files found in ${project_path}.`,
            },
          ],
        };
      }

      // Build module map: module path => { file, analysis, exportedSymbols }
      const moduleMap = new Map<
        string,
        {
          file: string;
          analysis: FileAnalysis;
          exportedSymbols: string[];
          exportAll: boolean;
          definedNames: Set<string>;
        }
      >();

      for (const file of files) {
        try {
          const content = await readFile(file, 'utf-8');
          const analysis = parseDefinitions(content);
          const modPath = fileToModulePath(file, project_path, packagePrefix);
          const { symbols, exportAll } = extractExportedSymbols(
            analysis.exports,
          );
          const definedNames = new Set(analysis.definitions.map((d) => d.name));

          moduleMap.set(modPath, {
            file,
            analysis,
            exportedSymbols: symbols,
            exportAll,
            definedNames,
          });
        } catch {
          // Skip unreadable files
        }
      }

      const issues: ExportIssue[] = [];

      // Check 1: Missing definitions for exported symbols
      for (const [modPath, mod] of moduleMap) {
        if (mod.exportAll) continue; // Can't check (export #t)

        for (const sym of mod.exportedSymbols) {
          if (!mod.definedNames.has(sym)) {
            const { lines } = extractExportedSymbols(mod.analysis.exports);
            issues.push({
              file: relative(project_path, mod.file),
              line: lines.get(sym) ?? null,
              severity: 'error',
              message: `Exports "${sym}" but no definition found in file`,
            });
          }
        }
      }

      // Check 2: Cross-module import mismatches
      for (const [_modPath, mod] of moduleMap) {
        const content = await readFile(mod.file, 'utf-8').catch(() => '');
        if (!content) continue;

        for (const imp of mod.analysis.imports) {
          const importedPaths = extractModulePaths(imp.raw);
          for (const importedMod of importedPaths) {
            // Only check project-internal modules
            const target = moduleMap.get(importedMod);
            if (!target) continue;
            if (target.exportAll) continue; // Can't check (export #t)

            // Find symbols from target that are used in this file
            const targetExports = new Set(target.exportedSymbols);
            const targetDefined = target.definedNames;

            // For each symbol defined in the target, check if it's used but not exported
            for (const defName of targetDefined) {
              if (targetExports.has(defName)) continue; // Already exported, fine

              // Check if this file uses the symbol
              const occurrences = findSymbolOccurrences(content, defName);
              // Filter out occurrences that are in the import form itself
              const usagesAfterImport = occurrences.filter(
                (o) => o.line > imp.line,
              );

              if (usagesAfterImport.length > 0) {
                issues.push({
                  file: relative(project_path, mod.file),
                  line: usagesAfterImport[0].line,
                  severity: 'warning',
                  message:
                    `Uses "${defName}" from ${importedMod}, but that module does not export it`,
                });
              }
            }
          }
        }
      }

      if (issues.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Export check passed: ${moduleMap.size} module(s) in ${project_path} — no issues found.`,
            },
          ],
        };
      }

      // Sort: errors first, then by file, then by line
      const sevOrder: Record<string, number> = { error: 0, warning: 1 };
      issues.sort((a, b) => {
        const sa = sevOrder[a.severity] ?? 2;
        const sb = sevOrder[b.severity] ?? 2;
        if (sa !== sb) return sa - sb;
        if (a.file !== b.file) return a.file.localeCompare(b.file);
        return (a.line ?? 0) - (b.line ?? 0);
      });

      const errors = issues.filter((i) => i.severity === 'error');
      const warnings = issues.filter((i) => i.severity === 'warning');

      const sections: string[] = [
        `Export check: ${project_path}`,
        `  ${errors.length} error(s), ${warnings.length} warning(s)`,
        '',
      ];

      for (const issue of issues) {
        const loc = issue.line ? `${issue.file}:${issue.line}` : issue.file;
        sections.push(
          `  [${issue.severity.toUpperCase()}] ${loc}: ${issue.message}`,
        );
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: errors.length > 0,
      };
    },
  );
}
