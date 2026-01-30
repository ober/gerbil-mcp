import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { runGxc } from '../gxi.js';
import {
  parseDefinitions,
  parseGxcErrors,
  extractModulePaths,
  type FileAnalysis,
} from './parse-utils.js';

interface LintDiagnostic {
  line: number | null;
  severity: 'error' | 'warning' | 'info';
  code: string;
  message: string;
}

const COMMON_STDLIB = new Set([
  'map',
  'filter',
  'fold',
  'for-each',
  'apply',
  'append',
  'cons',
  'car',
  'cdr',
  'list',
  'length',
  'reverse',
  'sort',
  'display',
  'displayln',
  'write',
  'read',
  'string-append',
  'number->string',
  'string->number',
  'not',
  'error',
  'raise',
  'values',
  'call-with-values',
  'void',
  'begin',
  'hash',
  'vector',
  'string',
  'number?',
  'string?',
  'pair?',
  'null?',
  'boolean?',
  'symbol?',
  'vector?',
  'procedure?',
  'equal?',
  'eq?',
  'eqv?',
]);

export function registerLintTool(server: McpServer): void {
  server.registerTool(
    'gerbil_lint',
    {
      title: 'Basic Linting',
      description:
        'Static analysis for common Gerbil issues: unused imports, duplicate definitions, ' +
        'style warnings (define vs def, missing transparent:), shadowed standard bindings, ' +
        'and compilation errors via gxc.',
      inputSchema: {
        file_path: z
          .string()
          .describe('Absolute path to a Gerbil source file to lint'),
      },
    },
    async ({ file_path }) => {
      let content: string;
      try {
        content = await readFile(file_path, 'utf-8');
      } catch (err) {
        const msg =
          err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            { type: 'text' as const, text: `Failed to read file: ${msg}` },
          ],
          isError: true,
        };
      }

      const diagnostics: LintDiagnostic[] = [];
      const analysis = parseDefinitions(content);
      const lines = content.split('\n');

      checkUnusedImports(content, analysis, diagnostics);
      checkDuplicateDefinitions(analysis, diagnostics);
      checkStyleIssues(lines, diagnostics);
      checkShadowedBindings(analysis, diagnostics);

      // Compile check via gxc
      const compileResult = await runGxc(file_path, { timeout: 30_000 });
      if (
        compileResult.exitCode !== 0 &&
        compileResult.exitCode !== 127 &&
        !compileResult.timedOut
      ) {
        const gxcDiags = parseGxcErrors(compileResult.stderr, file_path);
        for (const d of gxcDiags) {
          diagnostics.push({
            line: d.line,
            severity: d.severity,
            code: 'compile-error',
            message: d.message,
          });
        }
      }

      if (diagnostics.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No issues found in ${file_path}.`,
            },
          ],
        };
      }

      // Sort: errors first, then warnings, then info; within severity by line
      const sevOrder: Record<string, number> = {
        error: 0,
        warning: 1,
        info: 2,
      };
      diagnostics.sort((a, b) => {
        const sa = sevOrder[a.severity] ?? 3;
        const sb = sevOrder[b.severity] ?? 3;
        if (sa !== sb) return sa - sb;
        return (a.line ?? 0) - (b.line ?? 0);
      });

      const errors = diagnostics.filter((d) => d.severity === 'error');
      const warnings = diagnostics.filter((d) => d.severity === 'warning');
      const infos = diagnostics.filter((d) => d.severity === 'info');

      const sections: string[] = [
        `Lint: ${file_path}`,
        `  ${errors.length} error(s), ${warnings.length} warning(s), ${infos.length} info`,
        '',
      ];

      for (const d of diagnostics) {
        const loc = d.line ? `L${d.line}` : '---';
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

function checkUnusedImports(
  content: string,
  analysis: FileAnalysis,
  diagnostics: LintDiagnostic[],
): void {
  const contentLines = content.split('\n');

  for (const imp of analysis.imports) {
    const modPaths = extractModulePaths(imp.raw);
    for (const modPath of modPaths) {
      // Skip relative imports — harder to check
      if (modPath.startsWith('./')) continue;

      const lastSeg = modPath.split('/').pop() || '';
      if (!lastSeg) continue;

      // Check if the module's last segment or full path appears after the import line
      const afterImport = contentLines.slice(imp.line).join('\n');
      if (!afterImport.includes(lastSeg) && !afterImport.includes(modPath)) {
        diagnostics.push({
          line: imp.line,
          severity: 'warning',
          code: 'possibly-unused-import',
          message: `Import ${modPath} may be unused`,
        });
      }
    }
  }
}

function checkDuplicateDefinitions(
  analysis: FileAnalysis,
  diagnostics: LintDiagnostic[],
): void {
  const seen = new Map<string, number>();
  for (const def of analysis.definitions) {
    if (seen.has(def.name)) {
      diagnostics.push({
        line: def.line,
        severity: 'warning',
        code: 'duplicate-definition',
        message: `"${def.name}" already defined at line ${seen.get(def.name)}`,
      });
    } else {
      seen.set(def.name, def.line);
    }
  }
}

function checkStyleIssues(
  lines: string[],
  diagnostics: LintDiagnostic[],
): void {
  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trimStart();
    const lineNum = i + 1;

    // Skip comments
    if (trimmed.startsWith(';')) continue;

    // Prefer def over define
    if (trimmed.startsWith('(define ') && !trimmed.startsWith('(define-')) {
      diagnostics.push({
        line: lineNum,
        severity: 'info',
        code: 'style-prefer-def',
        message: 'Prefer "def" over "define" (supports optional/keyword args)',
      });
    }

    // defstruct without transparent:
    if (trimmed.startsWith('(defstruct ') && !trimmed.includes('transparent:')) {
      const lookAhead = lines
        .slice(i, Math.min(i + 5, lines.length))
        .join(' ');
      if (!lookAhead.includes('transparent:')) {
        diagnostics.push({
          line: lineNum,
          severity: 'info',
          code: 'style-missing-transparent',
          message:
            'Consider adding transparent: #t to defstruct for debugging/printing',
        });
      }
    }
  }
}

function checkShadowedBindings(
  analysis: FileAnalysis,
  diagnostics: LintDiagnostic[],
): void {
  for (const def of analysis.definitions) {
    if (COMMON_STDLIB.has(def.name)) {
      diagnostics.push({
        line: def.line,
        severity: 'warning',
        code: 'shadowed-binding',
        message: `"${def.name}" shadows a common standard binding`,
      });
    }
  }
}
