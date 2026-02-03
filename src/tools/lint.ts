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
        'hash literal symbol key warnings, channel anti-patterns (spinning try-get, ' +
        'wg-wait visibility gaps), and compilation errors via gxc.',
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
      checkHashLiteralKeys(lines, diagnostics);
      checkChannelPatterns(lines, diagnostics);

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

function extractFormFromLines(lines: string[], startIdx: number): string {
  let depth = 0;
  let result = '';
  for (let i = startIdx; i < lines.length; i++) {
    const line = lines[i];
    result += (i > startIdx ? '\n' : '') + line;
    for (const ch of line) {
      if (ch === '(' || ch === '[') depth++;
      else if (ch === ')' || ch === ']') depth--;
    }
    if (depth <= 0 && result.includes('(')) break;
  }
  return result;
}

/**
 * Extract top-level entries from a (hash ...) form.
 * Returns the key token of each (key value) pair at depth 1.
 */
function extractHashEntries(form: string): string[] {
  const keys: string[] = [];
  // Skip past the opening "(hash" prefix
  const start = form.indexOf('(hash');
  if (start === -1) return keys;
  let pos = start + 5; // skip "(hash"
  const len = form.length;

  while (pos < len) {
    const ch = form[pos];
    // Skip whitespace
    if (/\s/.test(ch)) { pos++; continue; }
    // End of hash form
    if (ch === ')') break;
    // Start of an entry: (key value)
    if (ch === '(') {
      pos++; // skip '('
      // Skip whitespace before key
      while (pos < len && /\s/.test(form[pos])) pos++;
      // Read the key token
      const keyStart = pos;
      while (pos < len && !/[\s()]/.test(form[pos])) pos++;
      const key = form.slice(keyStart, pos);
      if (key.length > 0) keys.push(key);
      // Skip the rest of this entry by tracking depth
      let depth = 1;
      while (pos < len && depth > 0) {
        if (form[pos] === '(' || form[pos] === '[') depth++;
        else if (form[pos] === ')' || form[pos] === ']') depth--;
        // Skip string literals to avoid counting parens inside them
        if (form[pos] === '"') {
          pos++;
          while (pos < len && form[pos] !== '"') {
            if (form[pos] === '\\') pos++; // skip escaped char
            pos++;
          }
        }
        pos++;
      }
    } else {
      // Skip any other token (shouldn't happen in well-formed hash)
      pos++;
    }
  }
  return keys;
}

/**
 * Check if a token looks like a bare identifier (symbol key) in a hash literal.
 * Returns false for strings, numbers, booleans, keywords, and characters.
 */
function isBareIdentifier(token: string): boolean {
  if (token.length === 0) return false;
  // String literal
  if (token.startsWith('"')) return false;
  // Number (including negative)
  if (/^-?[0-9]/.test(token)) return false;
  // Boolean
  if (token === '#t' || token === '#f') return false;
  // Keyword (ends with colon)
  if (token.endsWith(':')) return false;
  // Character literal
  if (token.startsWith('#\\')) return false;
  // Quoted symbol
  if (token.startsWith("'")) return false;
  return true;
}

function checkHashLiteralKeys(
  lines: string[],
  diagnostics: LintDiagnostic[],
): void {
  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trimStart();
    const lineNum = i + 1;

    // Skip comments
    if (trimmed.startsWith(';')) continue;

    // Match (hash ...) but not (hash-eq ...), (hash-eqv ...), (hash-ref ...), etc.
    if (!trimmed.match(/^\(hash[\s\n)]/)) continue;

    // Extract the full form spanning multiple lines
    const form = extractFormFromLines(lines, i);

    // Extract entry keys and check for bare identifiers
    const keys = extractHashEntries(form);
    for (const key of keys) {
      if (isBareIdentifier(key)) {
        diagnostics.push({
          line: lineNum,
          severity: 'warning',
          code: 'hash-symbol-key',
          message:
            `Hash literal uses bare symbol key '${key}' — this creates a symbol key, not a string. ` +
            `Use ("${key}" ...) for string keys, or hash-eq for intentional symbol keys.`,
        });
      }
    }
  }
}

/**
 * Check if a line index is inside a loop construct by scanning backward.
 * Stops at top-level definition boundaries (def, def*, defmethod, etc.).
 */
function isInsideLoop(lines: string[], targetIdx: number): boolean {
  const loopPattern =
    /\(\s*(while|until|do|for|for\/collect|for\/fold)\s/;
  const namedLetPattern = /\(\s*let\s+\S+\s+\(/;
  const defBoundary = /^\s*\(\s*(def\b|def\*|defmethod|defclass|defstruct)/;

  for (let i = targetIdx; i >= 0; i--) {
    const line = lines[i];
    // Stop scanning at top-level definition boundaries
    if (defBoundary.test(line) && i < targetIdx) return false;
    if (loopPattern.test(line)) return true;
    if (namedLetPattern.test(line)) return true;
  }
  return false;
}

function checkChannelPatterns(
  lines: string[],
  diagnostics: LintDiagnostic[],
): void {
  const defBoundary = /^\s*\(\s*(def\b|def\*|defmethod|defclass|defstruct)/;

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trimStart();
    const lineNum = i + 1;

    // Skip comments
    if (trimmed.startsWith(';')) continue;

    // Rule 1: channel-try-get inside a loop
    if (trimmed.includes('channel-try-get')) {
      if (isInsideLoop(lines, i)) {
        diagnostics.push({
          line: lineNum,
          severity: 'warning',
          code: 'channel-try-get-in-loop',
          message:
            'channel-try-get inside a loop spins the CPU. ' +
            'Use channel-get (blocking) or add a sleep/timeout.',
        });
      }
    }

    // Rule 2: wg-wait! followed by channel-try-get
    if (trimmed.includes('wg-wait!')) {
      const lookAheadEnd = Math.min(i + 15, lines.length);
      for (let j = i + 1; j < lookAheadEnd; j++) {
        // Stop at definition boundaries
        if (defBoundary.test(lines[j])) break;
        if (lines[j].includes('channel-try-get')) {
          diagnostics.push({
            line: lineNum,
            severity: 'warning',
            code: 'wg-wait-then-try-get',
            message:
              'wg-wait! followed by channel-try-get may miss values. ' +
              'Workers may not have put to the channel yet when wg signals. ' +
              'Use channel-get (blocking) or collect results before wg-wait!.',
          });
          break;
        }
      }
    }
  }
}
