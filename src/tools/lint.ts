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
        'wg-wait visibility gaps), pitfall detection (unquote outside quasiquote, ' +
        'dot in brackets, missing exported definitions), SRFI-19 time->seconds shadow, ' +
        'unsafe mutex-lock!/unlock! without unwind-protect, and compilation errors via gxc.',
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
      checkUnquoteOutsideQuasiquote(lines, diagnostics);
      checkDotInBrackets(lines, diagnostics);
      checkMissingExportedDefinitions(analysis, diagnostics);
      checkSrfi19TimeShadow(content, analysis, diagnostics);
      checkUnsafeMutexPattern(lines, diagnostics);

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

/**
 * Detect `,expr` or `,@expr` that appear outside quasiquote context.
 * Scans for unquote (comma) at Scheme word boundaries, not inside strings,
 * and checks whether a quasiquote (backtick) is active in preceding lines.
 */
function checkUnquoteOutsideQuasiquote(
  lines: string[],
  diagnostics: LintDiagnostic[],
): void {
  // Track quasiquote depth across lines (heuristic: look for backtick starts)
  let qqDepth = 0;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const lineNum = i + 1;
    const trimmed = line.trimStart();

    // Skip comments
    if (trimmed.startsWith(';')) continue;

    // Track quasiquote nesting (simple heuristic)
    let inString = false;
    for (let j = 0; j < line.length; j++) {
      const ch = line[j];

      // Skip string contents
      if (ch === '"' && (j === 0 || line[j - 1] !== '\\')) {
        inString = !inString;
        continue;
      }
      if (inString) continue;

      // Skip after semicolon (comment)
      if (ch === ';') break;

      if (ch === '`') {
        qqDepth++;
      } else if (ch === ',' && qqDepth > 0) {
        qqDepth--;
      }
    }

    // Reset at top-level forms (heuristic: line starts with `(`)
    if (trimmed.startsWith('(') && !trimmed.startsWith('(`') && qqDepth > 0) {
      // Check if this looks like a top-level definition
      if (
        /^\((def\b|defstruct|defclass|defmethod|defrules?|defsyntax|export|import)/.test(
          trimmed,
        )
      ) {
        qqDepth = 0;
      }
    }

    // Now check for commas that aren't inside quasiquote
    inString = false;
    for (let j = 0; j < line.length; j++) {
      const ch = line[j];

      if (ch === '"' && (j === 0 || line[j - 1] !== '\\')) {
        inString = !inString;
        continue;
      }
      if (inString) continue;

      if (ch === ';') break;

      if (ch === ',') {
        // Check if preceded by a Scheme delimiter or start of line
        const leftOk = j === 0 || /[\s([\]{}'`]/.test(line[j - 1]);
        if (!leftOk) continue;

        // Check that something follows (not just whitespace to EOL)
        const rest = line.slice(j + 1).trimStart();
        if (!rest || rest.startsWith(';')) continue;

        // Skip if it looks like it's a comma in a hash literal key-value form
        // e.g., (hash ("key" , value)) — this is unusual but not unquote
        // Actually, in Scheme `,` is always unquote syntax

        // Check if we're inside a quasiquote context by scanning backward
        let inQQ = false;
        // Scan backward on this line for backtick
        for (let k = j - 1; k >= 0; k--) {
          if (line[k] === '`') {
            inQQ = true;
            break;
          }
        }

        // Also check if a previous line's quasiquote is still open
        if (!inQQ && qqDepth <= 0) {
          // Scan back a few lines for an open quasiquote
          let foundQQ = false;
          for (let back = i - 1; back >= Math.max(0, i - 20); back--) {
            const prevLine = lines[back].trimStart();
            if (prevLine.startsWith(';')) continue;
            if (
              /^\((def\b|defstruct|defclass|defmethod|defrules?|defsyntax|export|import)/.test(
                prevLine,
              )
            ) {
              break; // Hit a top-level form boundary
            }
            if (prevLine.includes('`')) {
              foundQQ = true;
              break;
            }
          }

          if (!foundQQ) {
            const snippet = line.slice(j, Math.min(j + 20, line.length));
            diagnostics.push({
              line: lineNum,
              severity: 'warning',
              code: 'unquote-outside-quasiquote',
              message:
                `Unquote "${snippet.trim()}" appears outside quasiquote context. ` +
                'Did you mean to use a backtick ` template?',
            });
          }
        }
      }
    }
  }
}

/**
 * Detect `. ` (dot-space) inside `[...]` constructs.
 * Gerbil's `[]` is list sugar, so `[a . b]` is almost always a mistake —
 * the user likely meant `(cons a b)` or `(a . b)` with parentheses.
 */
function checkDotInBrackets(
  lines: string[],
  diagnostics: LintDiagnostic[],
): void {
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const lineNum = i + 1;
    const trimmed = line.trimStart();

    // Skip comments
    if (trimmed.startsWith(';')) continue;

    // Quick check: does this line contain both `[` and `. `?
    if (!line.includes('[') || !line.includes('. ')) continue;

    // Scan character by character tracking bracket depth and string state
    let inString = false;
    let bracketDepth = 0;

    for (let j = 0; j < line.length; j++) {
      const ch = line[j];

      if (ch === '"' && (j === 0 || line[j - 1] !== '\\')) {
        inString = !inString;
        continue;
      }
      if (inString) continue;

      if (ch === ';') break; // Rest is comment

      if (ch === '[') {
        bracketDepth++;
      } else if (ch === ']') {
        bracketDepth--;
      } else if (
        ch === '.' &&
        bracketDepth > 0 &&
        j + 1 < line.length &&
        line[j + 1] === ' '
      ) {
        // Check it's at a word boundary (not part of a symbol like `foo.bar`)
        const leftOk = j === 0 || /[\s([\]{}]/.test(line[j - 1]);
        if (leftOk) {
          diagnostics.push({
            line: lineNum,
            severity: 'warning',
            code: 'dot-in-brackets',
            message:
              'Dotted pair inside [...] brackets. ' +
              'Gerbil [] is list sugar, not cons syntax. ' +
              'Use (a . b) with parentheses for dotted pairs, or [a b] for a list.',
          });
          break; // One warning per line is enough
        }
      }
    }
  }
}

/**
 * Detect time->seconds usage in files that import :std/srfi/19.
 * SRFI-19's current-time shadows Gambit's current-time, but time->seconds
 * remains Gambit's version. Passing an SRFI-19 time object to Gambit's
 * time->seconds causes a runtime type error. This is invisible at compile
 * time and can cause cascading deadlocks if inside a mutex-protected section.
 */
function checkSrfi19TimeShadow(
  content: string,
  analysis: FileAnalysis,
  diagnostics: LintDiagnostic[],
): void {
  // Check if file imports :std/srfi/19
  const hasSrfi19 = analysis.imports.some((imp) =>
    imp.raw.includes(':std/srfi/19'),
  );
  if (!hasSrfi19) return;

  const lines = content.split('\n');
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const lineNum = i + 1;
    const trimmed = line.trimStart();

    // Skip comments and import lines
    if (trimmed.startsWith(';')) continue;
    if (trimmed.startsWith('(import ')) continue;

    // Check for time->seconds usage
    let searchFrom = 0;
    while (true) {
      const idx = line.indexOf('time->seconds', searchFrom);
      if (idx === -1) break;

      // Word boundary check
      const leftOk = idx === 0 || /[\s([\]{}'`,;]/.test(line[idx - 1]);
      const rightIdx = idx + 'time->seconds'.length;
      const rightOk =
        rightIdx >= line.length || /[\s)[\]{}'`,;]/.test(line[rightIdx]);

      if (leftOk && rightOk) {
        // Check it's not inside a string or comment
        const before = line.slice(0, idx);
        const inComment = before.includes(';');
        const quoteCount = (before.match(/"/g) || []).length;
        const inString = quoteCount % 2 !== 0;

        if (!inComment && !inString) {
          diagnostics.push({
            line: lineNum,
            severity: 'warning',
            code: 'srfi19-time-seconds-shadow',
            message:
              'time->seconds with :std/srfi/19 import is likely a bug. ' +
              "SRFI-19's current-time shadows Gambit's, but time->seconds remains " +
              "Gambit's version and will fail on SRFI-19 time objects. " +
              'Use time-second for SRFI-19, or ##current-time for Gambit timing.',
          });
        }
      }
      searchFrom = idx + 1;
    }
  }
}

/**
 * Detect mutex-lock!/mutex-unlock! pairs not wrapped in unwind-protect
 * or dynamic-wind. If an exception occurs between lock and unlock, the
 * mutex stays permanently locked, causing deadlocks for all subsequent callers.
 */
function checkUnsafeMutexPattern(
  lines: string[],
  diagnostics: LintDiagnostic[],
): void {
  const defBoundary = /^\s*\(\s*(def\b|def\*|defmethod|defclass|defstruct)/;

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trimStart();
    const lineNum = i + 1;

    // Skip comments
    if (trimmed.startsWith(';')) continue;

    if (!trimmed.includes('mutex-lock!')) continue;

    // Check it's not inside a string or comment
    const before = trimmed.slice(0, trimmed.indexOf('mutex-lock!'));
    if (before.includes(';')) continue;
    const quoteCount = (before.match(/"/g) || []).length;
    if (quoteCount % 2 !== 0) continue;

    // Scan backward to check if we're inside an unwind-protect or dynamic-wind
    let protected_ = false;
    for (let j = i - 1; j >= Math.max(0, i - 30); j--) {
      const prev = lines[j].trimStart();
      // Stop at definition boundaries
      if (defBoundary.test(prev) && j < i) break;
      if (prev.includes('unwind-protect') || prev.includes('dynamic-wind')) {
        protected_ = true;
        break;
      }
    }

    if (protected_) continue;

    // Scan forward for a matching mutex-unlock! in the same scope
    let hasUnlock = false;
    for (let j = i + 1; j < Math.min(lines.length, i + 30); j++) {
      if (defBoundary.test(lines[j])) break;
      if (lines[j].includes('mutex-unlock!')) {
        hasUnlock = true;
        break;
      }
      // Also check if unwind-protect appears after the lock (wrapping the body)
      if (lines[j].includes('unwind-protect') || lines[j].includes('dynamic-wind')) {
        protected_ = true;
        break;
      }
    }

    if (protected_) continue;

    if (hasUnlock) {
      diagnostics.push({
        line: lineNum,
        severity: 'warning',
        code: 'unsafe-mutex-pattern',
        message:
          'mutex-lock!/mutex-unlock! without unwind-protect. ' +
          'If an exception occurs between lock and unlock, the mutex stays permanently locked. ' +
          'Wrap the body in (unwind-protect body (mutex-unlock! mx)).',
      });
    }
  }
}

/**
 * Check that all symbols listed in (export ...) forms are actually
 * defined in the file. Catches typos and forgotten definitions.
 */
function checkMissingExportedDefinitions(
  analysis: FileAnalysis,
  diagnostics: LintDiagnostic[],
): void {
  const definedNames = new Set(analysis.definitions.map((d) => d.name));

  for (const exp of analysis.exports) {
    const raw = exp.raw;

    // (export #t) — export everything, skip
    if (raw.includes('#t')) continue;

    // Strip outer (export ...)
    const inner = raw
      .replace(/^\s*\(export\s+/, '')
      .replace(/\)\s*$/, '')
      .trim();

    if (!inner) continue;

    // Extract plain symbol tokens (skip sub-forms like (struct-out ...) etc.)
    let pos = 0;
    while (pos < inner.length) {
      while (pos < inner.length && /\s/.test(inner[pos])) pos++;
      if (pos >= inner.length) break;

      if (inner[pos] === '(') {
        // Skip sub-form
        let depth = 1;
        pos++;
        while (pos < inner.length && depth > 0) {
          if (inner[pos] === '(') depth++;
          else if (inner[pos] === ')') depth--;
          pos++;
        }
      } else {
        // Read plain symbol
        const start = pos;
        while (pos < inner.length && !/[\s()[\]{}]/.test(inner[pos])) pos++;
        const sym = inner.slice(start, pos);
        if (sym && sym !== '#t' && sym !== '#f' && !definedNames.has(sym)) {
          diagnostics.push({
            line: exp.line,
            severity: 'warning',
            code: 'missing-exported-definition',
            message: `Exports "${sym}" but no definition found in this file`,
          });
        }
      }
    }
  }
}
