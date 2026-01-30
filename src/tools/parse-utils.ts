import { readdir, stat } from 'node:fs/promises';
import { join } from 'node:path';

// ── Definition Parsing ──────────────────────────────────────────────

export interface SymbolDefinition {
  name: string;
  kind: string;
  line: number;
}

export interface ImportInfo {
  raw: string;
  line: number;
}

export interface ExportInfo {
  raw: string;
  line: number;
}

export interface FileAnalysis {
  definitions: SymbolDefinition[];
  imports: ImportInfo[];
  exports: ExportInfo[];
}

const DEF_PATTERNS: Array<{ keyword: string; kind: string }> = [
  { keyword: 'def*', kind: 'procedure' },
  { keyword: 'def/c', kind: 'procedure' },
  { keyword: 'defstruct', kind: 'struct' },
  { keyword: 'defclass', kind: 'class' },
  { keyword: 'definterface', kind: 'interface' },
  { keyword: 'defrules', kind: 'macro' },
  { keyword: 'defrule', kind: 'macro' },
  { keyword: 'defsyntax-call', kind: 'macro' },
  { keyword: 'defsyntax', kind: 'macro' },
  { keyword: 'defmacro', kind: 'macro' },
  { keyword: 'defmethod', kind: 'method' },
  { keyword: 'defconst', kind: 'constant' },
  { keyword: 'definline', kind: 'inline' },
  { keyword: 'defvalues', kind: 'values' },
  { keyword: 'defalias', kind: 'alias' },
  { keyword: 'deftype', kind: 'type' },
  { keyword: 'define', kind: 'procedure' },
  { keyword: 'def', kind: 'procedure' },
];

/**
 * Parse definitions, imports, and exports from Gerbil source text.
 * Pure TypeScript line-based parsing — fast, no subprocess needed.
 */
export function parseDefinitions(content: string): FileAnalysis {
  const lines = content.split('\n');
  const definitions: SymbolDefinition[] = [];
  const imports: ImportInfo[] = [];
  const exports: ExportInfo[] = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const trimmed = line.trimStart();
    const lineNum = i + 1;

    if (!trimmed || trimmed.startsWith(';') || trimmed.startsWith('#|')) {
      continue;
    }

    if (trimmed.startsWith('(import ') || trimmed === '(import') {
      imports.push({ raw: extractForm(lines, i), line: lineNum });
      continue;
    }

    if (trimmed.startsWith('(export ') || trimmed === '(export') {
      exports.push({ raw: extractForm(lines, i), line: lineNum });
      continue;
    }

    for (const { keyword, kind } of DEF_PATTERNS) {
      const prefix = `(${keyword} `;
      if (trimmed.startsWith(prefix)) {
        const rest = trimmed.slice(prefix.length);
        const name = extractDefName(rest, keyword);
        if (name) {
          definitions.push({ name, kind, line: lineNum });
        }
        break;
      }
    }
  }

  return { definitions, imports, exports };
}

function extractDefName(rest: string, keyword: string): string | null {
  const trimmed = rest.trimStart();
  if (!trimmed) return null;

  if (trimmed.startsWith('(')) {
    const inner = trimmed.slice(1);
    return readToken(inner) || null;
  }

  if (trimmed.startsWith('{')) {
    const inner = trimmed.slice(1);
    // For defmethod {name type}, extract the first token inside braces
    const token = readToken(inner);
    if (keyword === 'defmethod' && token) {
      // Could be {:name type} — strip leading colon if present
      return token.startsWith(':') ? token.slice(1) : token;
    }
    return token || null;
  }

  return readToken(trimmed) || null;
}

function readToken(text: string): string {
  const delimiters = new Set([
    ' ', '\t', '\n', '\r', '(', ')', '[', ']', '{', '}',
    '"', "'", '`', ',', ';',
  ]);
  let i = 0;
  while (i < text.length && !delimiters.has(text[i])) {
    i++;
  }
  return text.slice(0, i);
}

function extractForm(lines: string[], startIdx: number): string {
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

// ── File System Scanning ────────────────────────────────────────────

const SKIP_DIRS = new Set([
  '.git', '.svn', 'node_modules', '.gerbil', '__pycache__', 'dist',
]);

/**
 * Recursively scan a directory for .ss and .scm files.
 */
export async function scanSchemeFiles(directory: string): Promise<string[]> {
  const results: string[] = [];
  await scanDir(directory, results);
  return results.sort();
}

async function scanDir(dir: string, results: string[]): Promise<void> {
  let entries: string[];
  try {
    entries = await readdir(dir);
  } catch {
    return;
  }

  for (const entry of entries) {
    if (entry.startsWith('.') || SKIP_DIRS.has(entry)) continue;

    const fullPath = join(dir, entry);
    try {
      const info = await stat(fullPath);
      if (info.isDirectory()) {
        await scanDir(fullPath, results);
      } else if (entry.endsWith('.ss') || entry.endsWith('.scm')) {
        results.push(fullPath);
      }
    } catch {
      // skip inaccessible entries
    }
  }
}

// ── Scheme Word Boundary Detection ──────────────────────────────────

const SCHEME_DELIMITERS = new Set([
  ' ', '\t', '\n', '\r', '(', ')', '[', ']', '{', '}',
  '"', "'", '`', ',', ';', '#',
]);

export function isSchemeDelimiter(ch: string): boolean {
  return SCHEME_DELIMITERS.has(ch);
}

/**
 * Find all occurrences of a symbol in text with word-boundary checking.
 * Skips matches inside comments and string literals (heuristic).
 */
export function findSymbolOccurrences(
  content: string,
  symbol: string,
): Array<{ line: number; column: number; lineText: string }> {
  const results: Array<{ line: number; column: number; lineText: string }> = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    let searchFrom = 0;
    while (true) {
      const idx = line.indexOf(symbol, searchFrom);
      if (idx === -1) break;

      const leftOk = idx === 0 || isSchemeDelimiter(line[idx - 1]);
      const rightIdx = idx + symbol.length;
      const rightOk =
        rightIdx >= line.length || isSchemeDelimiter(line[rightIdx]);

      if (leftOk && rightOk) {
        const beforeMatch = line.slice(0, idx);
        const inComment = beforeMatch.includes(';');
        const quoteCount = (beforeMatch.match(/"/g) || []).length;
        const inString = quoteCount % 2 !== 0;

        if (!inComment && !inString) {
          results.push({ line: i + 1, column: idx + 1, lineText: line });
        }
      }

      searchFrom = idx + 1;
    }
  }

  return results;
}

// ── gxc Error Parsing ───────────────────────────────────────────────

export interface Diagnostic {
  file: string;
  line: number | null;
  column: number | null;
  severity: 'error' | 'warning' | 'info';
  message: string;
}

/**
 * Parse gxc/gxi stderr output into structured diagnostics.
 */
export function parseGxcErrors(
  stderr: string,
  defaultFile?: string,
): Diagnostic[] {
  const diagnostics: Diagnostic[] = [];
  const lines = stderr.split('\n');

  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed) continue;

    // Pattern 1: /path/file.ss:LINE:COL: message
    const match1 = trimmed.match(/^(.+\.ss?):(\d+):(\d+):\s*(.+)$/);
    if (match1) {
      diagnostics.push({
        file: match1[1],
        line: parseInt(match1[2], 10),
        column: parseInt(match1[3], 10),
        severity: classifySeverity(match1[4]),
        message: match1[4],
      });
      continue;
    }

    // Pattern 2: "file.ss"@LINE.COL -- message  (Gambit format)
    const match2 = trimmed.match(/"([^"]+)"@(\d+)\.(\d+)\s+--\s+(.+)$/);
    if (match2) {
      diagnostics.push({
        file: match2[1],
        line: parseInt(match2[2], 10),
        column: parseInt(match2[3], 10),
        severity: 'error',
        message: match2[4],
      });
      continue;
    }

    // Pattern 3: *** ERROR ... -- message
    const match3 = trimmed.match(
      /^\*\*\*\s+ERROR\s+(?:IN\s+.+?\s+)?--\s*(.+)$/,
    );
    if (match3) {
      diagnostics.push({
        file: defaultFile || '<unknown>',
        line: null,
        column: null,
        severity: 'error',
        message: match3[1],
      });
      continue;
    }

    // Pattern 4: *** WARNING -- message
    const match4 = trimmed.match(/^\*\*\*\s+WARNING\s+--\s*(.+)$/);
    if (match4) {
      diagnostics.push({
        file: defaultFile || '<unknown>',
        line: null,
        column: null,
        severity: 'warning',
        message: match4[1],
      });
      continue;
    }

    // Fallback: unrecognized error line
    if (trimmed.startsWith('***') || trimmed.startsWith('Error')) {
      diagnostics.push({
        file: defaultFile || '<unknown>',
        line: null,
        column: null,
        severity: 'error',
        message: trimmed,
      });
    }
  }

  return diagnostics;
}

function classifySeverity(message: string): 'error' | 'warning' | 'info' {
  const lower = message.toLowerCase();
  if (lower.includes('warning')) return 'warning';
  if (lower.includes('info') || lower.includes('note')) return 'info';
  return 'error';
}

// ── Build Script Parsing ────────────────────────────────────────────

export interface BuildTargets {
  libraries: string[];
  executables: Array<{ module: string; binary: string }>;
}

/**
 * Parse build.ss defbuild-script content to extract build targets.
 */
export function parseBuildTargets(content: string): BuildTargets {
  const libraries: string[] = [];
  const executables: Array<{ module: string; binary: string }> = [];

  // Extract the quoted list from defbuild-script '(...)
  const listMatch = content.match(/defbuild-script\s*\n?\s*'?\(([^]*)\)/);
  if (!listMatch) return { libraries, executables };

  const body = listMatch[1];

  // Match executable entries: (exe: "module" bin: "binary")
  const exePattern = /\(exe:\s*"([^"]+)"\s+bin:\s*"([^"]+)"\)/g;
  let exeMatch;
  while ((exeMatch = exePattern.exec(body)) !== null) {
    executables.push({ module: exeMatch[1], binary: exeMatch[2] });
  }

  // Match plain string entries (library modules)
  const strPattern = /"([^"]+)"/g;
  let strMatch;
  // Reset to scan from beginning for all strings
  const allStrings: string[] = [];
  while ((strMatch = strPattern.exec(body)) !== null) {
    allStrings.push(strMatch[1]);
  }

  // Library strings are those not part of exe entries
  const exeModules = new Set(executables.map((e) => e.module));
  const exeBinaries = new Set(executables.map((e) => e.binary));
  for (const s of allStrings) {
    if (!exeModules.has(s) && !exeBinaries.has(s)) {
      libraries.push(s);
    }
  }

  return { libraries, executables };
}

/**
 * Extract module paths from an import form text.
 */
export function extractModulePaths(importText: string): string[] {
  const absolute = importText.match(/:[a-zA-Z][a-zA-Z0-9/_-]*/g) || [];
  const relative = importText.match(/\.\/[a-zA-Z][a-zA-Z0-9/_-]*/g) || [];
  return [...absolute, ...relative];
}
