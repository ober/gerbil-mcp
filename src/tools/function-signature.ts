import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, readdir } from 'node:fs/promises';
import { join, basename } from 'node:path';
import { runGxi, escapeSchemeString, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-SIG:';
const SOURCE_MARKER = 'GERBIL-MCP-SIG-SOURCE:';
const KEYWORDS_MARKER = 'GERBIL-MCP-SIG-KW:';

export function registerFunctionSignatureTool(server: McpServer): void {
  server.registerTool(
    'gerbil_function_signature',
    {
      title: 'Inspect Function Signatures',
      description:
        'Get arity and type info for exported symbols in a Gerbil module. ' +
        'Shows whether each export is a procedure (with parameter names when source is available), macro/syntax, or value. ' +
        'Detects keyword arguments at runtime even when source is unavailable. ' +
        'Example: module_path ":std/text/json" returns all exports with their signatures. ' +
        'Optionally filter to a single symbol.',
      inputSchema: {
        module_path: z
          .string()
          .describe('Module path (e.g. ":std/text/json", ":std/iter")'),
        symbol: z
          .string()
          .optional()
          .describe(
            'Specific symbol to inspect. If omitted, inspects all exports.',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution ' +
            '(e.g. ["/path/to/project/.gerbil/lib"])',
          ),
        project_path: z
          .string()
          .optional()
          .describe(
            'Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib',
          ),
      },
    },
    async ({ module_path, symbol, loadpath, project_path }) => {
      const modPath = module_path.startsWith(':')
        ? module_path
        : `:${module_path}`;

      const filterExpr = symbol
        ? `(let ((target "${escapeSchemeString(symbol)}")) (lambda (name) (string=? (symbol->string name) target)))`
        : '(lambda (name) #t)';

      const exprs = [
        `(import :gerbil/expander)`,
        `(import ${modPath})`,
        [
          '(with-catch',
          '  (lambda (e)',
          `    (display "${ERROR_MARKER}\\n")`,
          '    (display-exception e (current-output-port)))',
          '  (lambda ()',
          `    (let* ((mod (import-module (quote ${modPath}) #f #t))`,
          '           (exports (module-context-export mod))',
          `           (filter-fn ${filterExpr}))`,
          // Emit source path for the module
          '      (with-catch (lambda (e) (void))',
          '        (lambda ()',
          `          (let ((resolved (core-resolve-library-module-path (quote ${modPath}))))`,
          '            (let ((ss-path (string-append (path-strip-extension resolved) ".ss")))',
          '              (when (file-exists? ss-path)',
          `                (display "${SOURCE_MARKER}")`,
          '                (display ss-path)',
          '                (newline))))))',
          '      (for-each',
          '        (lambda (e)',
          '          (let ((name (module-export-name e)))',
          '            (when (filter-fn name)',
          '              (with-catch',
          '                (lambda (ex)',
          `                  (display "${RESULT_MARKER}")`,
          '                  (display name)',
          '                  (display "\\tmacro/syntax\\t\\t")',
          '                  (newline))',
          '                (lambda ()',
          '                  (let ((val (eval name)))',
          '                    (cond',
          '                      ((procedure? val)',
          `                       (display "${RESULT_MARKER}")`,
          '                       (display name)',
          '                       (display "\\tprocedure\\t")',
          '                       (display (##subprocedure-nb-parameters val))',
          '                       (display "\\t")',
          '                       (display (##procedure-name val))',
          // Extract keyword args from ##subprocedure-info using built-in string ops
          '                       (with-catch (lambda (_) (void))',
          '                         (lambda ()',
          '                           (let* ((info (##subprocedure-info val))',
          '                                  (info-str (with-output-to-string (lambda () (write info))))',
          '                                  (marker "keyword-dispatch \'#(")',
          '                                  (idx (string-contains info-str marker)))',
          '                             (when idx',
          '                               (let* ((start (+ idx (string-length marker)))',
          '                                      (end-idx (string-index info-str #\\) start))',
          '                                      (content (substring info-str start end-idx))',
          '                                      (tokens (string-split content #\\space))',
          '                                      (kws (filter (lambda (t) (and (not (string=? t "#f")) (not (string=? t "")))) tokens)))',
          '                                 (when (pair? kws)',
          '                                   (display "\\t")',
          '                                   (for-each (lambda (k) (display k) (display " ")) kws)))))))',
          '                       (newline))',
          '                      (else',
          `                       (display "${RESULT_MARKER}")`,
          '                       (display name)',
          '                       (display "\\tvalue\\t\\t")',
          '                       (display (type-of val))',
          '                       (newline))))))',
          '            )))',
          '        exports))))',
        ].join(' '),
      ];

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const env = effectiveLoadpath.length > 0 ? buildLoadpathEnv(effectiveLoadpath) : undefined;

      const result = await runGxi(exprs, { env });

      if (result.timedOut) {
        return {
          content: [
            { type: 'text' as const, text: 'Signature introspection timed out.' },
          ],
          isError: true,
        };
      }

      if (result.exitCode !== 0 && result.stderr) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to load module ${modPath}:\n${result.stderr.trim()}`,
            },
          ],
          isError: true,
        };
      }

      const stdout = result.stdout;
      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        return {
          content: [
            {
              type: 'text' as const,
              text: `Error inspecting ${modPath}:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Extract source path if available
      const sourceLine = stdout
        .split('\n')
        .find((l) => l.startsWith(SOURCE_MARKER));
      const sourcePath = sourceLine
        ? sourceLine.slice(SOURCE_MARKER.length).trim()
        : null;

      // Read source file for formal parameter extraction
      let sourceContent: string | null = null;
      if (sourcePath) {
        try {
          sourceContent = await readFile(sourcePath, 'utf-8');
        } catch {
          // Source not readable — fall back to arity
        }
      }

      // Parse result lines
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      if (lines.length === 0) {
        const msg = symbol
          ? `Symbol "${symbol}" not found in ${modPath} exports.`
          : `No exports found in ${modPath}.`;
        return { content: [{ type: 'text' as const, text: msg }] };
      }

      const entries = lines.map((line) => {
        const parts = line.slice(RESULT_MARKER.length).split('\t');
        const name = parts[0] || '';
        const kind = parts[1] || '';
        const arity = parts[2] || '';
        const qualifiedName = parts[3] || '';
        const keywords = parts[4]?.trim() || '';
        return { name, kind, arity, qualifiedName, keywords };
      });

      // Try to extract formals from source for procedure entries
      const formalsMap = new Map<string, string>();
      if (sourceContent) {
        for (const e of entries) {
          if (e.kind === 'procedure') {
            const formals = extractFormals(sourceContent, e.name);
            if (formals) {
              formalsMap.set(e.name, formals);
            }
          }
        }
      }

      // For procedures without source-level formals and without runtime keywords,
      // try scanning compiled .scm artifacts for keyword/optional arg patterns
      const needsCompiledScan = entries.some(
        (e) => e.kind === 'procedure' && !formalsMap.has(e.name) && !e.keywords,
      );
      if (needsCompiledScan) {
        const compiledKws = await scanCompiledArtifacts(modPath, effectiveLoadpath);
        for (const e of entries) {
          if (e.kind === 'procedure' && !formalsMap.has(e.name) && !e.keywords) {
            const kw = compiledKws.get(e.name);
            if (kw) {
              e.keywords = kw;
            }
          }
        }
      }

      const formatted = [
        `${modPath} — ${entries.length} export(s):`,
        '',
        ...entries.map((e) => {
          if (e.kind === 'procedure') {
            let sig = formalsMap.get(e.name);
            if (!sig && e.keywords) {
              // Build signature from arity + keyword args
              const kwList = e.keywords.split(/\s+/).filter(Boolean);
              sig = `(... keywords: [${kwList.join(' ')}])`;
            }
            if (!sig) {
              sig = `arity:${e.arity}`;
            }
            return `  ${e.name}  procedure  ${sig}  (${e.qualifiedName})`;
          } else if (e.kind === 'macro/syntax') {
            return `  ${e.name}  macro/syntax`;
          } else {
            return `  ${e.name}  ${e.kind}  ${e.qualifiedName}`;
          }
        }),
      ].join('\n');

      return { content: [{ type: 'text' as const, text: formatted }] };
    },
  );
}

/**
 * Scan compiled .scm artifacts in $GERBIL_PATH/lib/static/ for keyword-dispatch
 * patterns and optional/rest argument info. Falls back to loadpath directories.
 * Returns a map of symbol-name -> keyword-string.
 */
async function scanCompiledArtifacts(
  modPath: string,
  loadpath: string[],
): Promise<Map<string, string>> {
  const result = new Map<string, string>();

  // Convert module path to potential file name
  // :std/net/request -> std/net/request__0.scm, std/net/request__rt.scm, etc.
  const modSuffix = modPath.replace(/^:/, '').replace(/\//g, '_');

  // Candidate directories
  const gerbilPath = process.env['GERBIL_PATH'] || join(process.env['HOME'] || '', '.gerbil');
  const dirs = [
    join(gerbilPath, 'lib', 'static'),
    ...loadpath.map((lp) => join(lp, '..', 'static')),
  ];

  for (const dir of dirs) {
    let entries: string[];
    try {
      entries = await readdir(dir);
    } catch {
      continue;
    }

    // Find .scm files matching the module pattern
    const matching = entries.filter(
      (e) => e.endsWith('.scm') && e.startsWith(modSuffix),
    );

    for (const file of matching) {
      let content: string;
      try {
        content = await readFile(join(dir, file), 'utf-8');
      } catch {
        continue;
      }

      // Scan for keyword-dispatch patterns:
      // (keyword-dispatch '#(key1: key2: #f) ... name ...)
      const kwPattern = /keyword-dispatch\s+'#\(([^)]+)\)/g;
      let match: RegExpExecArray | null;
      while ((match = kwPattern.exec(content)) !== null) {
        const kwContent = match[1];
        const kws = kwContent
          .split(/\s+/)
          .filter((t) => t.endsWith(':') && t !== '#f');

        if (kws.length === 0) continue;

        // Try to find the associated function name nearby
        // Look for a define pattern before or near this match
        const context = content.slice(Math.max(0, match.index - 200), match.index);
        const nameMatch = context.match(/define\s+\(([^\s()]+)/g);
        if (nameMatch) {
          const lastDef = nameMatch[nameMatch.length - 1];
          const funcName = lastDef.replace(/^define\s+\(/, '');
          result.set(funcName, kws.join(' '));
        }
      }

      // Also scan for ##rest-args-fix patterns which indicate rest parameters
      const restPattern = /##rest-args-fix\s+\d+\s+([^\s()]+)/g;
      while ((match = restPattern.exec(content)) !== null) {
        // Could enhance with rest param info if needed
      }
    }
  }

  return result;
}

/**
 * Extract formal parameter list from a Gerbil source definition.
 * Returns a formatted string like "(x y [port])" or null if not found.
 */
export function extractFormals(
  sourceContent: string,
  symbolName: string,
): string | null {
  const lines = sourceContent.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trimStart();
    if (!trimmed.startsWith('(')) continue;
    const rest = trimmed.slice(1);

    // Match (def (name ...) or (define (name ...)
    for (const kw of ['def ', 'define ']) {
      if (!rest.startsWith(kw)) continue;
      const after = rest.slice(kw.length).trimStart();
      if (!after.startsWith('(')) continue;
      const inner = after.slice(1);

      // Check if the function name matches
      const nameEnd = inner.search(/[\s()[\]{}]/);
      const foundName =
        nameEnd === -1 ? inner : inner.slice(0, nameEnd);
      if (foundName !== symbolName) continue;

      // Extract the formals from this definition
      // Gather the full parameter list (may span multiple lines)
      const formText = gatherFormText(lines, i, after);
      return parseFormalsFromDef(formText);
    }

    // Match (def* name for case-lambda style
    if (rest.startsWith('def* ')) {
      const after = rest.slice(5).trimStart();
      const nameEnd = after.search(/[\s()[\]{}]/);
      const foundName =
        nameEnd === -1 ? after : after.slice(0, nameEnd);
      if (foundName !== symbolName) continue;
      return 'case-lambda';
    }
  }

  return null;
}

/**
 * Gather text from a definition form across multiple lines until we find
 * the closing paren of the parameter list.
 */
function gatherFormText(
  lines: string[],
  startIdx: number,
  startText: string,
): string {
  let text = startText;
  // We need enough text to capture the parameter list
  for (let i = startIdx + 1; i < lines.length && i < startIdx + 20; i++) {
    text += '\n' + lines[i];
    // Check if we've closed the param list paren
    if (hasClosedParamList(text)) break;
  }
  return text;
}

function hasClosedParamList(text: string): boolean {
  // Find the opening ( of the param list and track depth
  let depth = 0;
  let started = false;
  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    if (ch === '"') {
      i++;
      while (i < text.length && text[i] !== '"') {
        if (text[i] === '\\') i++;
        i++;
      }
      continue;
    }
    if (ch === '(' || ch === '[') {
      depth++;
      started = true;
    } else if (ch === ')' || ch === ']') {
      depth--;
      if (started && depth <= 0) return true;
    }
  }
  return false;
}

/**
 * Parse formal parameters from a def-form text like "(name x y [port]) body..."
 * Returns formatted string like "(x y [port])" or null.
 */
function parseFormalsFromDef(text: string): string | null {
  // text starts with "(name params...) body"
  // Find the opening ( of the param list
  const openIdx = text.indexOf('(');
  if (openIdx === -1) return null;

  // Track depth to find the matching )
  let depth = 0;
  let paramEnd = -1;
  for (let i = openIdx; i < text.length; i++) {
    const ch = text[i];
    if (ch === '"') {
      i++;
      while (i < text.length && text[i] !== '"') {
        if (text[i] === '\\') i++;
        i++;
      }
      continue;
    }
    if (ch === '(' || ch === '[') depth++;
    else if (ch === ')' || ch === ']') {
      depth--;
      if (depth === 0) {
        paramEnd = i;
        break;
      }
    }
  }

  if (paramEnd === -1) return null;

  // Extract just the content inside the parens
  const paramContent = text.slice(openIdx + 1, paramEnd);

  // Skip the function name (first token)
  const tokens = tokenizeFormals(paramContent);
  if (tokens.length === 0) return null;

  // First token is the function name, rest are params
  const params = tokens.slice(1);
  if (params.length === 0) return '()';

  return `(${params.join(' ')})`;
}

/**
 * Tokenize a Gerbil formal parameter list, preserving grouped forms like
 * (x default), keyword args like key: (key default), and rest ". rest".
 */
function tokenizeFormals(text: string): string[] {
  const tokens: string[] = [];
  let i = 0;

  while (i < text.length) {
    // Skip whitespace
    while (i < text.length && /\s/.test(text[i])) i++;
    if (i >= text.length) break;

    const ch = text[i];

    if (ch === '(' || ch === '[') {
      // Grouped form — capture the entire (x default) as [x]
      const close = ch === '(' ? ')' : ']';
      let depth = 1;
      let j = i + 1;
      while (j < text.length && depth > 0) {
        if (text[j] === ch) depth++;
        else if (text[j] === close) depth--;
        j++;
      }
      const inner = text.slice(i + 1, j - 1).trim();
      const paramName = inner.split(/\s+/)[0] || inner;
      tokens.push(`[${paramName}]`);
      i = j;
    } else if (ch === '.') {
      // Rest parameter
      i++;
      while (i < text.length && /\s/.test(text[i])) i++;
      let j = i;
      while (j < text.length && !/[\s()[\]{}]/.test(text[j])) j++;
      const restName = text.slice(i, j);
      if (restName) tokens.push(`. ${restName}`);
      i = j;
    } else {
      // Regular token or keyword arg
      let j = i;
      while (j < text.length && !/[\s()[\]{}]/.test(text[j])) j++;
      const token = text.slice(i, j);
      i = j;

      if (token.endsWith(':')) {
        // Keyword arg — next token is the default group
        while (i < text.length && /\s/.test(text[i])) i++;
        if (i < text.length && (text[i] === '(' || text[i] === '[')) {
          const open = text[i];
          const close = open === '(' ? ')' : ']';
          let depth = 1;
          let k = i + 1;
          while (k < text.length && depth > 0) {
            if (text[k] === open) depth++;
            else if (text[k] === close) depth--;
            k++;
          }
          const inner = text.slice(i + 1, k - 1).trim();
          const paramName = inner.split(/\s+/)[0] || inner;
          tokens.push(`${token} [${paramName}]`);
          i = k;
        } else {
          // keyword followed by a plain token
          let k = i;
          while (k < text.length && !/[\s()[\]{}]/.test(text[k])) k++;
          const val = text.slice(i, k);
          tokens.push(`${token} ${val}`);
          i = k;
        }
      } else {
        tokens.push(token);
      }
    }
  }

  return tokens;
}
