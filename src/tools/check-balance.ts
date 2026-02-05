import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';

interface StackEntry {
  char: string;
  line: number;
  col: number;
  context: string;
}

export interface BalanceResult {
  ok: boolean;
  topLevelForms: number;
  errors: BalanceError[];
}

export interface BalanceError {
  kind: 'unclosed' | 'unexpected' | 'mismatch';
  line: number;
  col: number;
  char: string;
  expected?: string;
  openerLine?: number;
  openerCol?: number;
  openerChar?: string;
  context?: string;
}

const OPENERS: Record<string, string> = { '(': ')', '[': ']', '{': '}' };
const CLOSERS: Record<string, string> = { ')': '(', ']': '[', '}': '{' };

export function checkBalance(source: string): BalanceResult {
  const stack: StackEntry[] = [];
  const errors: BalanceError[] = [];
  let topLevelForms = 0;
  let line = 1;
  let col = 1;
  let i = 0;
  const len = source.length;

  while (i < len) {
    const ch = source[i];

    // --- String literal ---
    if (ch === '"') {
      i++;
      col++;
      while (i < len && source[i] !== '"') {
        if (source[i] === '\\') {
          i++;
          col++;
        }
        if (i < len && source[i] === '\n') {
          line++;
          col = 1;
        } else {
          col++;
        }
        i++;
      }
      // skip closing quote
      if (i < len) {
        i++;
        col++;
      }
      continue;
    }

    // --- Line comment ---
    if (ch === ';') {
      while (i < len && source[i] !== '\n') {
        i++;
      }
      // newline will be handled next iteration
      continue;
    }

    // --- Block comment #| ... |# (nestable) ---
    if (ch === '#' && i + 1 < len && source[i + 1] === '|') {
      let depth = 1;
      i += 2;
      col += 2;
      while (i < len && depth > 0) {
        if (source[i] === '#' && i + 1 < len && source[i + 1] === '|') {
          depth++;
          i += 2;
          col += 2;
        } else if (source[i] === '|' && i + 1 < len && source[i + 1] === '#') {
          depth--;
          i += 2;
          col += 2;
        } else if (source[i] === '\n') {
          line++;
          col = 1;
          i++;
        } else {
          col++;
          i++;
        }
      }
      continue;
    }

    // --- Datum comment #; ---
    if (ch === '#' && i + 1 < len && source[i + 1] === ';') {
      // Skip #; — the next datum is discarded by the reader, but
      // we still need to track its delimiters for balance purposes.
      // Just skip the two-char prefix and let normal scanning continue.
      i += 2;
      col += 2;
      continue;
    }

    // --- #! reader directives (#!void, #!eof, #!optional, etc.) ---
    if (ch === '#' && i + 1 < len && source[i + 1] === '!') {
      i += 2;
      col += 2;
      // Consume the rest of the directive name
      while (i < len && /[a-zA-Z0-9_-]/.test(source[i])) {
        i++;
        col++;
      }
      continue;
    }

    // --- Character literal #\x ---
    if (ch === '#' && i + 1 < len && source[i + 1] === '\\') {
      i += 2;
      col += 2;
      // Named characters: #\space, #\newline, etc.
      // Or single char: #\(, #\a, etc.
      // Consume alphanumeric chars (for named characters like #\newline)
      if (i < len && /[a-zA-Z]/.test(source[i])) {
        while (i < len && /[a-zA-Z0-9]/.test(source[i])) {
          i++;
          col++;
        }
      } else if (i < len) {
        // Single char like #\( or #\)
        i++;
        col++;
      }
      continue;
    }

    // --- Pipe symbol |...| ---
    if (ch === '|') {
      i++;
      col++;
      while (i < len && source[i] !== '|') {
        if (source[i] === '\\') {
          i++;
          col++;
        }
        if (i < len && source[i] === '\n') {
          line++;
          col = 1;
        } else {
          col++;
        }
        i++;
      }
      // skip closing pipe
      if (i < len) {
        i++;
        col++;
      }
      continue;
    }

    // --- Openers ---
    if (OPENERS[ch]) {
      // Determine context from characters following the opener
      let context = '';
      let j = i + 1;
      while (j < len && /\s/.test(source[j])) j++;
      const rest = source.slice(j, j + 30);
      const ctxMatch = rest.match(/^([^\s()[\]{}"]+)/);
      if (ctxMatch) {
        context = ctxMatch[1];
      }

      stack.push({ char: ch, line, col, context });
      i++;
      col++;
      continue;
    }

    // --- Closers ---
    if (CLOSERS[ch]) {
      if (stack.length === 0) {
        errors.push({
          kind: 'unexpected',
          line,
          col,
          char: ch,
        });
      } else {
        const top = stack[stack.length - 1];
        const expectedCloser = OPENERS[top.char];
        if (ch !== expectedCloser) {
          errors.push({
            kind: 'mismatch',
            line,
            col,
            char: ch,
            expected: expectedCloser,
            openerLine: top.line,
            openerCol: top.col,
            openerChar: top.char,
          });
        }
        stack.pop();
      }
      // Count top-level forms when stack returns to depth 0
      if (stack.length === 0 && errors.length === 0) {
        topLevelForms++;
      }
      i++;
      col++;
      continue;
    }

    // --- Newline ---
    if (ch === '\n') {
      line++;
      col = 1;
      i++;
      continue;
    }

    // --- Whitespace and other characters ---
    // Count bare atoms at top level as forms
    if (stack.length === 0 && !/\s/.test(ch) && ch !== "'" && ch !== '`' && ch !== ',') {
      // Consume the atom
      while (i < len && !/[\s()[\]{}"`;,]/.test(source[i])) {
        if (source[i] === '\n') {
          line++;
          col = 1;
        } else {
          col++;
        }
        i++;
      }
      topLevelForms++;
      continue;
    }

    col++;
    i++;
  }

  // Anything left on the stack is unclosed
  for (const entry of stack) {
    errors.push({
      kind: 'unclosed',
      line: entry.line,
      col: entry.col,
      char: entry.char,
      context: entry.context,
    });
  }

  return {
    ok: errors.length === 0,
    topLevelForms,
    errors,
  };
}

function formatBalanceResult(result: BalanceResult): string {
  if (result.ok) {
    return `Balance OK: ${result.topLevelForms} top-level form(s)`;
  }

  const lines: string[] = [];

  for (const err of result.errors) {
    switch (err.kind) {
      case 'unclosed':
        lines.push(
          `Unclosed '${err.char}' at line ${err.line}, col ${err.col}` +
            (err.context ? ` (near '${err.context}')` : ''),
        );
        break;
      case 'unexpected':
        lines.push(
          `Unexpected closer '${err.char}' at line ${err.line}, col ${err.col}` +
            ' — no matching opener',
        );
        break;
      case 'mismatch':
        lines.push(
          `Mismatched '${err.char}' at line ${err.line}, col ${err.col}` +
            ` — expected '${err.expected}' to close '${err.openerChar}'` +
            ` opened at line ${err.openerLine}, col ${err.openerCol}`,
        );
        break;
    }
  }

  lines.push('');
  lines.push(
    'Note: This is a heuristic check. Use gerbil_read_forms or gerbil_check_syntax for definitive validation.',
  );

  return lines.join('\n');
}

export function registerCheckBalanceTool(server: McpServer): void {
  server.registerTool(
    'gerbil_check_balance',
    {
      title: 'Check Delimiter Balance',
      description:
        'Check parenthesis/bracket/brace balance in Gerbil Scheme code. ' +
        'Pure delimiter scanner — no subprocess, runs in milliseconds. ' +
        'Reports unclosed delimiters, unexpected closers, and mismatches with positions.',
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Path to a Gerbil source file to check'),
        code: z
          .string()
          .optional()
          .describe('Inline Gerbil code to check (alternative to file_path)'),
      },
    },
    async ({ file_path, code }) => {
      let source: string;

      if (file_path) {
        try {
          source = await readFile(file_path, 'utf-8');
        } catch (err) {
          const msg = err instanceof Error ? err.message : 'Unknown error';
          return {
            content: [{ type: 'text' as const, text: `Failed to read file: ${msg}` }],
            isError: true,
          };
        }
      } else if (code) {
        source = code;
      } else {
        return {
          content: [
            { type: 'text' as const, text: 'Either file_path or code must be provided.' },
          ],
          isError: true,
        };
      }

      const result = checkBalance(source);
      const text = formatBalanceResult(result);

      return {
        content: [{ type: 'text' as const, text }],
        isError: !result.ok,
      };
    },
  );
}
