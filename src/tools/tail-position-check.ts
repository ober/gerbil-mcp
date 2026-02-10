import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';

interface TailCallSite {
  line: number;
  isTail: boolean;
  reason: string;
  callExpr: string;
}

/**
 * Analyze whether recursive calls in a function definition are in tail position.
 * Uses the Gerbil source text to detect common patterns.
 */
function analyzeTailCalls(
  content: string,
  funcName: string,
): TailCallSite[] {
  const sites: TailCallSite[] = [];
  const lines = content.split('\n');
  const escaped = funcName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  const callPattern = new RegExp(
    `(?<![a-zA-Z0-9_!?*+/<>=.-])\\(${escaped}\\s`,
    'g',
  );

  // Find the function definition
  let defLine = -1;
  let defDepth = 0;
  let defEndLine = -1;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();
    if (
      line.match(
        new RegExp(
          `^\\(\\s*(?:def|define)\\s+\\(?${escaped}[\\s)]`,
        ),
      )
    ) {
      defLine = i;
      break;
    }
    // Also match named let: (let funcName ((params...)) body...)
    if (
      line.match(
        new RegExp(`^\\(\\s*let\\s+${escaped}\\s`),
      )
    ) {
      defLine = i;
      break;
    }
  }

  if (defLine === -1) return sites;

  // Find extent of the definition
  let depth = 0;
  for (let i = defLine; i < lines.length; i++) {
    for (const ch of lines[i]) {
      if (ch === '(' || ch === '[') depth++;
      else if (ch === ')' || ch === ']') depth--;
    }
    if (depth <= 0) {
      defEndLine = i;
      break;
    }
  }
  if (defEndLine === -1) defEndLine = lines.length - 1;

  // Scan for recursive calls within the definition
  for (let i = defLine + 1; i <= defEndLine; i++) {
    const line = lines[i];
    let match;
    callPattern.lastIndex = 0;

    while ((match = callPattern.exec(line)) !== null) {
      const beforeMatch = line.slice(0, match.index);
      if (beforeMatch.includes(';')) continue; // skip comments

      const analysis = analyzeTailPosition(lines, i, match.index, funcName, defLine, defEndLine);
      sites.push({
        line: i + 1,
        isTail: analysis.isTail,
        reason: analysis.reason,
        callExpr: line.trim().slice(0, 60),
      });
    }
  }

  return sites;
}

function analyzeTailPosition(
  lines: string[],
  callLine: number,
  callCol: number,
  funcName: string,
  defStart: number,
  defEnd: number,
): { isTail: boolean; reason: string } {
  // Get context around the call
  const line = lines[callLine];

  // Check what's after the closing paren of this call
  // If nothing follows (or just closing parens), it's likely in tail position

  // Check for wrapping forms that make it non-tail
  const nonTailWrappers = [
    { pattern: /\(\s*(?:cons|list|vector|append|string-append|values)\s+/, reason: 'wrapped in data constructor' },
    { pattern: /\(\s*(?:\+|-|\*|\/|quotient|remainder|modulo)\s+/, reason: 'wrapped in arithmetic' },
    { pattern: /\(\s*(?:not|and|or)\s+/, reason: 'wrapped in boolean combinator' },
    { pattern: /\(\s*(?:car|cdr|caar|cadr|cddr|length)\s+/, reason: 'wrapped in list accessor' },
    { pattern: /\(\s*(?:map|filter|for-each|fold|foldl|foldr)\s+/, reason: 'inside higher-order function callback' },
    { pattern: /\(\s*(?:string->number|number->string|symbol->string)\s+/, reason: 'wrapped in conversion' },
  ];

  // Check preceding context on the same line
  const before = line.slice(0, callCol).trim();

  for (const { pattern, reason } of nonTailWrappers) {
    if (pattern.test(before)) {
      return { isTail: false, reason };
    }
  }

  // Check if the call result is being consumed by something
  // Simple heuristic: count parens. If the call is not at the deepest nesting
  // level within its body form, it's likely not in tail position.

  // Check for let/let* where the call is in binding position (not tail)
  const context = lines.slice(Math.max(defStart, callLine - 3), callLine + 1).join('\n');
  if (context.match(/\(\s*(?:let\*?|letrec\*?)\s+\(/)) {
    // Check if the call is in a binding (before the body)
    const bindingSection = context.match(/\(\s*(?:let\*?|letrec\*?)\s+\(([^]*)\)\s/);
    if (bindingSection) {
      const escaped = funcName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
      if (new RegExp(`\\(${escaped}\\s`).test(bindingSection[1])) {
        return { isTail: false, reason: 'in let binding position (not body)' };
      }
    }
  }

  // If the call appears right before closing parens, it's likely tail
  const afterCall = line.slice(callCol);
  const closeMatch = afterCall.match(/\([^)]*\)(\s*\)*\s*)$/);
  if (closeMatch) {
    return { isTail: true, reason: 'in tail position' };
  }

  // Default: assume tail position if not otherwise flagged
  // (conservative — better to say "likely tail" than to miss non-tail)
  return { isTail: true, reason: 'likely in tail position (no wrapping form detected)' };
}

export function registerTailPositionCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_tail_position_check',
    {
      title: 'Tail Position Check',
      description:
        'Verify that recursive calls in a function are in tail position. ' +
        'Reports each call site as tail or non-tail with the reason ' +
        '(e.g. "wrapped in cons", "inside map callback", "in let binding position"). ' +
        'Catches the common Scheme bug of non-tail recursion that blows the stack.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .describe('Gerbil source file containing the function'),
        function_name: z
          .string()
          .describe('Name of the function to check for tail calls'),
      },
    },
    async ({ file_path, function_name }) => {
      let content: string;
      try {
        content = await readFile(file_path, 'utf-8');
      } catch {
        return {
          content: [
            {
              type: 'text' as const,
              text: `File not found: ${file_path}`,
            },
          ],
          isError: true,
        };
      }

      const sites = analyzeTailCalls(content, function_name);

      if (sites.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No recursive calls to '${function_name}' found in ${file_path}.\n` +
                'Note: The function must be defined with def/define or named let.',
            },
          ],
        };
      }

      const tailCount = sites.filter((s) => s.isTail).length;
      const nonTailCount = sites.filter((s) => !s.isTail).length;

      const sections: string[] = [
        `## Tail Position Analysis: ${function_name}`,
        '',
        `Recursive calls found: ${sites.length}`,
        `Tail calls: ${tailCount}`,
        `Non-tail calls: ${nonTailCount}`,
        '',
      ];

      for (const site of sites) {
        const tag = site.isTail ? 'TAIL' : '**NON-TAIL**';
        sections.push(`Line ${site.line}: [${tag}] ${site.reason}`);
        sections.push(`  ${site.callExpr}`);
        sections.push('');
      }

      if (nonTailCount > 0) {
        sections.push('### Suggestion');
        sections.push(
          'Convert non-tail recursive calls to use an accumulator pattern:',
        );
        sections.push(
          '```scheme',
        );
        sections.push(
          `(def (${function_name} input (acc []))`,
        );
        sections.push(
          `  ;; ... accumulate result instead of wrapping recursive call`,
        );
        sections.push(
          `  (${function_name} (rest input) (cons (process (first input)) acc)))`,
        );
        sections.push('```');
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
