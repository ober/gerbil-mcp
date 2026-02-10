import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';

interface ReturnPath {
  line: number;
  returnType: string;
  context: string;
  isGotcha: boolean;
  gotchaNote?: string;
}

/**
 * Known return type gotchas in Gerbil/Gambit.
 */
const GOTCHAS: Array<{
  pattern: RegExp;
  returnType: string;
  note: string;
}> = [
  {
    pattern: /\(hash-ref\s/,
    returnType: '#!void (truthy!)',
    note: 'hash-ref returns #!void for missing keys (truthy!) — use hash-get for #f',
  },
  {
    pattern: /\(when\s/,
    returnType: '#<void> or value',
    note: 'when returns #<void> when condition is false, not #f',
  },
  {
    pattern: /\(unless\s/,
    returnType: '#<void> or value',
    note: 'unless returns #<void> when condition is true, not #f',
  },
  {
    pattern: /\(string-contains\s/,
    returnType: 'index (integer) or #f',
    note: 'string-contains returns an index (truthy integer), not #t',
  },
  {
    pattern: /\(assoc\s/,
    returnType: 'pair or #f',
    note: 'assoc returns the pair, not the value — use cdr to get the value',
  },
  {
    pattern: /\(member\s/,
    returnType: 'sublist or #f',
    note: 'member returns the tail of the list starting at the match, not #t',
  },
  {
    pattern: /\(set!\s/,
    returnType: '#<void>',
    note: 'set! returns void — do not use as an expression value',
  },
  {
    pattern: /\(for-each\s/,
    returnType: '#<void>',
    note: 'for-each returns void — use map or for/collect for results',
  },
  {
    pattern: /\(display\s/,
    returnType: '#<void>',
    note: 'display returns void — it writes to a port as a side effect',
  },
  {
    pattern: /\(vector-set!\s/,
    returnType: '#<void>',
    note: 'vector-set! returns void',
  },
  {
    pattern: /\(hash-put!\s/,
    returnType: '#<void>',
    note: 'hash-put! returns void',
  },
  {
    pattern: /\(table-set!\s/,
    returnType: '#<void>',
    note: 'table-set! returns void',
  },
];

/**
 * Analyze return paths of a function.
 */
function analyzeReturnPaths(
  content: string,
  funcName: string,
): ReturnPath[] {
  const paths: ReturnPath[] = [];
  const lines = content.split('\n');
  const escaped = funcName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

  // Find function definition
  let defLine = -1;
  for (let i = 0; i < lines.length; i++) {
    if (lines[i].match(new RegExp(`^\\(\\s*(?:def|define)\\s+\\(?${escaped}[\\s)]`))) {
      defLine = i;
      break;
    }
  }

  if (defLine === -1) return paths;

  // Find end of definition
  let depth = 0;
  let defEnd = lines.length - 1;
  for (let i = defLine; i < lines.length; i++) {
    for (const ch of lines[i]) {
      if (ch === '(' || ch === '[') depth++;
      else if (ch === ')' || ch === ']') depth--;
    }
    if (depth <= 0) {
      defEnd = i;
      break;
    }
  }

  // Scan body for return-type-affecting forms
  for (let i = defLine + 1; i <= defEnd; i++) {
    const line = lines[i].trim();
    if (!line || line.startsWith(';')) continue;

    // Check for gotcha patterns
    for (const gotcha of GOTCHAS) {
      if (gotcha.pattern.test(line)) {
        paths.push({
          line: i + 1,
          returnType: gotcha.returnType,
          context: line.slice(0, 80),
          isGotcha: true,
          gotchaNote: gotcha.note,
        });
      }
    }

    // Check for when/unless in tail position
    if (line.match(/^\(when\s/) || line.match(/^\(unless\s/)) {
      paths.push({
        line: i + 1,
        returnType: '#<void> when condition fails',
        context: line.slice(0, 80),
        isGotcha: true,
        gotchaNote: 'when/unless in tail position returns #<void> when condition fails — use if instead for boolean returns',
      });
    }

    // Check for match without else clause
    if (line.match(/^\(match\s/)) {
      // Look for else clause
      let hasElse = false;
      for (let j = i; j <= Math.min(defEnd, i + 20); j++) {
        if (lines[j].includes('(else') || lines[j].includes('(_')) {
          hasElse = true;
          break;
        }
      }
      if (!hasElse) {
        paths.push({
          line: i + 1,
          returnType: 'exception if no pattern matches',
          context: line.slice(0, 80),
          isGotcha: true,
          gotchaNote: 'match without else/_ clause throws an exception if no pattern matches',
        });
      }
    }
  }

  return paths;
}

export function registerReturnTypeAnalysisTool(server: McpServer): void {
  server.registerTool(
    'gerbil_return_type_analysis',
    {
      title: 'Return Type Analysis',
      description:
        'Analyze what types/values a function can return across all code paths. ' +
        'Catches common Gerbil gotchas: hash-ref returning #!void (truthy!), ' +
        'when/unless returning #<void> instead of #f, match with incomplete patterns, ' +
        'string-contains returning index not #t, void-returning functions used as expressions.',
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
          .describe('Name of the function to analyze'),
      },
    },
    async ({ file_path, function_name }) => {
      let content: string;
      try {
        content = await readFile(file_path, 'utf-8');
      } catch {
        return {
          content: [{
            type: 'text' as const,
            text: `File not found: ${file_path}`,
          }],
          isError: true,
        };
      }

      const paths = analyzeReturnPaths(content, function_name);

      if (paths.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: `No return type issues detected for '${function_name}' in ${file_path}.\n` +
              'Either the function was not found or no gotcha patterns were detected.',
          }],
        };
      }

      const gotchas = paths.filter((p) => p.isGotcha);

      const sections: string[] = [
        `## Return Type Analysis: ${function_name}`,
        '',
        `Potential return type issues: ${paths.length}`,
        `Gotcha patterns: ${gotchas.length}`,
        '',
      ];

      for (const path of paths) {
        sections.push(`### Line ${path.line}: ${path.returnType}`);
        sections.push(`  ${path.context}`);
        if (path.gotchaNote) {
          sections.push(`  **Gotcha**: ${path.gotchaNote}`);
        }
        sections.push('');
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
