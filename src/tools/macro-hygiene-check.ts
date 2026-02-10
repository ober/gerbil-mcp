import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';

interface HygieneIssue {
  line: number;
  severity: 'warning' | 'error';
  message: string;
  suggestion: string;
}

/**
 * Known free variables that are commonly captured in macros.
 */
const COMMON_CAPTURES = new Set([
  'result', 'tmp', 'temp', 'val', 'value', 'it', 'self', 'this',
  'loop', 'next', 'acc', 'i', 'j', 'k', 'x', 'y', 'n',
]);

/**
 * Analyze a macro definition for hygiene violations.
 */
function checkMacroHygiene(content: string): HygieneIssue[] {
  const issues: HygieneIssue[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();
    if (line.startsWith(';')) continue;

    // Check 1: defrules with free variables in templates
    if (line.match(/\(\s*defrules\s/)) {
      const macroBlock = extractMacroBlock(lines, i);
      checkDefrules(macroBlock, i, issues);
    }

    // Check 2: syntax-case with potential capture
    if (line.match(/\(\s*syntax-case\s/)) {
      const macroBlock = extractMacroBlock(lines, i);
      checkSyntaxCase(macroBlock, i, issues);
    }

    // Check 3: def inside macro expansion (should be let)
    if (line.match(/\(\s*(?:defrules|defsyntax|defmacro)\s/)) {
      const macroBlock = extractMacroBlock(lines, i);
      if (macroBlock.includes('(def ') && !macroBlock.includes('(begin ')) {
        issues.push({
          line: i + 1,
          severity: 'warning',
          message: 'def inside macro expansion may not work in expression context',
          suggestion: 'Use let/letrec instead of def inside macro templates',
        });
      }
    }

    // Check 4: Ellipsis outside template
    if (line.includes('...') && !line.startsWith(';')) {
      const context = lines.slice(Math.max(0, i - 5), i + 1).join('\n');
      if (!context.match(/\(\s*(?:defrules|syntax-rules|syntax-case|with-syntax)\s/) &&
          !context.includes('syntax') && !context.includes('#\'')) {
        // Only flag if we're inside a macro definition somewhere
        const macroContext = lines.slice(Math.max(0, i - 20), i + 1).join('\n');
        if (macroContext.match(/\(\s*(?:defrules|defsyntax|defmacro)\s/)) {
          issues.push({
            line: i + 1,
            severity: 'warning',
            message: 'Ellipsis (...) may be outside its pattern scope',
            suggestion: 'Ensure ... appears only in syntax templates matching a pattern variable with ...',
          });
        }
      }
    }
  }

  return issues;
}

function extractMacroBlock(lines: string[], startIdx: number): string {
  let depth = 0;
  let result = '';
  for (let i = startIdx; i < lines.length && i < startIdx + 50; i++) {
    result += lines[i] + '\n';
    for (const ch of lines[i]) {
      if (ch === '(' || ch === '[') depth++;
      else if (ch === ')' || ch === ']') depth--;
    }
    if (depth <= 0 && result.includes('(')) break;
  }
  return result;
}

function checkDefrules(block: string, startLine: number, issues: HygieneIssue[]): void {
  // Find template sections (after =>)
  const templateParts = block.split('=>');
  for (let t = 1; t < templateParts.length; t++) {
    const template = templateParts[t];

    // Extract identifiers used in the template
    const identPattern = /(?<![a-zA-Z0-9_!?*+/<>=.-])([a-zA-Z][a-zA-Z0-9_!?*-]*)(?![a-zA-Z0-9_!?*+/<>=.-])/g;
    let match;
    while ((match = identPattern.exec(template)) !== null) {
      const ident = match[1];
      if (COMMON_CAPTURES.has(ident)) {
        // Check if it's a pattern variable (defined in the pattern before =>)
        const pattern = templateParts[t - 1];
        if (!pattern.includes(ident)) {
          issues.push({
            line: startLine + 1,
            severity: 'warning',
            message: `Free variable '${ident}' in macro template could capture user bindings`,
            suggestion: `Use (with-syntax ((${ident} (generate-temporary))) ...) for a fresh binding`,
          });
        }
      }
    }
  }
}

function checkSyntaxCase(block: string, startLine: number, issues: HygieneIssue[]): void {
  // Check for datum->syntax usage
  if (block.includes('datum->syntax') || block.includes('#\'')) {
    // Good — they're using proper syntax construction
    return;
  }

  // Check for identifier construction without datum->syntax
  const constructPattern = /string->symbol|symbol-append|string-append.*->.*symbol/;
  if (constructPattern.test(block)) {
    issues.push({
      line: startLine + 1,
      severity: 'warning',
      message: 'Constructing identifiers without datum->syntax may break hygiene',
      suggestion: 'Use (datum->syntax stx (string->symbol ...)) to create hygienic identifiers',
    });
  }
}

export function registerMacroHygieneCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_macro_hygiene_check',
    {
      title: 'Macro Hygiene Check',
      description:
        'Detect potential variable capture and hygiene violations in macro definitions. ' +
        'Checks for: unintended free variable capture in defrules templates, missing ' +
        'datum->syntax conversions in syntax-case, def inside macro expansion (should be let), ' +
        'and ellipsis usage outside pattern scope.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .describe('Gerbil source file to check'),
      },
    },
    async ({ file_path }) => {
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

      const issues = checkMacroHygiene(content);

      if (issues.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: `No macro hygiene issues detected in ${file_path}.`,
          }],
        };
      }

      const sections: string[] = [
        `## Macro Hygiene Check: ${file_path}`,
        '',
        `Issues found: ${issues.length}`,
        '',
      ];

      for (const issue of issues) {
        sections.push(`### Line ${issue.line} [${issue.severity.toUpperCase()}]`);
        sections.push(`  ${issue.message}`);
        sections.push(`  **Fix**: ${issue.suggestion}`);
        sections.push('');
      }

      sections.push('**Note**: Use `gerbil_expand_macro` or `gerbil_trace_macro` to verify');
      sections.push('macro expansions and test hygiene in different binding contexts.');

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
