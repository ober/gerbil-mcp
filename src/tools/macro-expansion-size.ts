import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, buildLoadpathEnv, ERROR_MARKER, escapeSchemeString } from '../gxi.js';

interface ExpansionMetrics {
  sourceForms: number;
  sourceTokens: number;
  expandedForms: number;
  expandedTokens: number;
  expansionRatio: number;
  warningLevel: 'ok' | 'large' | 'explosive';
}

/**
 * Count top-level forms in a Scheme expression.
 */
function countForms(code: string): number {
  // Simple heuristic: count balanced top-level parentheses
  let depth = 0;
  let forms = 0;
  let inString = false;
  let escaped = false;

  for (let i = 0; i < code.length; i++) {
    const char = code[i];

    if (escaped) {
      escaped = false;
      continue;
    }

    if (char === '\\' && inString) {
      escaped = true;
      continue;
    }

    if (char === '"') {
      inString = !inString;
      continue;
    }

    if (inString) continue;

    if (char === '(') {
      depth++;
      if (depth === 1) forms++;
    } else if (char === ')') {
      depth--;
    }
  }

  return forms;
}

/**
 * Count tokens (words, symbols, numbers) in a Scheme expression.
 */
function countTokens(code: string): number {
  // Remove comments, strings, and delimiters, then count words
  const cleaned = code
    .replace(/;[^\n]*/g, '') // Remove line comments
    .replace(/"(?:\\.|[^"\\])*"/g, '') // Remove strings
    .replace(/[()[\]{}]/g, ' ') // Replace delimiters with spaces
    .trim();

  if (!cleaned) return 0;

  return cleaned.split(/\s+/).filter((token) => token.length > 0).length;
}

/**
 * Determine warning level based on expansion ratio.
 */
function getWarningLevel(ratio: number): 'ok' | 'large' | 'explosive' {
  if (ratio < 10) return 'ok';
  if (ratio < 50) return 'large';
  return 'explosive';
}

export function registerMacroExpansionSizeTool(server: McpServer): void {
  server.registerTool(
    'gerbil_macro_expansion_size',
    {
      title: 'Macro Expansion Size Checker',
      description:
        'Analyze the size of a macro expansion to detect accidentally explosive macros. ' +
        'Expands the macro invocation and compares the number of forms and tokens in the ' +
        'source vs the expanded output. Reports expansion ratio and warning level (ok/large/explosive). ' +
        'Helps catch: (1) macros that expand recursively, (2) macros generating more code than expected, ' +
        '(3) cases where a function would be more appropriate. Returns metrics and a warning if the ' +
        'expansion ratio exceeds normal thresholds (>10x = large, >50x = explosive).',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        expression: z
          .string()
          .describe('The macro invocation to analyze (e.g., "(my-macro arg1 arg2)")'),
        imports: z
          .array(z.string())
          .optional()
          .describe(
            'Optional list of modules to import (e.g., [":std/sugar", ":myproject/macros"])',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Optional list of directories to add to GERBIL_LOADPATH for resolving project-local dependencies',
          ),
      },
    },
    async ({ expression, imports, loadpath }) => {
      // Count source metrics
      const sourceForms = countForms(expression);
      const sourceTokens = countTokens(expression);

      // Expand the macro
      const escaped = escapeSchemeString(expression);

      const exprs: string[] = ['(import :gerbil/expander)'];

      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      const wrapper = [
        '(with-catch',
        '  (lambda (e)',
        `    (display "${ERROR_MARKER}\\n")`,
        '    (display-exception e (current-output-port)))',
        '  (lambda ()',
        `    (let ((expanded (call-with-output-string`,
        `            (lambda (port) (pretty-print`,
        `              (syntax->datum (core-expand (read (open-input-string "${escaped}"))))`,
        `              port)))))`,
        '      (write expanded)',
        '      (newline))))',
      ].join(' ');

      exprs.push(wrapper);

      try {
        const env = loadpath && loadpath.length > 0 ? buildLoadpathEnv(loadpath) : undefined;
        const result = await runGxi(exprs, { env });

        if (result.timedOut) {
          return {
            content: [
              {
                type: 'text' as const,
                text: 'Macro expansion timed out.',
              },
            ],
            isError: true,
          };
        }

        if (result.exitCode !== 0 && result.stderr) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Error:\n${result.stderr.trim()}`,
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
                text: `Expansion error:\n${errorMsg}`,
              },
            ],
            isError: true,
          };
        }

        // Parse the expanded string (it's a quoted string from the write call)
        let expanded = stdout.trim();
        if (expanded.startsWith('"') && expanded.endsWith('"')) {
          // Remove surrounding quotes and unescape
          expanded = expanded.slice(1, -1).replace(/\\n/g, '\n').replace(/\\\\/g, '\\');
        }

        // Count expanded metrics
        const expandedForms = countForms(expanded);
        const expandedTokens = countTokens(expanded);
        const expansionRatio =
          sourceTokens > 0 ? expandedTokens / sourceTokens : 0;
        const warningLevel = getWarningLevel(expansionRatio);

        const metrics: ExpansionMetrics = {
          sourceForms,
          sourceTokens,
          expandedForms,
          expandedTokens,
          expansionRatio: Math.round(expansionRatio * 10) / 10,
          warningLevel,
        };

        // Format output
        const sections: string[] = [
          `Macro Expansion Size Analysis`,
          ``,
          `Source:`,
          `  Forms: ${metrics.sourceForms}`,
          `  Tokens: ${metrics.sourceTokens}`,
          ``,
          `Expanded:`,
          `  Forms: ${metrics.expandedForms}`,
          `  Tokens: ${metrics.expandedTokens}`,
          ``,
          `Expansion Ratio: ${metrics.expansionRatio}x`,
          ``,
        ];

        if (warningLevel === 'explosive') {
          sections.push(
            `⚠️  WARNING: EXPLOSIVE EXPANSION (${metrics.expansionRatio}x)`,
          );
          sections.push(
            `   This macro expands to more than 50x the source size.`,
          );
          sections.push(
            `   Consider: (1) simplifying the macro, (2) using a function instead, (3) checking for accidental recursion.`,
          );
        } else if (warningLevel === 'large') {
          sections.push(
            `⚠️  WARNING: Large expansion (${metrics.expansionRatio}x)`,
          );
          sections.push(
            `   This macro expands to more than 10x the source size.`,
          );
          sections.push(`   Verify this is intentional.`);
        } else {
          sections.push(`✓ Expansion size is reasonable.`);
        }

        sections.push(``);
        sections.push(`Expanded Code:`);
        sections.push(``);
        sections.push(expanded);

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to analyze macro expansion: ${msg}`,
            },
          ],
          isError: true,
        };
      }
    },
  );
}
