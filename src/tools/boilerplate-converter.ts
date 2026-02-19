import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';

interface MacroGeneration {
  macroDefinition: string;
  invocations: string[];
  explanation: string;
}

/**
 * Tokenize a Scheme expression into parts.
 */
function tokenize(expr: string): string[] {
  const tokens: string[] = [];
  let current = '';
  let inString = false;
  let depth = 0;

  for (let i = 0; i < expr.length; i++) {
    const char = expr[i];

    if (char === '"' && (i === 0 || expr[i - 1] !== '\\')) {
      inString = !inString;
      current += char;
    } else if (!inString) {
      if (char === '(' || char === ')') {
        if (current.trim()) {
          tokens.push(current.trim());
          current = '';
        }
        tokens.push(char);
        if (char === '(') depth++;
        else depth--;
      } else if (/\s/.test(char)) {
        if (current.trim()) {
          tokens.push(current.trim());
          current = '';
        }
      } else {
        current += char;
      }
    } else {
      current += char;
    }
  }

  if (current.trim()) {
    tokens.push(current.trim());
  }

  return tokens;
}

/**
 * Align expressions and find varying parts.
 */
function alignExpressions(exprs: string[]): {
  template: string[];
  varying: Map<number, Set<string>>;
} {
  if (exprs.length < 2) {
    throw new Error('Need at least 2 expressions to align');
  }

  const tokenized = exprs.map((e) => tokenize(e));
  const minLength = Math.min(...tokenized.map((t) => t.length));

  const template: string[] = [];
  const varying = new Map<number, Set<string>>();

  for (let i = 0; i < minLength; i++) {
    const values = new Set(tokenized.map((t) => t[i]));

    if (values.size === 1) {
      // Constant part
      template.push([...values][0]);
    } else {
      // Varying part
      template.push(`VAR${i}`);
      varying.set(i, values);
    }
  }

  return { template, varying };
}

/**
 * Generate a macro definition from aligned expressions.
 */
function generateMacro(
  examples: string[],
): MacroGeneration {
  try {
    const { template, varying } = alignExpressions(examples);

    // Extract parameter names from varying parts
    const paramNames: string[] = [];
    const varIndexes: number[] = [];

    for (const [idx, values] of varying) {
      paramNames.push(`param${paramNames.length + 1}`);
      varIndexes.push(idx);
    }

    // Build macro template
    const macroTemplate = template
      .map((part, idx) => {
        const varIdx = varIndexes.indexOf(idx);
        if (varIdx >= 0) {
          return paramNames[varIdx];
        }
        return part;
      })
      .join(' ')
      .replace(/\s+/g, ' ');

    // Build macro definition
    const macroName = 'generated-macro';
    const macroDefinition = `(defrule (${macroName} ${paramNames.join(' ')})\n  ${macroTemplate})`;

    // Build invocations
    const tokenized = examples.map((e) => tokenize(e));
    const invocations = tokenized.map((tokens) => {
      const args = varIndexes.map((idx) => tokens[idx]);
      return `(${macroName} ${args.join(' ')})`;
    });

    const explanation = `Generated macro with ${paramNames.length} parameter(s): ${paramNames.join(', ')}.\n` +
      `Detected ${varying.size} varying position(s) across ${examples.length} examples.`;

    return {
      macroDefinition,
      invocations,
      explanation,
    };
  } catch (err) {
    const msg = err instanceof Error ? err.message : 'Unknown error';
    throw new Error(`Failed to generate macro: ${msg}`);
  }
}

export function registerBoilerplateConverterTool(server: McpServer): void {
  server.registerTool(
    'gerbil_boilerplate_converter',
    {
      title: 'Boilerplate to Macro Converter',
      description:
        'Convert repetitive code blocks into a macro definition automatically. ' +
        'Given 2-3 similar code expressions, extracts the common pattern and generates ' +
        'a defrule macro that captures it. Identifies varying vs constant parts and ' +
        'creates appropriate pattern variables. Returns both the macro definition and ' +
        'the replacement invocations.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        examples: z
          .array(z.string())
          .min(2)
          .describe(
            'Array of 2+ similar Scheme expressions to convert into a macro (e.g., repeated function definitions)',
          ),
      },
    },
    async ({ examples }) => {
      if (examples.length < 2) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Error: Need at least 2 examples to generate a macro.',
            },
          ],
          isError: true,
        };
      }

      try {
        const result = generateMacro(examples);

        const sections: string[] = [
          `Boilerplate to Macro Conversion`,
          ``,
          `Input: ${examples.length} similar expressions`,
          ``,
          result.explanation,
          ``,
          `Generated Macro Definition:`,
          ``,
          result.macroDefinition,
          ``,
          `Replacement Invocations:`,
          ``,
        ];

        result.invocations.forEach((inv, idx) => {
          sections.push(`  ${idx + 1}. ${inv}`);
        });

        sections.push(``);
        sections.push(
          `Code reduction: ${examples.join('\n').split('\n').length} lines → ${result.macroDefinition.split('\n').length + result.invocations.length} lines`,
        );

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to generate macro: ${msg}`,
            },
          ],
          isError: true,
        };
      }
    },
  );
}
