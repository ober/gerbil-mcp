import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';

interface Pattern {
  type: string;
  instances: string[];
  lines: number[];
  template: string;
  suggestion: string;
  savings: number; // Lines saved
}

/**
 * Detect repeated function definitions with similar structure.
 * Pattern: (def (name-VARIANT args...) body)
 */
function detectRepeatedFunctions(content: string): Pattern[] {
  const patterns: Pattern[] = [];
  const lines = content.split('\n');

  // Group functions by structure similarity
  const functionGroups = new Map<string, Array<{ name: string; line: number; full: string }>>();

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Match (def (name args...) body)
    const match = line.match(/^\s*\(def\s+\(([a-zA-Z0-9_!?<>=+\-*/:~#]+)\s+([^)]*)\)\s*(.+)$/);
    if (match) {
      const funcName = match[1];
      const args = match[2];
      const body = match[3];

      // Create a normalized signature (body structure without specific names)
      const signature = `(def (NAME ${args}) ${normalizeExpression(body)})`;

      if (!functionGroups.has(signature)) {
        functionGroups.set(signature, []);
      }
      functionGroups.get(signature)!.push({
        name: funcName,
        line: i + 1,
        full: line.trim(),
      });
    }
  }

  // Find groups with 3+ similar functions
  for (const [signature, funcs] of functionGroups) {
    if (funcs.length >= 3) {
      const instances = funcs.map(f => f.full);
      const linesNums = funcs.map(f => f.line);
      const totalLines = funcs.length;
      const macroLines = 2 + funcs.length; // defrule definition + invocations
      const savings = totalLines - macroLines;

      patterns.push({
        type: 'repeated-function-definitions',
        instances: instances.slice(0, 3), // Show first 3 as examples
        lines: linesNums,
        template: signature,
        suggestion: `Define a macro to generate these ${funcs.length} similar functions`,
        savings,
      });
    }
  }

  return patterns;
}

/**
 * Detect repeated hash-ref accessor patterns.
 * Pattern: (def (get-X obj) (hash-ref obj "X"))
 */
function detectHashAccessors(content: string): Pattern[] {
  const patterns: Pattern[] = [];
  const lines = content.split('\n');
  const accessors: Array<{ name: string; key: string; line: number; full: string }> = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Match (def (get-X obj) (hash-ref obj "X"))
    const match = line.match(/^\s*\(def\s+\(([a-zA-Z0-9_!?<>=+\-*/:~#]+)\s+(\w+)\)\s+\(hash-ref\s+\2\s+["']([^"']+)["']\)\s*\)?\s*$/);
    if (match) {
      accessors.push({
        name: match[1],
        key: match[3],
        line: i + 1,
        full: line.trim(),
      });
    }
  }

  if (accessors.length >= 3) {
    const totalLines = accessors.length;
    const macroLines = 2 + accessors.length; // defrule + invocations
    const savings = totalLines - macroLines;

    patterns.push({
      type: 'hash-ref-accessors',
      instances: accessors.slice(0, 3).map(a => a.full),
      lines: accessors.map(a => a.line),
      template: '(def (NAME obj) (hash-ref obj "KEY"))',
      suggestion: `Use a macro like: (defrule (def-getter name key) (def (name obj) (hash-ref obj key)))\nThen: ${accessors.map(a => `(def-getter ${a.name} "${a.key}")`).slice(0, 3).join(' ')}`,
      savings,
    });
  }

  return patterns;
}

/**
 * Detect repeated method wrappers.
 * Pattern: (def (method-name obj args...) ({method-name obj} args...))
 */
function detectMethodWrappers(content: string): Pattern[] {
  const patterns: Pattern[] = [];
  const lines = content.split('\n');
  const wrappers: Array<{ name: string; line: number; full: string }> = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Match (def (name obj args...) ({method obj} args...))
    const match = line.match(/^\s*\(def\s+\(([a-zA-Z0-9_!?<>=+\-*/:~#]+)\s+(\w+)(\s+[^)]+)?\)\s+\(\{[^}]+\s+\2\}[^)]*\)\s*\)?\s*$/);
    if (match) {
      wrappers.push({
        name: match[1],
        line: i + 1,
        full: line.trim(),
      });
    }
  }

  if (wrappers.length >= 3) {
    const totalLines = wrappers.length;
    const macroLines = 2 + wrappers.length;
    const savings = totalLines - macroLines;

    patterns.push({
      type: 'method-wrappers',
      instances: wrappers.slice(0, 3).map(w => w.full),
      lines: wrappers.map(w => w.line),
      template: '(def (method-name obj args...) ({method obj} args...))',
      suggestion: `Consider using a macro to generate these ${wrappers.length} method wrapper functions`,
      savings,
    });
  }

  return patterns;
}

/**
 * Normalize an expression for pattern matching (replace specific names with placeholders).
 */
function normalizeExpression(expr: string): string {
  // Replace quoted strings with STRING
  let normalized = expr.replace(/"[^"]*"/g, 'STRING');
  normalized = normalized.replace(/'[^']+/g, 'SYMBOL');
  // Replace numbers with NUM
  normalized = normalized.replace(/\b\d+\b/g, 'NUM');
  return normalized;
}

export function registerMacroPatternDetectorTool(server: McpServer): void {
  server.registerTool(
    'gerbil_macro_pattern_detector',
    {
      title: 'Macro Pattern Detector',
      description:
        'Analyze Gerbil source files for repetitive code patterns that could be replaced with macros. ' +
        'Detects: (1) repeated function definitions with similar structure, (2) repeated hash-ref accessors, ' +
        '(3) repeated method wrappers. For each pattern, suggests a macro definition and shows potential ' +
        'code size reduction. Helps identify opportunities to reduce boilerplate and improve maintainability.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z.string().describe('Path to Gerbil source file (.ss) to analyze'),
        min_occurrences: z
          .number()
          .optional()
          .describe('Minimum number of repetitions to report as a pattern (default: 3)'),
      },
    },
    async ({ file_path, min_occurrences }) => {
      const minOccur = min_occurrences ?? 3;

      try {
        const content = await readFile(file_path, 'utf-8');

        // Run all pattern detectors
        const allPatterns: Pattern[] = [
          ...detectRepeatedFunctions(content),
          ...detectHashAccessors(content),
          ...detectMethodWrappers(content),
        ];

        // Filter by minimum occurrences
        const filteredPatterns = allPatterns.filter(
          (p) => p.lines.length >= minOccur,
        );

        if (filteredPatterns.length === 0) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `No repetitive patterns detected (minimum ${minOccur} occurrences).`,
              },
            ],
          };
        }

        // Format output
        const sections: string[] = [
          `Macro Pattern Detection: ${file_path}`,
          ``,
          `Found ${filteredPatterns.length} repetitive pattern(s):`,
          ``,
        ];

        for (const pattern of filteredPatterns) {
          sections.push(`Pattern: ${pattern.type}`);
          sections.push(`  Occurrences: ${pattern.lines.length}`);
          sections.push(`  Lines: ${pattern.lines.slice(0, 10).join(', ')}${pattern.lines.length > 10 ? '...' : ''}`);
          sections.push(`  Template: ${pattern.template}`);
          sections.push(`  Example instances:`);
          pattern.instances.forEach((inst) => {
            sections.push(`    ${inst}`);
          });
          sections.push(`  Suggestion: ${pattern.suggestion}`);
          sections.push(`  Code reduction: ${pattern.savings} lines saved (${pattern.lines.length} → ${pattern.lines.length - pattern.savings})`);
          sections.push(``);
        }

        const totalSavings = filteredPatterns.reduce((sum, p) => sum + p.savings, 0);
        sections.push(`Total potential savings: ${totalSavings} lines`);

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to read file: ${msg}`,
            },
          ],
          isError: true,
        };
      }
    },
  );
}
