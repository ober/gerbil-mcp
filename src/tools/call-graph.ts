import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { parseDefinitions, isSchemeDelimiter } from './parse-utils.js';

export function registerCallGraphTool(server: McpServer): void {
  server.registerTool(
    'gerbil_call_graph',
    {
      title: 'Static Call Graph',
      description:
        'Analyze which functions call which other functions in a Gerbil source file. ' +
        'Uses static analysis (no execution). Shows a tree visualization and flat listing ' +
        'of call relationships between locally defined functions. ' +
        'Optionally filter to show only the tree rooted at a specific function.',
      inputSchema: {
        file_path: z
          .string()
          .describe('Absolute path to a Gerbil source file (.ss or .scm)'),
        function: z
          .string()
          .optional()
          .describe(
            'If provided, only show the call tree rooted at this function',
          ),
      },
    },
    async ({ file_path, function: filterFn }) => {
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

      if (!content.trim()) {
        return {
          content: [
            { type: 'text' as const, text: 'File is empty.' },
          ],
        };
      }

      const analysis = parseDefinitions(content);
      const lines = content.split('\n');

      // Filter to function-like definitions
      const funcKinds = new Set([
        'procedure',
        'inline',
        'method',
      ]);
      const funcDefs = analysis.definitions.filter((d) =>
        funcKinds.has(d.kind),
      );

      if (funcDefs.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No function definitions found in ${file_path}.`,
            },
          ],
        };
      }

      const funcNames = new Set(funcDefs.map((d) => d.name));

      // Build adjacency list: function -> list of local functions it calls
      const graph = new Map<string, string[]>();
      for (const def of funcDefs) {
        const body = extractFunctionBody(lines, def.line - 1); // line is 1-indexed
        const callees = findLocalCalls(body, funcNames, def.name);
        graph.set(def.name, callees);
      }

      // Build line number lookup
      const lineMap = new Map<string, number>();
      for (const def of funcDefs) {
        lineMap.set(def.name, def.line);
      }

      // Format output
      const sections: string[] = [`Call graph: ${file_path}`, ''];

      if (filterFn) {
        if (!funcNames.has(filterFn)) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Function "${filterFn}" not found in ${file_path}.\n\nDefined functions: ${[...funcNames].join(', ')}`,
              },
            ],
            isError: true,
          };
        }

        // Show tree rooted at filterFn
        const treeLines = buildTree(graph, filterFn, new Set());
        sections.push(...treeLines);
        sections.push('');

        // Show flat listing for the subtree
        const reachable = getReachable(graph, filterFn);
        sections.push(`Functions in subtree: ${reachable.size}`);
        for (const name of reachable) {
          const callees = graph.get(name) || [];
          const line = lineMap.get(name) || 0;
          const calleesStr =
            callees.length > 0
              ? callees.join(', ')
              : '(no calls to local functions)';
          sections.push(`  ${name} (L${line}) -> ${calleesStr}`);
        }
      } else {
        // Find root functions (not called by any other function)
        const calledBy = new Set<string>();
        for (const callees of graph.values()) {
          for (const c of callees) {
            calledBy.add(c);
          }
        }
        const roots = funcDefs
          .filter((d) => !calledBy.has(d.name))
          .map((d) => d.name);

        // Show tree for each root
        if (roots.length > 0) {
          for (const root of roots) {
            const treeLines = buildTree(graph, root, new Set());
            sections.push(...treeLines);
            sections.push('');
          }
        }

        // Show flat listing for all functions
        sections.push(`Defined functions: ${funcDefs.length}`);
        for (const def of funcDefs) {
          const callees = graph.get(def.name) || [];
          const calleesStr =
            callees.length > 0
              ? callees.join(', ')
              : '(no calls to local functions)';
          sections.push(
            `  ${def.name} (L${def.line}) -> ${calleesStr}`,
          );
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

/**
 * Extract the body of a function definition starting at the given 0-indexed line.
 * Tracks paren depth to find the end of the form.
 */
function extractFunctionBody(
  lines: string[],
  startIdx: number,
): string {
  let depth = 0;
  let started = false;
  const bodyLines: string[] = [];

  for (let i = startIdx; i < lines.length; i++) {
    const line = lines[i];
    bodyLines.push(line);

    for (let j = 0; j < line.length; j++) {
      const ch = line[j];
      if (ch === ';') break; // skip rest of line (comment)
      if (ch === '"') {
        // skip string literal
        j++;
        while (j < line.length && line[j] !== '"') {
          if (line[j] === '\\') j++;
          j++;
        }
        continue;
      }
      if (ch === '(' || ch === '[') {
        depth++;
        started = true;
      } else if (ch === ')' || ch === ']') {
        depth--;
        if (started && depth <= 0) {
          return bodyLines.join('\n');
        }
      }
    }
  }

  return bodyLines.join('\n');
}

/**
 * Find references to locally defined functions within a function body.
 * Uses word-boundary detection and skips strings and comments.
 */
function findLocalCalls(
  body: string,
  definedNames: Set<string>,
  selfName: string,
): string[] {
  const found = new Set<string>();
  const lines = body.split('\n');

  for (const line of lines) {
    let i = 0;
    while (i < line.length) {
      const ch = line[i];

      // Skip comments
      if (ch === ';') break;

      // Skip string literals
      if (ch === '"') {
        i++;
        while (i < line.length && line[i] !== '"') {
          if (line[i] === '\\') i++;
          i++;
        }
        i++;
        continue;
      }

      // Check for identifier start (at word boundary)
      if (!isSchemeDelimiter(ch)) {
        // Read the full identifier
        const start = i;
        while (i < line.length && !isSchemeDelimiter(line[i])) {
          i++;
        }
        const ident = line.slice(start, i);

        // Check if it's a locally defined function (not self)
        if (ident !== selfName && definedNames.has(ident)) {
          found.add(ident);
        }
        continue;
      }

      i++;
    }
  }

  return [...found].sort();
}

/**
 * Build a tree visualization starting from a root function.
 */
function buildTree(
  graph: Map<string, string[]>,
  root: string,
  visited: Set<string>,
): string[] {
  const result: string[] = [root];
  visited.add(root);

  const callees = graph.get(root) || [];
  for (let i = 0; i < callees.length; i++) {
    const callee = callees[i];
    const isLast = i === callees.length - 1;
    const prefix = isLast ? '+-- ' : '+-- ';
    const childPrefix = isLast ? '    ' : '|   ';

    if (visited.has(callee)) {
      result.push(`  ${prefix}${callee} (recursive)`);
    } else {
      const subtree = buildTree(graph, callee, new Set(visited));
      result.push(`  ${prefix}${subtree[0]}`);
      for (let j = 1; j < subtree.length; j++) {
        result.push(`  ${childPrefix}${subtree[j]}`);
      }
    }
  }

  return result;
}

/**
 * Get all functions reachable from a root in the call graph.
 */
function getReachable(
  graph: Map<string, string[]>,
  root: string,
): Set<string> {
  const visited = new Set<string>();
  const stack = [root];
  while (stack.length > 0) {
    const fn = stack.pop()!;
    if (visited.has(fn)) continue;
    visited.add(fn);
    const callees = graph.get(fn) || [];
    for (const c of callees) {
      if (!visited.has(c)) {
        stack.push(c);
      }
    }
  }
  return visited;
}
