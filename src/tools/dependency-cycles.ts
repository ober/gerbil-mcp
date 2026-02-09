import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, readdir, stat } from 'node:fs/promises';
import { join, relative } from 'node:path';

/**
 * Recursively find all .ss files in a directory.
 */
async function findSsFiles(dir: string): Promise<string[]> {
  const files: string[] = [];
  try {
    const entries = await readdir(dir, { withFileTypes: true });
    for (const entry of entries) {
      const fullPath = join(dir, entry.name);
      if (entry.isDirectory() && !entry.name.startsWith('.') && entry.name !== 'node_modules') {
        files.push(...await findSsFiles(fullPath));
      } else if (entry.isFile() && entry.name.endsWith('.ss')) {
        files.push(fullPath);
      }
    }
  } catch {
    // Skip directories we can't read
  }
  return files;
}

/**
 * Extract import module paths from file content.
 * Only captures project-local imports (not :std/*, :gerbil/*, etc.)
 */
function extractLocalImports(content: string, packagePrefix: string): string[] {
  const imports: string[] = [];
  // Match (import :prefix/module ...) patterns
  const importPattern = /\(import\s+([^)]+)\)/g;
  let match: RegExpExecArray | null;
  while ((match = importPattern.exec(content)) !== null) {
    const body = match[1];
    // Find module paths that start with the package prefix
    const modPattern = new RegExp(`:${packagePrefix}/[a-zA-Z0-9_/.-]+`, 'g');
    let modMatch: RegExpExecArray | null;
    while ((modMatch = modPattern.exec(body)) !== null) {
      imports.push(modMatch[0]);
    }
  }
  return imports;
}

/**
 * Read gerbil.pkg to get package prefix.
 */
async function readPackagePrefix(projectPath: string): Promise<string | null> {
  try {
    const content = await readFile(join(projectPath, 'gerbil.pkg'), 'utf-8');
    const match = content.match(/package:\s*([a-zA-Z0-9_/.-]+)/);
    return match ? match[1] : null;
  } catch {
    return null;
  }
}

/**
 * Convert a file path to a module path given the project path and package prefix.
 */
function fileToModulePath(filePath: string, projectPath: string, prefix: string): string {
  let rel = relative(projectPath, filePath);
  // Remove .ss extension
  rel = rel.replace(/\.ss$/, '');
  return `:${prefix}/${rel}`;
}

/**
 * Find all cycles in a directed graph using DFS.
 */
function findCycles(graph: Map<string, string[]>): string[][] {
  const cycles: string[][] = [];
  const visited = new Set<string>();
  const inStack = new Set<string>();
  const stack: string[] = [];

  function dfs(node: string): void {
    if (inStack.has(node)) {
      // Found a cycle — extract it from the stack
      const cycleStart = stack.indexOf(node);
      if (cycleStart !== -1) {
        const cycle = stack.slice(cycleStart);
        cycle.push(node); // Complete the cycle
        cycles.push(cycle);
      }
      return;
    }

    if (visited.has(node)) return;

    visited.add(node);
    inStack.add(node);
    stack.push(node);

    const neighbors = graph.get(node) ?? [];
    for (const neighbor of neighbors) {
      if (graph.has(neighbor)) {
        // Only follow edges to known project modules
        dfs(neighbor);
      }
    }

    stack.pop();
    inStack.delete(node);
  }

  for (const node of graph.keys()) {
    dfs(node);
  }

  return cycles;
}

export function registerDependencyCyclesTool(server: McpServer): void {
  server.registerTool(
    'gerbil_dependency_cycles',
    {
      title: 'Detect Dependency Cycles',
      description:
        'Detect circular module dependencies in a Gerbil project. ' +
        'Circular imports cause cryptic compilation errors. This tool builds ' +
        'a dependency graph from import statements and reports any cycles found.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        project_path: z.string().describe('Path to the Gerbil project directory (must contain gerbil.pkg)'),
      },
    },
    async ({ project_path }) => {
      // Read package prefix
      const prefix = await readPackagePrefix(project_path);
      if (!prefix) {
        return {
          content: [{
            type: 'text' as const,
            text: `No gerbil.pkg found in ${project_path}. This tool requires a Gerbil project with a package: declaration.`,
          }],
          isError: true,
        };
      }

      const ssFiles = await findSsFiles(project_path);
      if (ssFiles.length === 0) {
        return {
          content: [{ type: 'text' as const, text: `No .ss files found in ${project_path}` }],
        };
      }

      // Build dependency graph
      const graph = new Map<string, string[]>();
      const moduleToFile = new Map<string, string>();

      for (const file of ssFiles) {
        const modPath = fileToModulePath(file, project_path, prefix);
        const content = await readFile(file, 'utf-8');
        const imports = extractLocalImports(content, prefix);
        graph.set(modPath, imports);
        moduleToFile.set(modPath, relative(project_path, file));
      }

      // Find cycles
      const cycles = findCycles(graph);

      if (cycles.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: `No circular dependencies detected in ${ssFiles.length} module(s) in ${project_path}.`,
          }],
        };
      }

      // Deduplicate cycles (same cycle can be found from different starting points)
      const uniqueCycles: string[][] = [];
      const seen = new Set<string>();
      for (const cycle of cycles) {
        // Normalize: rotate so smallest element is first
        const minIdx = cycle.indexOf(
          cycle.slice(0, -1).reduce((a, b) => (a < b ? a : b)),
        );
        const normalized = [
          ...cycle.slice(minIdx, -1),
          ...cycle.slice(0, minIdx),
          cycle[minIdx],
        ];
        const key = normalized.join(' -> ');
        if (!seen.has(key)) {
          seen.add(key);
          uniqueCycles.push(normalized);
        }
      }

      const sections: string[] = [];
      sections.push(`## Circular Dependencies: ${project_path}\n`);
      sections.push(`Found ${uniqueCycles.length} cycle(s) in ${ssFiles.length} module(s):\n`);

      for (let i = 0; i < uniqueCycles.length; i++) {
        const cycle = uniqueCycles[i];
        sections.push(`### Cycle ${i + 1}`);
        sections.push('```');
        sections.push(cycle.join(' → '));
        sections.push('```');
        sections.push('');
      }

      sections.push(
        '\n## How to Fix\n' +
        '- Extract shared types/interfaces into a separate module that both can import\n' +
        '- Use late binding or dynamic dispatch to break the cycle\n' +
        '- Consider merging tightly coupled modules\n' +
        '- Use `gerbil_project_dep_graph` to visualize the full dependency tree\n',
      );

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
