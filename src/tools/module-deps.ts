import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, ERROR_MARKER } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-DEP:';
const MAX_TRANSITIVE_DEPTH = 20;

export function registerModuleDepsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_module_deps',
    {
      title: 'Show Module Dependencies',
      description:
        'Show what modules a given Gerbil module imports. ' +
        'Returns the direct dependency module IDs. ' +
        'Use transitive: true to recursively resolve the full dependency tree. ' +
        'Example: module_path ":std/text/json" shows its sub-module dependencies.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        module_path: z
          .string()
          .describe('Module path (e.g. ":std/text/json", ":std/net/httpd")'),
        transitive: z
          .boolean()
          .optional()
          .describe(
            'If true, recursively resolve all transitive dependencies. Default: false.',
          ),
      },
    },
    async ({ module_path, transitive }) => {
      const modPath = module_path.startsWith(':')
        ? module_path
        : `:${module_path}`;

      const resolveTransitive = transitive === true;

      if (!resolveTransitive) {
        return await resolveDirectDeps(modPath);
      }

      // Transitive: iteratively resolve deps in TypeScript
      return await resolveTransitiveDeps(modPath);
    },
  );
}

async function getDirectDeps(
  modPath: string,
): Promise<{ deps: string[]; error?: string }> {
  const exprs = [
    '(import :gerbil/expander)',
    buildDirectExpr(modPath),
  ];

  const result = await runGxi(exprs);

  if (result.timedOut) {
    return { deps: [], error: 'Module dependency resolution timed out.' };
  }

  if (result.exitCode !== 0 && result.stderr) {
    return {
      deps: [],
      error: `Failed to load module ${modPath}:\n${result.stderr.trim()}`,
    };
  }

  const stdout = result.stdout;
  const errorIdx = stdout.indexOf(ERROR_MARKER);
  if (errorIdx !== -1) {
    const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
    return {
      deps: [],
      error: `Error resolving deps for ${modPath}:\n${errorMsg}`,
    };
  }

  const deps = stdout
    .split('\n')
    .filter((l) => l.startsWith(RESULT_MARKER))
    .map((l) => l.slice(RESULT_MARKER.length).trim())
    .filter(Boolean);

  return { deps };
}

async function resolveDirectDeps(modPath: string) {
  const { deps, error } = await getDirectDeps(modPath);

  if (error) {
    return {
      content: [{ type: 'text' as const, text: error }],
      isError: true,
    };
  }

  if (deps.length === 0) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Module ${modPath} has no detected module dependencies.`,
        },
      ],
    };
  }

  const formatted = [
    `${modPath} — ${deps.length} direct dependencies:`,
    '',
    ...deps.map((d) => `  :${d}`),
  ].join('\n');

  return { content: [{ type: 'text' as const, text: formatted }] };
}

async function resolveTransitiveDeps(modPath: string) {
  const visited = new Set<string>();
  const queue: string[] = [modPath];
  const tree: Array<{ mod: string; deps: string[] }> = [];

  let iterations = 0;
  while (queue.length > 0 && iterations < MAX_TRANSITIVE_DEPTH) {
    const current = queue.shift()!;
    const normalizedCurrent = current.startsWith(':')
      ? current.slice(1)
      : current;

    if (visited.has(normalizedCurrent)) continue;
    visited.add(normalizedCurrent);
    iterations++;

    const qualifiedPath = current.startsWith(':') ? current : `:${current}`;
    const { deps, error } = await getDirectDeps(qualifiedPath);

    if (error) continue; // Skip modules that can't be resolved

    tree.push({ mod: normalizedCurrent, deps });

    for (const dep of deps) {
      if (!visited.has(dep)) {
        queue.push(`:${dep}`);
      }
    }
  }

  if (tree.length === 0) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Module ${modPath} has no detected module dependencies.`,
        },
      ],
    };
  }

  const allDeps = new Set<string>();
  for (const entry of tree) {
    for (const dep of entry.deps) {
      allDeps.add(dep);
    }
  }

  const sections: string[] = [
    `${modPath} — transitive dependency tree (${allDeps.size} unique modules):`,
    '',
  ];

  for (const entry of tree) {
    if (entry.deps.length > 0) {
      sections.push(`  :${entry.mod}`);
      for (const dep of entry.deps) {
        sections.push(`    -> :${dep}`);
      }
    }
  }

  return { content: [{ type: 'text' as const, text: sections.join('\n') }] };
}

function buildDirectExpr(modPath: string): string {
  return [
    '(with-catch',
    '  (lambda (e)',
    `    (display "${ERROR_MARKER}\\n")`,
    '    (display-exception e (current-output-port)))',
    '  (lambda ()',
    `    (let* ((mod (import-module (quote ${modPath}) #f #t))`,
    '           (imports (module-context-import mod)))',
    '      (for-each',
    '        (lambda (imp)',
    '          (when (import-set? imp)',
    '            (let ((src (import-set-source imp)))',
    '              (when (module-context? src)',
    `                (display "${RESULT_MARKER}")`,
    '                (displayln (expander-context-id src))))))',
    '        imports))))',
  ].join(' ');
}
