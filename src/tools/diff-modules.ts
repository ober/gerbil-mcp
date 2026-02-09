import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join } from 'node:path';
import { runGxi, ERROR_MARKER, RESULT_MARKER, buildLoadpathEnv } from '../gxi.js';

/**
 * Helper to extract module exports via gxi.
 */
async function getModuleExports(
  modPath: string,
  env?: Record<string, string>,
): Promise<{ symbols: string[]; error?: string }> {
  const exprs = [
    '(import :gerbil/expander)',
    [
      '(with-catch',
      '  (lambda (e)',
      `    (display "${ERROR_MARKER}\\n")`,
      '    (display-exception e (current-output-port)))',
      '  (lambda ()',
      `    (display "${RESULT_MARKER}\\n")`,
      `    (let ((mod (import-module (quote ${modPath}) #f #t)))`,
      '      (let ((exports (module-context-export mod)))',
      '        (for-each',
      '          (lambda (e)',
      '            (displayln (module-export-name e)))',
      '          exports)))))',
    ].join(' '),
  ];

  const result = await runGxi(exprs, { env });

  if (result.timedOut) {
    return { symbols: [], error: 'Timed out' };
  }

  const stdout = result.stdout;
  const errorIdx = stdout.indexOf(ERROR_MARKER);
  if (errorIdx !== -1) {
    return { symbols: [], error: stdout.slice(errorIdx + ERROR_MARKER.length).trim() };
  }

  const resultIdx = stdout.indexOf(RESULT_MARKER);
  if (resultIdx === -1) {
    return { symbols: [], error: 'No output' };
  }

  const symbols = stdout
    .slice(resultIdx + RESULT_MARKER.length)
    .trim()
    .split('\n')
    .map((s) => s.trim())
    .filter(Boolean);

  return { symbols };
}

export function registerDiffModulesTool(server: McpServer): void {
  server.registerTool(
    'gerbil_diff_modules',
    {
      title: 'Diff Module Exports',
      description:
        'Compare two modules and show added/removed/shared exports. ' +
        'Critical for v0.18 to v0.19 migration work or comparing any two modules. ' +
        'Shows a clear diff of what changed between them.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        module_a: z.string().describe('First module path (e.g. ":std/getopt")'),
        module_b: z.string().describe('Second module path (e.g. ":std/cli/getopt")'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Directories to add to GERBIL_LOADPATH'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib'),
      },
    },
    async ({ module_a, module_b, loadpath, project_path }) => {
      const modA = module_a.startsWith(':') ? module_a : `:${module_a}`;
      const modB = module_b.startsWith(':') ? module_b : `:${module_b}`;

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const env = effectiveLoadpath.length > 0 ? buildLoadpathEnv(effectiveLoadpath) : undefined;

      // Fetch exports from both modules in parallel
      const [resultA, resultB] = await Promise.all([
        getModuleExports(modA, env),
        getModuleExports(modB, env),
      ]);

      if (resultA.error && resultB.error) {
        return {
          content: [{
            type: 'text' as const,
            text: `Failed to load both modules:\n- ${modA}: ${resultA.error}\n- ${modB}: ${resultB.error}`,
          }],
          isError: true,
        };
      }

      const setA = new Set(resultA.symbols);
      const setB = new Set(resultB.symbols);

      const onlyInA = resultA.symbols.filter((s) => !setB.has(s)).sort();
      const onlyInB = resultB.symbols.filter((s) => !setA.has(s)).sort();
      const shared = resultA.symbols.filter((s) => setB.has(s)).sort();

      const sections: string[] = [];
      sections.push(`## Module Diff: ${modA} vs ${modB}\n`);

      if (resultA.error) {
        sections.push(`**Warning**: Could not load ${modA}: ${resultA.error}\n`);
      }
      if (resultB.error) {
        sections.push(`**Warning**: Could not load ${modB}: ${resultB.error}\n`);
      }

      sections.push(`| | ${modA} | ${modB} |`);
      sections.push(`|---|---|---|`);
      sections.push(`| Total exports | ${resultA.symbols.length} | ${resultB.symbols.length} |`);
      sections.push(`| Only in this module | ${onlyInA.length} | ${onlyInB.length} |`);
      sections.push(`| Shared | ${shared.length} | ${shared.length} |`);
      sections.push('');

      if (onlyInA.length > 0) {
        sections.push(`### Only in ${modA} (${onlyInA.length}):`);
        sections.push(onlyInA.map((s) => `  - ${s}`).join('\n'));
        sections.push('');
      }

      if (onlyInB.length > 0) {
        sections.push(`### Only in ${modB} (${onlyInB.length}):`);
        sections.push(onlyInB.map((s) => `  + ${s}`).join('\n'));
        sections.push('');
      }

      if (shared.length > 0) {
        sections.push(`### Shared (${shared.length}):`);
        sections.push(shared.map((s) => `    ${s}`).join('\n'));
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
