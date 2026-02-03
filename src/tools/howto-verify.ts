import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString } from '../gxi.js';
import { RECIPES, loadCookbook, REPO_COOKBOOK_PATH, type Recipe } from './howto.js';

const PASS_MARKER = 'GERBIL-MCP-VERIFY-PASS:';
const FAIL_MARKER = 'GERBIL-MCP-VERIFY-FAIL:';
const BATCH_SIZE = 5;

export function registerHowtoVerifyTool(server: McpServer): void {
  server.registerTool(
    'gerbil_howto_verify',
    {
      title: 'Verify Cookbook Recipes',
      description:
        'Verify that cookbook recipes have valid syntax by checking their imports and code ' +
        'against the Gerbil expander. Does not execute recipes — only checks that they parse ' +
        'and expand correctly. Reports pass/fail for each recipe.',
      inputSchema: {
        cookbook_path: z
          .string()
          .optional()
          .describe(
            'Absolute path to an external cookbook JSON file to verify (merged with built-in recipes)',
          ),
        recipe_id: z
          .string()
          .optional()
          .describe('Single recipe ID to verify. If omitted, verifies all recipes.'),
      },
    },
    async ({ cookbook_path, recipe_id }) => {
      // Load and merge recipes
      let recipes: Recipe[] = [...RECIPES];
      const sources = [REPO_COOKBOOK_PATH];
      if (cookbook_path) sources.push(cookbook_path);
      for (const src of sources) {
        const external = loadCookbook(src);
        if (external.length > 0) {
          const externalIds = new Set(external.map((r) => r.id));
          recipes = recipes.filter((r) => !externalIds.has(r.id));
          recipes.push(...external);
        }
      }

      // Filter by recipe_id if provided
      if (recipe_id) {
        recipes = recipes.filter((r) => r.id === recipe_id);
        if (recipes.length === 0) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Recipe "${recipe_id}" not found.`,
              },
            ],
            isError: true,
          };
        }
      }

      // Verify in batches
      const results: Array<{ id: string; passed: boolean; error?: string }> = [];

      for (let i = 0; i < recipes.length; i += BATCH_SIZE) {
        const batch = recipes.slice(i, i + BATCH_SIZE);
        const batchResults = await verifyBatch(batch);
        results.push(...batchResults);
      }

      // Format output
      const passed = results.filter((r) => r.passed);
      const failed = results.filter((r) => !r.passed);

      const sections: string[] = [
        `Cookbook verification: ${results.length} recipe(s) checked`,
        '',
      ];

      for (const r of results) {
        if (r.passed) {
          sections.push(`  PASS  ${r.id}`);
        } else {
          sections.push(`  FAIL  ${r.id} \u2014 ${r.error || 'unknown error'}`);
        }
      }

      sections.push('');
      sections.push(`Summary: ${passed.length} passed, ${failed.length} failed`);

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: failed.length > 0,
      };
    },
  );
}

async function verifyBatch(
  recipes: Recipe[],
): Promise<Array<{ id: string; passed: boolean; error?: string }>> {
  // Build a single gxi expression that checks each recipe
  const checks = recipes.map((recipe) => {
    const importExprs = recipe.imports
      .map((imp) => `(import ${imp})`)
      .join(' ');
    const escapedId = escapeSchemeString(recipe.id);
    const escapedCode = escapeSchemeString(recipe.code);

    return [
      '(with-catch',
      '  (lambda (e)',
      `    (display "${FAIL_MARKER}${escapedId}\\t")`,
      '    (display-exception e (current-output-port)))',
      '  (lambda ()',
      importExprs ? `    ${importExprs}` : '',
      `    (let ((p (open-input-string "${escapedCode}")))`,
      '      (let loop ((form (read p)))',
      '        (unless (eof-object? form)',
      '          (core-expand form)',
      '          (loop (read p)))))',
      `    (display "${PASS_MARKER}${escapedId}\\n")))`,
    ]
      .filter(Boolean)
      .join(' ');
  });

  const fullExpr = `(import :gerbil/expander) ${checks.join(' ')}`;

  const result = await runGxi([fullExpr], { timeout: 60_000 });

  // Parse results
  const output = result.stdout;
  const resultMap = new Map<string, { passed: boolean; error?: string }>();

  for (const line of output.split('\n')) {
    if (line.startsWith(PASS_MARKER)) {
      const id = line.slice(PASS_MARKER.length).trim();
      resultMap.set(id, { passed: true });
    } else if (line.startsWith(FAIL_MARKER)) {
      const rest = line.slice(FAIL_MARKER.length);
      const tabIdx = rest.indexOf('\t');
      if (tabIdx !== -1) {
        const id = rest.slice(0, tabIdx);
        const error = rest.slice(tabIdx + 1).trim();
        resultMap.set(id, { passed: false, error });
      } else {
        const id = rest.trim();
        resultMap.set(id, { passed: false, error: 'verification failed' });
      }
    }
  }

  // Build results preserving order, marking missing ones as failed
  return recipes.map((recipe) => {
    const r = resultMap.get(recipe.id);
    if (r) {
      return { id: recipe.id, ...r };
    }
    // If a recipe didn't produce output, it might have crashed the batch
    if (result.timedOut) {
      return { id: recipe.id, passed: false, error: 'batch timed out' };
    }
    if (result.exitCode !== 0) {
      return {
        id: recipe.id,
        passed: false,
        error: `batch failed (exit ${result.exitCode})`,
      };
    }
    return { id: recipe.id, passed: false, error: 'no result (batch crash?)' };
  });
}
