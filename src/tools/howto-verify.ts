import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { findGxi, findGxc } from '../gxi.js';
import { RECIPES, loadCookbook, REPO_COOKBOOK_PATH, type Recipe } from './howto.js';
import {
  DEFAULT_BATCH_SIZE,
  runVerifyBatch,
  runCompileCheckBatch,
} from './verify-utils.js';

export function registerHowtoVerifyTool(server: McpServer): void {
  server.registerTool(
    'gerbil_howto_verify',
    {
      title: 'Verify Cookbook Recipes',
      description:
        'Verify that cookbook recipes have valid syntax by checking their imports and code ' +
        'against the Gerbil expander. Does not execute recipes — only checks that they parse ' +
        'and expand correctly. Reports pass/fail for each recipe. ' +
        'Use compile_check: true to also run gxc compilation, which catches unbound identifiers ' +
        'and other issues that the expander alone misses (e.g. REPL-only patterns).',
      annotations: {
        readOnlyHint: false,
        idempotentHint: true,
      },
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
        compile_check: z
          .boolean()
          .optional()
          .describe(
            'If true, also compile-check recipes with gxc (not just syntax expansion). ' +
            'This catches unbound identifiers and REPL-only patterns that fail during compilation. ' +
            'Slower but more thorough. Default: false.',
          ),
        gerbil_version: z
          .string()
          .optional()
          .describe(
            'Only verify recipes matching this Gerbil version (e.g. "v0.18", "v0.19"). ' +
            'Untagged recipes are always included. If omitted, verifies all recipes.',
          ),
      },
    },
    async ({ cookbook_path, recipe_id, compile_check, gerbil_version }) => {
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

      // Filter by gerbil_version if provided (untagged recipes always included)
      if (gerbil_version) {
        recipes = recipes.filter(
          (r) => !r.gerbil_version || r.gerbil_version === gerbil_version,
        );
      }

      // Resolve binary paths
      const gxiPath = await findGxi();
      const gxcPath = await findGxc();

      // Verify in batches (syntax expansion)
      const results: Array<{ id: string; passed: boolean; error?: string; compileError?: string }> = [];

      for (let i = 0; i < recipes.length; i += DEFAULT_BATCH_SIZE) {
        const batch = recipes.slice(i, i + DEFAULT_BATCH_SIZE);
        const batchResults = await runVerifyBatch(batch, gxiPath, { timeout: 60_000 });
        results.push(...batchResults);
      }

      // Compile-check if requested (only for recipes that passed syntax check)
      if (compile_check) {
        const passedRecipes = results
          .filter((r) => r.passed)
          .map((r) => recipes.find((rec) => rec.id === r.id)!)
          .filter(Boolean);

        if (passedRecipes.length > 0) {
          const compileResults = await runCompileCheckBatch(passedRecipes, gxcPath);
          for (const cr of compileResults) {
            const existing = results.find((r) => r.id === cr.id);
            if (existing && !cr.passed) {
              existing.compileError = cr.error;
            }
          }
        }
      }

      // Format output
      const mode = compile_check ? 'syntax + compile' : 'syntax';
      const versionLabel = gerbil_version ? `, version: ${gerbil_version}` : '';
      const syntaxPassed = results.filter((r) => r.passed);
      const syntaxFailed = results.filter((r) => !r.passed);
      const compileFailed = compile_check
        ? results.filter((r) => r.passed && r.compileError)
        : [];

      const sections: string[] = [
        `Cookbook verification (${mode}${versionLabel}): ${results.length} recipe(s) checked`,
        '',
      ];

      // Build a map of recipe id -> recipe for display
      const recipeMap = new Map<string, Recipe>();
      for (const rec of recipes) {
        recipeMap.set(rec.id, rec);
      }

      for (const r of results) {
        const rec = recipeMap.get(r.id);
        const verTag = rec?.gerbil_version ? ` [${rec.gerbil_version}]` : '';
        const validForTag =
          rec?.valid_for && rec.valid_for.length > 0
            ? ` (tested: ${rec.valid_for.map((v) => { const m = v.match(/^(v\d+\.\d+)/); return m ? m[1] : v; }).filter((v, i, a) => a.indexOf(v) === i).join(', ')})`
            : '';
        if (!r.passed) {
          sections.push(`  FAIL  ${r.id}${verTag}${validForTag} — ${r.error || 'unknown error'}`);
        } else if (r.compileError) {
          sections.push(`  COMPILE-FAIL  ${r.id}${verTag}${validForTag} — ${r.compileError} (repl_only?)`);
        } else {
          sections.push(`  PASS  ${r.id}${verTag}${validForTag}`);
        }
      }

      sections.push('');
      const summaryParts = [`${syntaxPassed.length} syntax passed`, `${syntaxFailed.length} syntax failed`];
      if (compile_check) {
        summaryParts.push(`${compileFailed.length} compile failed`);
      }
      sections.push(`Summary: ${summaryParts.join(', ')}`);

      const hasFailures = syntaxFailed.length > 0 || compileFailed.length > 0;
      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: hasFailures,
      };
    },
  );
}
