import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { writeFile, unlink } from 'node:fs/promises';
import { join } from 'node:path';
import { randomUUID } from 'node:crypto';
import { tmpdir } from 'node:os';
import { runGxi, runGxc } from '../gxi.js';
import { RECIPES, loadCookbook, REPO_COOKBOOK_PATH, type Recipe } from './howto.js';

export function registerHowtoRunTool(server: McpServer): void {
  server.registerTool(
    'gerbil_howto_run',
    {
      title: 'Run Cookbook Recipe',
      description:
        'Execute a cookbook recipe to verify it works in the current Gerbil environment. ' +
        'Takes a recipe ID, extracts its imports and code, compile-checks it, and optionally ' +
        'runs it with a timeout. Reports success/failure with output or error. ' +
        'Useful for validating recipes before recommending them.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        recipe_id: z
          .string()
          .describe('The cookbook recipe ID to execute (e.g. "json-parse", "hash-table-basics")'),
        execute: z
          .boolean()
          .optional()
          .describe(
            'If true, actually execute the recipe code (not just compile-check). ' +
            'Default: false (compile-check only). Execution has a 10-second timeout.',
          ),
        cookbook_path: z
          .string()
          .optional()
          .describe('Path to an additional cookbook JSON file to merge'),
      },
    },
    async ({ recipe_id, execute, cookbook_path }) => {
      // Load and merge recipes
      let recipes: Recipe[] = [...RECIPES];
      const sources = [REPO_COOKBOOK_PATH];
      if (cookbook_path) sources.push(cookbook_path);
      for (const src of sources) {
        const external = loadCookbook(src);
        if (external.length > 0) {
          const externalIds = new Set(external.map(r => r.id));
          recipes = recipes.filter(r => !externalIds.has(r.id));
          recipes.push(...external);
        }
      }

      // Find recipe
      const recipe = recipes.find(r => r.id === recipe_id);
      if (!recipe) {
        return {
          content: [{
            type: 'text' as const,
            text: `Recipe "${recipe_id}" not found. Use gerbil_howto to search for recipes.`,
          }],
          isError: true,
        };
      }

      if (recipe.deprecated) {
        const supersededMsg = recipe.superseded_by
          ? ` Superseded by: "${recipe.superseded_by}".`
          : '';
        return {
          content: [{
            type: 'text' as const,
            text: `Recipe "${recipe_id}" is deprecated.${supersededMsg} Try gerbil_howto to find an alternative.`,
          }],
          isError: true,
        };
      }

      // Build source code from recipe
      const importLines = recipe.imports.map(m => `(import ${m})`).join('\n');
      const fullCode = importLines + '\n' + recipe.code;

      // Write to temp file
      const tempName = `gerbil-recipe-${randomUUID().slice(0, 8)}.ss`;
      const tempPath = join(tmpdir(), tempName);
      try {
        await writeFile(tempPath, fullCode, 'utf-8');
      } catch (err) {
        return {
          content: [{
            type: 'text' as const,
            text: `Failed to write temp file: ${err instanceof Error ? err.message : String(err)}`,
          }],
          isError: true,
        };
      }

      const sections: string[] = [`## Recipe: ${recipe.title} (\`${recipe.id}\`)\n`];

      try {
        // Phase 1: Compile check
        const compileResult = await runGxc(tempPath);
        if (compileResult.exitCode !== 0) {
          let error = compileResult.stderr.trim() || compileResult.stdout.trim();
          error = error.replaceAll(tempPath, '<recipe>');
          sections.push('### Compile Check: FAIL\n');
          sections.push('```\n' + error + '\n```\n');
          sections.push('The recipe does not compile in the current environment.');
          return {
            content: [{ type: 'text' as const, text: sections.join('\n') }],
            isError: true,
          };
        }
        sections.push('### Compile Check: PASS\n');

        // Phase 2: Execute (if requested)
        if (execute) {
          try {
            // Run via gxi with timeout
            const exprs = recipe.imports.map(m => `(import ${m})`);
            exprs.push(recipe.code);
            const runResult = await runGxi(exprs, { timeout: 10000 });

            if (runResult.timedOut) {
              sections.push('### Execution: TIMEOUT (10s)\n');
              sections.push('The recipe timed out during execution.');
            } else if (runResult.exitCode !== 0) {
              let error = runResult.stderr.trim() || runResult.stdout.trim();
              sections.push('### Execution: ERROR\n');
              sections.push('```\n' + error + '\n```');
            } else {
              const output = runResult.stdout.trim();
              sections.push('### Execution: SUCCESS\n');
              if (output) {
                sections.push('Output:\n```\n' + output + '\n```');
              } else {
                sections.push('(no output)');
              }
            }
          } catch (e) {
            sections.push(`### Execution: ERROR\n\n${e}`);
          }
        }

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      } finally {
        try { await unlink(tempPath); } catch { /* ignore */ }
      }
    },
  );
}
