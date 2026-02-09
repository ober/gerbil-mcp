import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { RECIPES, REPO_COOKBOOK_PATH, loadCookbook, type Recipe } from './howto.js';

export function registerHowtoGetTool(server: McpServer): void {
  server.registerTool(
    'gerbil_howto_get',
    {
      title: 'Get Gerbil Recipe by ID',
      description:
        'Fetch a single cookbook recipe by its ID. Returns the full recipe with code, ' +
        'imports, notes, and related recipes. Use after gerbil_howto with compact: true ' +
        'to retrieve only the recipes you need.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        id: z
          .string()
          .describe('Recipe ID to fetch (e.g. "json-parse", "hash-table-basics")'),
        cookbook_path: z
          .string()
          .optional()
          .describe(
            'Absolute path to a JSON cookbook file with additional recipes to merge',
          ),
      },
    },
    async ({ id, cookbook_path }) => {
      // Load all recipes (built-in + repo + optional external)
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

      const recipe = recipes.find((r) => r.id === id);
      if (!recipe) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Recipe "${id}" not found. Use gerbil_howto to search for recipes.`,
            },
          ],
          isError: true,
        };
      }

      const sections: string[] = [];
      const versionTag = recipe.gerbil_version ? ` [${recipe.gerbil_version}]` : '';
      const testedTag =
        recipe.valid_for && recipe.valid_for.length > 0
          ? ` (tested: ${recipe.valid_for.map((v) => { const m = v.match(/^(v\d+\.\d+)/); return m ? m[1] : v; }).filter((v, i, a) => a.indexOf(v) === i).join(', ')})`
          : '';

      if (recipe.deprecated) {
        sections.push(`## [DEPRECATED]${versionTag} ${recipe.title}`);
        if (recipe.superseded_by) {
          sections.push(`Superseded by: "${recipe.superseded_by}"`);
        }
      } else {
        sections.push(`## ${recipe.title}${versionTag}${testedTag}`);
      }
      sections.push(`ID: ${recipe.id}`);
      sections.push(`Tags: ${recipe.tags.join(', ')}`);
      if (recipe.imports.length > 0) {
        sections.push(`Imports: ${recipe.imports.join(' ')}`);
      }
      sections.push('```scheme');
      sections.push(recipe.code);
      sections.push('```');
      if (recipe.notes) {
        sections.push(`Note: ${recipe.notes}`);
      }
      if (recipe.related && recipe.related.length > 0) {
        sections.push(`Related: ${recipe.related.join(', ')}`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
