import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { dirname } from 'node:path';
import { z } from 'zod';
import type { Recipe } from './howto.js';
import { REPO_COOKBOOK_PATH } from './howto.js';

export function registerHowtoAddTool(server: McpServer): void {
  server.registerTool(
    'gerbil_howto_add',
    {
      title: 'Add Gerbil Cookbook Recipe',
      description:
        'Append a new Gerbil Scheme recipe to the gerbil-mcp cookbook. ' +
        'If a recipe with the same id already exists, it is replaced (update semantics). ' +
        'By default writes to the gerbil-mcp repo cookbook. ' +
        'Optionally specify cookbook_path to write to a different file.',
      inputSchema: {
        cookbook_path: z
          .string()
          .optional()
          .describe(
            'Absolute path to a JSON cookbook file. If omitted, writes to the gerbil-mcp repo cookbook.',
          ),
        id: z
          .string()
          .describe('Unique recipe identifier in kebab-case (e.g. "read-csv-file")'),
        title: z.string().describe('Human-readable title (e.g. "Read a CSV file")'),
        tags: z
          .array(z.string())
          .describe('Search keywords (e.g. ["csv", "file", "read", "parse"])'),
        imports: z
          .array(z.string())
          .describe('Gerbil module imports (e.g. [":std/text/csv"]). Use [] if none needed.'),
        code: z.string().describe('Code example'),
        notes: z.string().optional().describe('Usage notes'),
        related: z
          .array(z.string())
          .optional()
          .describe('Related recipe IDs'),
        supersedes: z
          .string()
          .optional()
          .describe(
            'If provided, marks the recipe with this ID as deprecated and sets its superseded_by to the new recipe ID.',
          ),
      },
    },
    async ({ cookbook_path: explicitPath, id, title, tags, imports, code, notes, related, supersedes }) => {
      const cookbook_path = explicitPath || REPO_COOKBOOK_PATH;
      // Read existing file or start fresh
      let recipes: Recipe[] = [];
      try {
        const raw = readFileSync(cookbook_path, 'utf-8');
        const parsed = JSON.parse(raw);
        if (!Array.isArray(parsed)) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Error: ${cookbook_path} does not contain a JSON array.`,
              },
            ],
            isError: true,
          };
        }
        recipes = parsed;
      } catch (e: unknown) {
        if ((e as NodeJS.ErrnoException).code === 'ENOENT') {
          // File doesn't exist yet — start with empty array
          recipes = [];
        } else {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Error reading ${cookbook_path}: ${e instanceof Error ? e.message : String(e)}`,
              },
            ],
            isError: true,
          };
        }
      }

      // Build the new recipe
      const recipe: Recipe = { id, title, tags, imports, code };
      if (notes) recipe.notes = notes;
      if (related && related.length > 0) recipe.related = related;

      // Replace existing recipe with same id, or append
      const existingIdx = recipes.findIndex((r) => r.id === id);
      if (existingIdx >= 0) {
        recipes[existingIdx] = recipe;
      } else {
        recipes.push(recipe);
      }

      // Mark superseded recipe as deprecated
      if (supersedes) {
        const supersededIdx = recipes.findIndex((r) => r.id === supersedes);
        if (supersededIdx >= 0) {
          recipes[supersededIdx] = {
            ...recipes[supersededIdx],
            deprecated: true,
            superseded_by: id,
          };
        }
      }

      // Write back
      try {
        mkdirSync(dirname(cookbook_path), { recursive: true });
        writeFileSync(cookbook_path, JSON.stringify(recipes, null, 2) + '\n');
      } catch (e: unknown) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Error writing ${cookbook_path}: ${e instanceof Error ? e.message : String(e)}`,
            },
          ],
          isError: true,
        };
      }

      const action = existingIdx >= 0 ? 'Updated' : 'Added';
      return {
        content: [
          {
            type: 'text' as const,
            text: `${action} recipe "${id}" in ${cookbook_path} (${recipes.length} total recipes).`,
          },
        ],
      };
    },
  );
}
