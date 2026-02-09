import { McpServer, ResourceTemplate } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

interface Recipe {
  id: string;
  title: string;
  tags: string[];
  imports: string[];
  code: string;
  notes?: string;
  related?: string[];
  deprecated?: boolean;
  superseded_by?: string;
  gerbil_version?: string;
  valid_for?: string[];
}

function loadCookbook(): Recipe[] {
  try {
    const thisDir = dirname(fileURLToPath(import.meta.url));
    const cookbookPath = join(thisDir, '..', 'cookbooks.json');
    const raw = readFileSync(cookbookPath, 'utf-8');
    return JSON.parse(raw) as Recipe[];
  } catch {
    return [];
  }
}

export function registerResources(server: McpServer): void {
  const recipes = loadCookbook();

  // Static resource: full cookbook index
  server.registerResource(
    'cookbook-index',
    'gerbil://cookbooks',
    {
      description: 'Index of all Gerbil cookbook recipes with id, title, and tags',
      mimeType: 'application/json',
    },
    async () => {
      const index = recipes.map(r => ({
        id: r.id,
        title: r.title,
        tags: r.tags,
        ...(r.deprecated ? { deprecated: true } : {}),
        ...(r.gerbil_version ? { gerbil_version: r.gerbil_version } : {}),
      }));
      return {
        contents: [{
          uri: 'gerbil://cookbooks',
          mimeType: 'application/json',
          text: JSON.stringify(index, null, 2),
        }],
      };
    },
  );

  // Dynamic resource: individual recipe by id
  server.registerResource(
    'cookbook-recipe',
    new ResourceTemplate('gerbil://cookbooks/{id}', { list: undefined }),
    {
      description: 'A single Gerbil cookbook recipe with full code, imports, and notes',
      mimeType: 'application/json',
    },
    async (uri, variables) => {
      const id = variables.id as string;
      const recipe = recipes.find(r => r.id === id);
      if (!recipe) {
        return {
          contents: [{
            uri: uri.href,
            mimeType: 'text/plain',
            text: `Recipe not found: ${id}`,
          }],
        };
      }
      return {
        contents: [{
          uri: uri.href,
          mimeType: 'application/json',
          text: JSON.stringify(recipe, null, 2),
        }],
      };
    },
  );
}
