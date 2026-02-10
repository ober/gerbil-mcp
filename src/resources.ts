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

  // Static reference documentation resources
  const thisDir = dirname(fileURLToPath(import.meta.url));
  const resourcesDir = join(thisDir, 'resources');

  const REFERENCE_RESOURCES: Array<{
    name: string;
    uri: string;
    description: string;
    filename: string;
  }> = [
    {
      name: 'Gerbil Idiom Cheat Sheet',
      uri: 'gerbil://reference/idioms',
      description:
        'Core syntax differences from standard Scheme: def vs define, bracket lists, ' +
        'hash tables, keyword args, defstruct/defclass, using, chain, iteration, ' +
        'pattern matching, import/export, error handling, and common gotchas.',
      filename: 'gerbil-idioms.md',
    },
    {
      name: 'Gerbil Pattern Matching Reference',
      uri: 'gerbil://reference/pattern-matching',
      description:
        'Complete guide to match syntax: literals, binding, wildcards, list/cons ' +
        'destructuring, struct patterns, predicates (?), and/or/not patterns, ' +
        'quasiquote, ellipsis, match lambda (<>), match*, when-let, if-let.',
      filename: 'gerbil-pattern-matching.md',
    },
    {
      name: 'Gerbil Actor System Reference',
      uri: 'gerbil://reference/actors',
      description:
        'Actor-oriented programming: spawn, message passing (<-, -->, ->>, !!), ' +
        'stateful loops, request/reply, supervisor patterns, worker pools, ' +
        'actor servers, ensembles, remote handles, and Gambit threading primitives.',
      filename: 'gerbil-actors.md',
    },
    {
      name: 'Gerbil Standard Library Map',
      uri: 'gerbil://reference/stdlib-map',
      description:
        'Complete standard library overview organized by domain: text processing, ' +
        'networking, concurrency/actors, data structures, iteration, I/O, OS/system, ' +
        'database, crypto, syntax/macros, logging, testing, serialization, FFI, SRFIs.',
      filename: 'gerbil-stdlib-map.md',
    },
    {
      name: 'Gerbil-Gambit Interop Guide',
      uri: 'gerbil://reference/gambit-interop',
      description:
        'When and how to use Gambit primitives from Gerbil: ## prefix, threading ' +
        'model comparison, I/O layers, C FFI (begin-foreign, c-lambda, extern), ' +
        'declare blocks for optimization, reader extensions, and common mistakes.',
      filename: 'gerbil-gambit-interop.md',
    },
  ];

  for (const res of REFERENCE_RESOURCES) {
    const filePath = join(resourcesDir, res.filename);

    server.registerResource(
      res.name,
      res.uri,
      {
        description: res.description,
        mimeType: 'text/markdown',
      },
      async () => {
        let content: string;
        try {
          content = readFileSync(filePath, 'utf-8');
        } catch (err) {
          content = `Error reading resource file ${res.filename}: ${err}`;
        }
        return {
          contents: [{
            uri: res.uri,
            mimeType: 'text/markdown',
            text: content,
          }],
        };
      },
    );
  }
}
