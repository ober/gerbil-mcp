import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { z } from 'zod';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/** Path to the repo-local features file. */
export const FEATURES_PATH = resolve(__dirname, '..', '..', 'features.json');

export interface FeatureSuggestion {
  id: string;
  title: string;
  description: string;
  impact: 'high' | 'medium' | 'low';
  tags: string[];
  use_case: string;
  example_scenario: string;
  estimated_token_reduction: string;
  votes: number;
}

export function registerSuggestFeatureTool(server: McpServer): void {
  server.registerTool(
    'gerbil_suggest_feature',
    {
      title: 'Suggest Feature',
      description:
        'Write a feature suggestion to the features file. ' +
        'If a suggestion with the same id already exists, it is replaced (update semantics). ' +
        'By default writes to the gerbil-mcp repo features.json. ' +
        'Optionally specify features_path to write to a different file.',
      inputSchema: {
        features_path: z
          .string()
          .optional()
          .describe(
            'Absolute path to a JSON features file. If omitted, writes to the gerbil-mcp repo features.json.',
          ),
        id: z
          .string()
          .describe('Unique feature identifier in kebab-case (e.g. "batch-module-check")'),
        title: z.string().describe('Short description of the feature'),
        description: z.string().describe('Detailed explanation of what the feature does'),
        impact: z
          .enum(['high', 'medium', 'low'])
          .describe('Estimated impact on token/time savings'),
        tags: z
          .array(z.string())
          .describe('Search keywords (e.g. ["module", "check", "batch"])'),
        use_case: z.string().describe('When this feature would be useful'),
        example_scenario: z.string().describe('Concrete example of the problem this solves'),
        estimated_token_reduction: z
          .string()
          .describe('Estimated token savings (e.g. "~500 tokens per invocation")'),
      },
    },
    async ({
      features_path: explicitPath,
      id,
      title,
      description,
      impact,
      tags,
      use_case,
      example_scenario,
      estimated_token_reduction,
    }) => {
      const features_path = explicitPath || FEATURES_PATH;

      // Read existing file or start fresh
      let features: FeatureSuggestion[] = [];
      try {
        const raw = readFileSync(features_path, 'utf-8');
        const parsed = JSON.parse(raw);
        if (!Array.isArray(parsed)) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Error: ${features_path} does not contain a JSON array.`,
              },
            ],
            isError: true,
          };
        }
        features = parsed;
      } catch (e: unknown) {
        if ((e as NodeJS.ErrnoException).code === 'ENOENT') {
          features = [];
        } else {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Error reading ${features_path}: ${e instanceof Error ? e.message : String(e)}`,
              },
            ],
            isError: true,
          };
        }
      }

      // Replace existing suggestion with same id, or append
      const existingIdx = features.findIndex((f) => f.id === id);
      const existingVotes = existingIdx >= 0 ? (features[existingIdx].votes ?? 0) : 0;

      const suggestion: FeatureSuggestion = {
        id,
        title,
        description,
        impact,
        tags,
        use_case,
        example_scenario,
        estimated_token_reduction,
        votes: existingVotes,
      };

      if (existingIdx >= 0) {
        features[existingIdx] = suggestion;
      } else {
        features.push(suggestion);
      }

      // Write back
      try {
        mkdirSync(dirname(features_path), { recursive: true });
        writeFileSync(features_path, JSON.stringify(features, null, 2) + '\n');
      } catch (e: unknown) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Error writing ${features_path}: ${e instanceof Error ? e.message : String(e)}`,
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
            text: `${action} feature suggestion "${id}" in ${features_path} (${features.length} total suggestions).`,
          },
        ],
      };
    },
  );
}
