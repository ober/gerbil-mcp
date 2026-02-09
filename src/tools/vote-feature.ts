import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFileSync, writeFileSync } from 'node:fs';
import { z } from 'zod';
import { FEATURES_PATH } from './suggest-feature.js';
import type { FeatureSuggestion } from './suggest-feature.js';

export function registerVoteFeatureTool(server: McpServer): void {
  server.registerTool(
    'gerbil_vote_feature',
    {
      title: 'Vote for Feature',
      description:
        'Increment the vote count for an existing feature suggestion. ' +
        'Use this when you encounter a situation where a suggested feature would have saved time or tokens. ' +
        'By default reads/writes the gerbil-mcp repo features.json. ' +
        'Optionally specify features_path for a different file.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: true,
      },
      inputSchema: {
        features_path: z
          .string()
          .optional()
          .describe(
            'Absolute path to a JSON features file. If omitted, uses the gerbil-mcp repo features.json.',
          ),
        id: z
          .string()
          .describe('The feature ID to vote for (e.g. "batch-module-check")'),
      },
    },
    async ({ features_path: explicitPath, id }) => {
      const features_path = explicitPath || FEATURES_PATH;

      // Read existing file
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
          return {
            content: [
              {
                type: 'text' as const,
                text: `Error: ${features_path} not found. No features to vote on.`,
              },
            ],
            isError: true,
          };
        }
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

      // Find the feature
      const idx = features.findIndex((f) => f.id === id);
      if (idx < 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Feature "${id}" not found. Use gerbil_list_features to see available features.`,
            },
          ],
          isError: true,
        };
      }

      // Increment votes
      features[idx].votes = (features[idx].votes ?? 0) + 1;
      const newCount = features[idx].votes;

      // Write back
      try {
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

      return {
        content: [
          {
            type: 'text' as const,
            text: `Voted for "${id}" — now has ${newCount} vote(s). Feature: ${features[idx].title}`,
          },
        ],
      };
    },
  );
}
