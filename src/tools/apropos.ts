import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString } from '../gxi.js';

export function registerAproposTool(server: McpServer): void {
  server.registerTool(
    'gerbil_apropos',
    {
      title: 'Search Gerbil Symbols',
      description:
        'Search for Gerbil/Gambit symbols matching a pattern string. ' +
        'Uses Gambit ##apropos to find symbols across all namespaces. ' +
        'Example: pattern "hash-get" returns hash-get and related symbols.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        pattern: z.string().describe('Search pattern (substring match against symbol names)'),
      },
    },
    async ({ pattern }) => {
      const escaped = escapeSchemeString(pattern);
      const expr = `(##apropos "${escaped}")`;

      const result = await runGxi([expr]);

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Symbol search timed out.' }],
          isError: true,
        };
      }

      if (result.exitCode !== 0) {
        const errMsg = result.stderr.trim() || result.stdout.trim();
        return {
          content: [{ type: 'text' as const, text: `Error:\n${errMsg}` }],
          isError: true,
        };
      }

      const output = result.stdout.trim();
      if (!output) {
        return {
          content: [{ type: 'text' as const, text: `No symbols found matching "${pattern}".` }],
        };
      }

      return {
        content: [{ type: 'text' as const, text: `Symbols matching "${pattern}":\n\n${output}` }],
      };
    },
  );
}
