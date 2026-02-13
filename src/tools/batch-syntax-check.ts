import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER, VALID_MARKER } from '../gxi.js';

interface SnippetResult {
  id: string;
  valid: boolean;
  error?: string;
}

export function registerBatchSyntaxCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_batch_syntax_check',
    {
      title: 'Batch Syntax Check',
      description:
        'Check multiple Gerbil code snippets for syntax validity in a single call. ' +
        'Returns per-snippet pass/fail results. Much more efficient than making individual ' +
        'gerbil_check_syntax calls when verifying many functions or definitions at once.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        snippets: z
          .array(z.object({
            id: z.string().describe('Identifier for this snippet (e.g. function name)'),
            code: z.string().describe('Gerbil code to check'),
          }))
          .describe('Array of code snippets to check'),
      },
    },
    async ({ snippets }) => {
      if (!snippets || snippets.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: 'No snippets provided.',
          }],
          isError: true,
        };
      }

      const results: SnippetResult[] = [];

      // Process snippets in batches to avoid overwhelming gxi
      // Each snippet gets its own gxi call for isolation
      const CONCURRENCY = 5;
      for (let i = 0; i < snippets.length; i += CONCURRENCY) {
        const batch = snippets.slice(i, i + CONCURRENCY);
        const promises = batch.map(async (snippet) => {
          const escaped = escapeSchemeString(snippet.code);
          const exprs = [
            '(import :gerbil/expander)',
            [
              '(with-catch',
              '  (lambda (e)',
              `    (display "${ERROR_MARKER}")`,
              '    (display-exception e (current-output-port)))',
              '  (lambda ()',
              `    (read (open-input-string "${escaped}"))`,
              `    (displayln "${VALID_MARKER}")))`,
            ].join(' '),
          ];
          try {
            const result = await runGxi(exprs, { timeout: 10000 });
            const stdout = result.stdout;
            if (stdout.includes(ERROR_MARKER)) {
              const errorMsg = stdout.slice(stdout.indexOf(ERROR_MARKER) + ERROR_MARKER.length).trim();
              return {
                id: snippet.id,
                valid: false,
                error: errorMsg.split('\n')[0],
              };
            }
            return { id: snippet.id, valid: true };
          } catch (err) {
            return {
              id: snippet.id,
              valid: false,
              error: err instanceof Error ? err.message : 'Check failed',
            };
          }
        });
        const batchResults = await Promise.all(promises);
        results.push(...batchResults);
      }

      // Format output
      const passed = results.filter(r => r.valid).length;
      const failed = results.filter(r => !r.valid).length;

      const sections: string[] = [
        `Checked ${results.length} snippet(s): ${passed} passed, ${failed} failed`,
        '',
      ];

      for (const r of results) {
        if (r.valid) {
          sections.push(`  [PASS] ${r.id}`);
        } else {
          sections.push(`  [FAIL] ${r.id}: ${r.error}`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: failed > 0,
      };
    },
  );
}
