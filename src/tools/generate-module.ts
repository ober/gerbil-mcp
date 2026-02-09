import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { isSchemeDelimiter } from './parse-utils.js';

/**
 * Replace all occurrences of `from` with `to` in `text`,
 * respecting Scheme word boundaries to avoid partial replacements.
 */
function schemeReplace(text: string, from: string, to: string): string {
  let result = '';
  let pos = 0;

  while (pos < text.length) {
    const idx = text.indexOf(from, pos);
    if (idx === -1) {
      result += text.slice(pos);
      break;
    }

    // Check word boundaries
    const leftOk = idx === 0 || isSchemeDelimiter(text[idx - 1]);
    const rightIdx = idx + from.length;
    const rightOk =
      rightIdx >= text.length || isSchemeDelimiter(text[rightIdx]);

    if (leftOk && rightOk) {
      // Check we're not inside a string literal (heuristic: count quotes before)
      const before = text.slice(0, idx);
      const quoteCount = (before.match(/"/g) || []).length;
      const inString = quoteCount % 2 !== 0;

      // Check we're not inside a comment
      const lastNewline = before.lastIndexOf('\n');
      const lineStart = lastNewline === -1 ? 0 : lastNewline + 1;
      const linePrefix = text.slice(lineStart, idx);
      const inComment = linePrefix.includes(';');

      if (!inString && !inComment) {
        result += text.slice(pos, idx) + to;
        pos = rightIdx;
        continue;
      }
    }

    // No match at word boundary — advance past this occurrence
    result += text.slice(pos, idx + 1);
    pos = idx + 1;
  }

  return result;
}

export function registerGenerateModuleTool(server: McpServer): void {
  server.registerTool(
    'gerbil_generate_module',
    {
      title: 'Template-Based Code Generation',
      description:
        'Generate a Gerbil module by reading a template file and applying ' +
        'word-boundary-aware string substitutions. Returns the generated text ' +
        '(does NOT write to disk). Useful for creating mechanical variations ' +
        'of an existing module pattern.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        template_path: z
          .string()
          .describe('Path to the template .ss file to use as a base'),
        substitutions: z
          .array(
            z.object({
              from: z.string().describe('Text to find'),
              to: z.string().describe('Replacement text'),
            }),
          )
          .describe('Array of {from, to} substitution pairs'),
      },
    },
    async ({ template_path, substitutions }) => {
      let content: string;
      try {
        content = await readFile(template_path, 'utf-8');
      } catch (err) {
        const msg =
          err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            { type: 'text' as const, text: `Failed to read template: ${msg}` },
          ],
          isError: true,
        };
      }

      if (substitutions.length === 0) {
        return {
          content: [{ type: 'text' as const, text: content }],
        };
      }

      let result = content;
      const applied: string[] = [];

      for (const sub of substitutions) {
        const before = result;
        result = schemeReplace(result, sub.from, sub.to);
        const count =
          (before.split(sub.from).length - 1) -
          (result.split(sub.from).length - 1);
        if (count > 0) {
          applied.push(`  "${sub.from}" -> "${sub.to}" (${count} replacement${count === 1 ? '' : 's'})`);
        } else {
          applied.push(`  "${sub.from}" -> "${sub.to}" (0 replacements — not found at word boundaries)`);
        }
      }

      const header = [
        `;; Generated from: ${template_path}`,
        `;; Substitutions applied:`,
        ...applied.map((a) => `;; ${a}`),
        '',
      ].join('\n');

      return {
        content: [{ type: 'text' as const, text: header + result }],
      };
    },
  );
}
