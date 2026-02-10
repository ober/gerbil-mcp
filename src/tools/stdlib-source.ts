import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, access } from 'node:fs/promises';
import { constants } from 'node:fs';
import { join } from 'node:path';
import { runGxi, escapeSchemeString, RESULT_MARKER } from '../gxi.js';

export function registerStdlibSourceTool(server: McpServer): void {
  server.registerTool(
    'gerbil_stdlib_source',
    {
      title: 'Read Stdlib Module Source',
      description:
        'Read the full source code of a Gerbil standard library module. ' +
        'Resolves the module path (e.g. :std/iter, :std/text/json) to its source file ' +
        'in the Gerbil installation tree and returns the contents. Useful for understanding ' +
        'implementation details, internal helpers, and undocumented behavior.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        module_path: z
          .string()
          .describe('Module path (e.g. :std/iter, :std/text/json, :std/db/dbi)'),
        max_lines: z
          .number()
          .optional()
          .describe('Maximum lines to return (default: 500). Use 0 for unlimited.'),
      },
    },
    async ({ module_path, max_lines }) => {
      const limit = max_lines === 0 ? Infinity : (max_lines ?? 500);

      // Step 1: Resolve module path to library file
      const exprs = [
        '(import :gerbil/expander)',
        [
          `(let* ((mod (import-module (quote ${module_path}) #t #t))`,
          '       (path (module-context-path mod)))',
          `  (display "${RESULT_MARKER}")`,
          '  (display path))',
        ].join(' '),
      ];

      let libPath: string;
      try {
        const result = await runGxi(exprs, { timeout: 10000 });
        const output = result.stdout;
        const markerIdx = output.indexOf(RESULT_MARKER);
        if (markerIdx < 0) {
          return {
            content: [{
              type: 'text' as const,
              text: `Could not resolve module path for ${module_path}.\n${output}`,
            }],
            isError: true,
          };
        }
        libPath = output.slice(markerIdx + RESULT_MARKER.length).trim();
      } catch (e) {
        return {
          content: [{
            type: 'text' as const,
            text: `Failed to resolve module ${module_path}: ${e}`,
          }],
          isError: true,
        };
      }

      // Step 2: Convert lib path to source path (lib/ → src/)
      // Gerbil convention: /opt/gerbil/lib/std/iter.ssi → /opt/gerbil/src/std/iter.ss
      const sourceCandidates: string[] = [];

      // Try lib → src rewrite
      if (libPath.includes('/lib/')) {
        const srcPath = libPath.replace('/lib/', '/src/');
        // Try .ss extension
        sourceCandidates.push(srcPath.replace(/\.ss[io]?$/, '.ss'));
        sourceCandidates.push(srcPath + '.ss');
        sourceCandidates.push(srcPath);
      }

      // Also try direct module path resolution
      // e.g. :std/iter → $GERBIL_HOME/src/std/iter.ss
      const modRelative = module_path.replace(/^:/, '').replace(/\//g, '/');
      const gerbilHomeExprs = ['(display (path-expand "~~"))'];
      try {
        const homeResult = await runGxi(gerbilHomeExprs, { timeout: 5000 });
        const gerbilHome = homeResult.stdout.trim();
        if (gerbilHome) {
          sourceCandidates.push(join(gerbilHome, 'src', modRelative + '.ss'));
          sourceCandidates.push(join(gerbilHome, 'src', modRelative, 'base.ss'));
        }
      } catch {
        // ignore
      }

      // Step 3: Find readable source file
      let sourceFile: string | null = null;
      for (const candidate of sourceCandidates) {
        try {
          await access(candidate, constants.R_OK);
          sourceFile = candidate;
          break;
        } catch {
          continue;
        }
      }

      if (!sourceFile) {
        return {
          content: [{
            type: 'text' as const,
            text: `Module ${module_path} resolved to ${libPath} but source file not found.\n` +
              `Tried: ${sourceCandidates.join(', ')}\n\n` +
              'The source may not be installed. Check that the Gerbil source tree is available.',
          }],
          isError: true,
        };
      }

      // Step 4: Read and return source
      try {
        const content = await readFile(sourceFile, 'utf-8');
        const lines = content.split('\n');
        const truncated = lines.length > limit;
        const output = truncated
          ? lines.slice(0, limit).join('\n') + `\n\n... (truncated at ${limit} lines, total: ${lines.length})`
          : content;

        return {
          content: [{
            type: 'text' as const,
            text: `# Source: ${sourceFile}\n` +
              `# Module: ${module_path}\n` +
              `# Lines: ${lines.length}\n\n` +
              '```scheme\n' + output + '\n```',
          }],
        };
      } catch (err) {
        return {
          content: [{
            type: 'text' as const,
            text: `Failed to read source file ${sourceFile}: ${err}`,
          }],
          isError: true,
        };
      }
    },
  );
}
