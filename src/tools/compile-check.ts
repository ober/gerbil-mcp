import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { writeFile, unlink } from 'node:fs/promises';
import { join } from 'node:path';
import { randomUUID } from 'node:crypto';
import { tmpdir } from 'node:os';
import { runGxc } from '../gxi.js';

export function registerCompileCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_compile_check',
    {
      title: 'Compile Check Gerbil Code',
      description:
        'Run the Gerbil compiler (gxc -S) on code to catch compilation errors ' +
        'such as unbound identifiers and type issues that syntax checking alone misses. ' +
        'Validates the full Gerbil compilation pipeline without producing C output. ' +
        'Provide either code as a string or a file_path to an existing .ss file.',
      inputSchema: {
        code: z
          .string()
          .optional()
          .describe('Gerbil source code to compile-check'),
        file_path: z
          .string()
          .optional()
          .describe(
            'Path to a .ss/.scm file to compile-check (alternative to code)',
          ),
      },
    },
    async ({ code, file_path }) => {
      if (!code && !file_path) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Either "code" or "file_path" must be provided.',
            },
          ],
          isError: true,
        };
      }

      let targetPath = file_path || '';
      let tempFile = false;

      // Write code to temp file if provided inline
      if (code) {
        const tempName = `gerbil-check-${randomUUID().slice(0, 8)}.ss`;
        targetPath = join(tmpdir(), tempName);
        try {
          await writeFile(targetPath, code, 'utf-8');
          tempFile = true;
        } catch (err) {
          const msg =
            err instanceof Error
              ? err.message
              : 'Unknown error writing temp file';
          return {
            content: [
              {
                type: 'text' as const,
                text: `Failed to write temp file: ${msg}`,
              },
            ],
            isError: true,
          };
        }
      }

      try {
        const result = await runGxc(targetPath);

        if (result.timedOut) {
          return {
            content: [
              {
                type: 'text' as const,
                text: 'Compilation check timed out after 30 seconds.',
              },
            ],
            isError: true,
          };
        }

        if (result.exitCode === 127) {
          return {
            content: [
              {
                type: 'text' as const,
                text: 'gxc compiler not found. Ensure Gerbil is installed and gxc is in PATH.',
              },
            ],
            isError: true,
          };
        }

        if (result.exitCode !== 0) {
          // Parse and clean up error output
          let errorOutput = result.stderr.trim();
          // Replace temp file path with "<input>" for cleaner output
          if (tempFile && errorOutput) {
            errorOutput = errorOutput.replaceAll(targetPath, '<input>');
          }

          return {
            content: [
              {
                type: 'text' as const,
                text: `Compilation errors found:\n\n${errorOutput}`,
              },
            ],
            isError: true,
          };
        }

        // Success
        const target = tempFile ? 'Code' : targetPath;
        return {
          content: [
            {
              type: 'text' as const,
              text: `Compilation check passed. ${target} compiled successfully (gxc -S).`,
            },
          ],
        };
      } finally {
        // Clean up temp file
        if (tempFile) {
          try {
            await unlink(targetPath);
          } catch {
            // ignore cleanup errors
          }
        }
      }
    },
  );
}
