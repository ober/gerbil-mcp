import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { writeFile, unlink } from 'node:fs/promises';
import { join } from 'node:path';
import { randomUUID } from 'node:crypto';
import { tmpdir } from 'node:os';
import { runGxc, buildLoadpathEnv } from '../gxi.js';

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
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
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
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution',
          ),
      },
    },
    async ({ code, file_path, loadpath }) => {
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
        const env = loadpath && loadpath.length > 0 ? buildLoadpathEnv(loadpath) : undefined;
        const result = await runGxc(targetPath, { env });

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
          // Combine stdout and stderr for error output — gxc may write
          // errors to either stream depending on the error type
          let errorOutput = result.stderr.trim();
          const stdoutOutput = result.stdout.trim();
          if (!errorOutput && stdoutOutput) {
            errorOutput = stdoutOutput;
          } else if (errorOutput && stdoutOutput) {
            errorOutput = errorOutput + '\n' + stdoutOutput;
          }

          // Replace temp file path with "<input>" for cleaner output
          if (tempFile && errorOutput) {
            errorOutput = errorOutput.replaceAll(targetPath, '<input>');
          }

          if (!errorOutput) {
            return {
              content: [
                {
                  type: 'text' as const,
                  text: `Compilation failed with exit code ${result.exitCode} (no error details available). Try gerbil_diagnostics for more info.`,
                },
              ],
              isError: true,
            };
          }

          const enhanced = enhanceGxcError(errorOutput);
          return {
            content: [
              {
                type: 'text' as const,
                text: `Compilation errors found:\n\n${enhanced}`,
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

function enhanceGxcError(errorOutput: string): string {
  const sections: string[] = [errorOutput];

  // Detect known compiler-internal crash patterns
  const internalPatterns = [
    {
      pattern: /stx-car-e/,
      hint: 'The compiler tried to destructure a non-pair syntax object (stx-car-e). This usually means a macro or form produced unexpected syntax. Try simplifying the expression — e.g., replace (or ...) with (let ((v ...)) (if v v ...)).',
    },
    {
      pattern: /gx-core-expand/,
      hint: 'The compiler crashed during core expansion. The code may use a macro form incorrectly.',
    },
    {
      pattern: /code generation/i,
      hint: 'The compiler crashed during code generation. This may be a gxc bug or an unsupported code pattern.',
    },
    {
      pattern: /Segmentation fault|SIGSEGV/,
      hint: 'The compiler segfaulted. This is almost certainly a gxc bug.',
    },
    {
      pattern: /##raise-heap-overflow/,
      hint: 'The compiler ran out of heap space. The code may have deeply nested or recursive macro expansions.',
    },
  ];

  for (const { pattern, hint } of internalPatterns) {
    if (pattern.test(errorOutput)) {
      sections.push('');
      sections.push(`Hint: ${hint}`);
      break;
    }
  }

  // Try to extract source location from expansion context
  const contextMatch = errorOutput.match(
    /--- expansion context ---[\s\S]*?at:\s+(.+)/,
  );
  if (contextMatch) {
    sections.push('');
    sections.push(`Expansion context location: ${contextMatch[1]}`);
  }

  // Extract the first "at:" location if present (but not if already found above)
  if (!contextMatch) {
    const atMatch = errorOutput.match(/at:\s+([^\n]+)/);
    if (atMatch) {
      sections.push('');
      sections.push(`Error location: ${atMatch[1]}`);
    }
  }

  return sections.join('\n');
}
