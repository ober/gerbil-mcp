import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, stat } from 'node:fs/promises';

interface BuildProgress {
  phase: string;
  currentModule: string | null;
  compiledModules: string[];
  errors: string[];
  warnings: string[];
  elapsedMs: number | null;
  estimatedProgress: string;
}

/**
 * Parse gerbil build output to extract progress information.
 * Gerbil build output typically shows:
 *   ... compile <module> <timestamp>
 *   ... compile exe: <exe-name> <timestamp>
 *   ... copy <module> ...
 *   ... install <target> ...
 * Errors show as compiler diagnostics.
 */
function parseBuildOutput(output: string): BuildProgress {
  const lines = output.split('\n');
  const compiledModules: string[] = [];
  const errors: string[] = [];
  const warnings: string[] = [];
  let currentModule: string | null = null;
  let phase = 'unknown';

  for (const line of lines) {
    const trimmed = line.trim();

    // Compile phase
    const compileMatch = trimmed.match(
      /\.\.\.\s+compil(?:e|ing)\s+(?:exe:\s*)?(\S+)/i,
    );
    if (compileMatch) {
      currentModule = compileMatch[1];
      compiledModules.push(currentModule);
      phase = trimmed.includes('exe:') ? 'link' : 'compile';
      continue;
    }

    // Copy/install phase
    if (/\.\.\.\s+copy\s/i.test(trimmed)) {
      phase = 'install';
      continue;
    }
    if (/\.\.\.\s+install\s/i.test(trimmed)) {
      phase = 'install';
      continue;
    }

    // Clean phase
    if (/\.\.\.\s+clean\s/i.test(trimmed)) {
      phase = 'clean';
      continue;
    }

    // Error detection
    if (
      trimmed.includes('*** ERROR') ||
      trimmed.includes('Unbound identifier:') ||
      /:\d+:\d+:.*error/i.test(trimmed)
    ) {
      errors.push(trimmed);
      continue;
    }

    // Warning detection
    if (/warning/i.test(trimmed) && !trimmed.startsWith(';')) {
      warnings.push(trimmed);
    }
  }

  // Estimate progress
  let estimatedProgress = 'in progress';
  if (errors.length > 0) {
    estimatedProgress = `failed (${errors.length} error(s))`;
  } else if (phase === 'install') {
    estimatedProgress = 'installing';
  } else if (phase === 'link') {
    estimatedProgress = 'linking executables';
  } else if (compiledModules.length > 0) {
    estimatedProgress = `compiling (${compiledModules.length} module(s) done)`;
  }

  return {
    phase,
    currentModule,
    compiledModules,
    errors,
    warnings,
    elapsedMs: null,
    estimatedProgress,
  };
}

export function registerBuildProgressTool(server: McpServer): void {
  server.registerTool(
    'gerbil_build_progress',
    {
      title: 'Monitor Build Progress',
      description:
        'Parse gerbil build output to show compilation progress. ' +
        'Reads from a build output file (e.g., from a background build task) ' +
        'and extracts: current phase, modules compiled, errors/warnings, ' +
        'and estimated progress. Use with background build tasks to check ' +
        'status without waiting for completion.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        output_file: z
          .string()
          .optional()
          .describe(
            'Path to the build output file to parse (e.g., from a background task)',
          ),
        build_output: z
          .string()
          .optional()
          .describe(
            'Build output text to parse directly (alternative to output_file)',
          ),
      },
    },
    async ({ output_file, build_output }) => {
      if (!output_file && !build_output) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Either output_file or build_output is required.',
            },
          ],
          isError: true,
        };
      }

      let output: string;

      if (build_output) {
        output = build_output;
      } else {
        try {
          output = await readFile(output_file!, 'utf-8');
        } catch {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Cannot read build output file: ${output_file}`,
              },
            ],
            isError: true,
          };
        }
      }

      // Check if the file is still being written to (for background tasks)
      let isStillRunning = false;
      if (output_file) {
        try {
          const fileStat = await stat(output_file);
          const ageMs = Date.now() - fileStat.mtimeMs;
          isStillRunning = ageMs < 5000; // Modified in last 5 seconds
        } catch {
          // Can't stat
        }
      }

      const progress = parseBuildOutput(output);

      const sections: string[] = [];
      sections.push(`Build Progress: ${progress.estimatedProgress}`);
      if (isStillRunning) {
        sections.push('Status: still running');
      } else {
        sections.push('Status: completed or stalled');
      }
      sections.push(`Phase: ${progress.phase}`);
      sections.push('');

      if (progress.compiledModules.length > 0) {
        sections.push(
          `Modules compiled (${progress.compiledModules.length}):`,
        );
        // Show last 10 modules
        const recent = progress.compiledModules.slice(-10);
        for (const m of recent) {
          sections.push(`  ${m}`);
        }
        if (progress.compiledModules.length > 10) {
          sections.push(
            `  ... and ${progress.compiledModules.length - 10} more`,
          );
        }
        sections.push('');
      }

      if (progress.currentModule) {
        sections.push(`Last module: ${progress.currentModule}`);
        sections.push('');
      }

      if (progress.errors.length > 0) {
        sections.push(`Errors (${progress.errors.length}):`);
        for (const e of progress.errors.slice(0, 5)) {
          sections.push(`  ${e}`);
        }
        if (progress.errors.length > 5) {
          sections.push(
            `  ... and ${progress.errors.length - 5} more`,
          );
        }
        sections.push('');
      }

      if (progress.warnings.length > 0) {
        sections.push(`Warnings (${progress.warnings.length}):`);
        for (const w of progress.warnings.slice(0, 5)) {
          sections.push(`  ${w}`);
        }
        sections.push('');
      }

      // Summary line count
      const totalLines = output.split('\n').length;
      sections.push(`Output: ${totalLines} lines`);

      return {
        content: [
          { type: 'text' as const, text: sections.join('\n') },
        ],
        isError: progress.errors.length > 0,
      };
    },
  );
}
