import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxpkg } from '../gxi.js';

export function registerBuildProjectTool(server: McpServer): void {
  server.registerTool(
    'gerbil_build_project',
    {
      title: 'Build Gerbil Project',
      description:
        'Build or clean a Gerbil project directory using gxpkg. ' +
        'The project directory should contain a gerbil.pkg file. ' +
        'Supports release, optimized, and debug build modes.',
      inputSchema: {
        project_path: z
          .string()
          .describe(
            'Directory containing the Gerbil project (with gerbil.pkg)',
          ),
        action: z
          .enum(['build', 'clean'])
          .optional()
          .describe('Action to perform (default: "build")'),
        release: z
          .boolean()
          .optional()
          .describe('Build released (static) executables'),
        optimized: z
          .boolean()
          .optional()
          .describe('Build full program optimized executables'),
        debug: z
          .boolean()
          .optional()
          .describe('Build with debug symbols'),
      },
    },
    async ({ project_path, action, release, optimized, debug }) => {
      const act = action ?? 'build';

      const args: string[] = [act];
      if (act === 'build') {
        if (release) args.push('--release');
        if (optimized) args.push('--optimized');
        if (debug) args.push('--debug');
      }

      const result = await runGxpkg(args, {
        cwd: project_path,
        timeout: 120_000,
      });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Build timed out after 120 seconds.',
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
              text: 'gxpkg not found. Ensure Gerbil is installed and gxpkg is in PATH.',
            },
          ],
          isError: true,
        };
      }

      const output = [result.stdout, result.stderr]
        .filter(Boolean)
        .join('\n')
        .trim();

      if (result.exitCode !== 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Build failed (exit code ${result.exitCode}):\n\n${output}`,
            },
          ],
          isError: true,
        };
      }

      const flags = [
        release && 'release',
        optimized && 'optimized',
        debug && 'debug',
      ].filter(Boolean);

      const flagStr = flags.length > 0 ? ` (${flags.join(', ')})` : '';
      const sections: string[] = [
        `${act === 'build' ? 'Build' : 'Clean'} succeeded${flagStr}.`,
      ];
      if (output) {
        sections.push('');
        sections.push(output);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
