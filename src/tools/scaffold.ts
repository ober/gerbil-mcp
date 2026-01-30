import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxpkg } from '../gxi.js';

export function registerScaffoldTool(server: McpServer): void {
  server.registerTool(
    'gerbil_scaffold',
    {
      title: 'Scaffold Gerbil Project',
      description:
        'Create a new Gerbil project from a template using gxpkg new. ' +
        'Generates gerbil.pkg, build.ss, and initial source files. ' +
        'The target directory must already exist.',
      inputSchema: {
        project_path: z
          .string()
          .describe(
            'Directory to create the project in. The directory should already exist.',
          ),
        package: z
          .string()
          .optional()
          .describe(
            'Package prefix for the project (default: current username)',
          ),
        name: z
          .string()
          .optional()
          .describe('Package name (default: directory name)'),
        link: z
          .string()
          .optional()
          .describe(
            'Public package link (e.g. "github.com/user/project")',
          ),
      },
    },
    async ({ project_path, package: pkg, name, link }) => {
      const args: string[] = ['new'];
      if (pkg) {
        args.push('--package', pkg);
      }
      if (name) {
        args.push('--name', name);
      }
      if (link) {
        args.push('--link', link);
      }

      const result = await runGxpkg(args, {
        cwd: project_path,
        timeout: 30_000,
      });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Scaffold timed out after 30 seconds.',
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
              text: `Scaffold failed (exit code ${result.exitCode}):\n\n${output}`,
            },
          ],
          isError: true,
        };
      }

      const sections: string[] = [
        `Project scaffolded in ${project_path}.`,
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
