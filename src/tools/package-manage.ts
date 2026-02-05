import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { access } from 'node:fs/promises';
import { join } from 'node:path';
import { homedir } from 'node:os';
import { runGxpkg } from '../gxi.js';

export function registerPackageManageTool(server: McpServer): void {
  server.registerTool(
    'gerbil_package_manage',
    {
      title: 'Manage Gerbil Packages',
      description:
        'Install, update, or uninstall Gerbil packages. ' +
        'Package names can include @tag for version pinning (e.g. "github.com/user/repo@v1.0"). ' +
        'Use "all" with the update action to update all installed packages. ' +
        'Safety: when the server is running in a project directory (with gerbil.pkg), ' +
        'the tool automatically defaults to HOME as the working directory to prevent ' +
        'gxpkg from operating on the local package context. Use the cwd parameter ' +
        'to override, or global_env to force global environment.',
      inputSchema: {
        action: z
          .enum(['install', 'update', 'uninstall'])
          .describe('Package management action to perform'),
        package: z
          .string()
          .describe(
            'Package name, optionally with @tag for version. Use "all" with update to update everything.',
          ),
        global_env: z
          .boolean()
          .optional()
          .describe(
            'Use the global environment even when in a local package context',
          ),
        force: z
          .boolean()
          .optional()
          .describe('Force the action (only applies to uninstall)'),
        cwd: z
          .string()
          .optional()
          .describe('Working directory for local package context'),
      },
    },
    async ({ action, package: pkg, global_env, force, cwd }) => {
      // Safety: if no explicit cwd is provided, check if the server's working
      // directory contains a gerbil.pkg. If so, default to HOME to prevent
      // gxpkg from operating on the local package context (which can destroy
      // project files during clean/uninstall operations).
      let effectiveCwd = cwd;
      if (!effectiveCwd && !global_env) {
        try {
          await access(join(process.cwd(), 'gerbil.pkg'));
          // We're in a project directory — use HOME as safe default
          effectiveCwd = homedir();
        } catch {
          // No gerbil.pkg — safe to use default cwd
        }
      }

      // Validate: "all" only works with update
      if (pkg === 'all' && action !== 'update') {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'The "all" target is only supported with the "update" action.',
            },
          ],
          isError: true,
        };
      }

      // Validate: force only works with uninstall
      if (force && action !== 'uninstall') {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'The "force" option only applies to the "uninstall" action.',
            },
          ],
          isError: true,
        };
      }

      const args: string[] = [action];
      if (global_env) args.push('--global-env');
      if (force) args.push('--force');
      args.push(pkg);

      const result = await runGxpkg(args, {
        cwd: effectiveCwd,
        timeout: 120_000,
      });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Package ${action} timed out after 120 seconds.`,
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
              text: `Package ${action} failed (exit code ${result.exitCode}):\n\n${output}`,
            },
          ],
          isError: true,
        };
      }

      const actionPast =
        action === 'install'
          ? 'installed'
          : action === 'update'
            ? 'updated'
            : 'uninstalled';

      const sections: string[] = [
        `Package ${pkg} ${actionPast} successfully.`,
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
