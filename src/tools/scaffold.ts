import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { access, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { randomUUID } from 'node:crypto';
import { runGxpkg } from '../gxi.js';

// Files that gxpkg new generates and would overwrite.
const CONFLICT_FILES = ['gerbil.pkg', 'Makefile', 'build.ss'];

async function fileExists(path: string): Promise<boolean> {
  try {
    await access(path);
    return true;
  } catch {
    return false;
  }
}

export function registerScaffoldTool(server: McpServer): void {
  server.registerTool(
    'gerbil_scaffold',
    {
      title: 'Scaffold Gerbil Project',
      description:
        'Create a new Gerbil project from a template using gxpkg new. ' +
        'Generates gerbil.pkg, build.ss, Makefile, and initial source files. ' +
        'If project_path is omitted, creates a temporary directory. ' +
        'Refuses to run if the target already contains gerbil.pkg, Makefile, or build.ss.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        project_path: z
          .string()
          .optional()
          .describe(
            'Directory to create the project in. If omitted, a temporary directory is created. ' +
            'If provided, the directory must already exist and not contain gerbil.pkg, Makefile, or build.ss.',
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
      // Resolve target directory: use provided path or create a temp dir.
      let targetDir: string;
      let createdTmp = false;
      if (project_path) {
        targetDir = project_path;
      } else {
        const dirName = name ?? `gerbil-project-${randomUUID().slice(0, 8)}`;
        targetDir = join(tmpdir(), dirName);
        await mkdir(targetDir, { recursive: true });
        createdTmp = true;
      }

      // Safety check: refuse to overwrite existing project files.
      const conflicts: string[] = [];
      for (const file of CONFLICT_FILES) {
        if (await fileExists(join(targetDir, file))) {
          conflicts.push(file);
        }
      }
      if (conflicts.length > 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text:
                `Refusing to scaffold: the target directory already contains ${conflicts.join(', ')}. ` +
                `Running gxpkg new here would overwrite existing files.\n\n` +
                `Target: ${targetDir}\n\n` +
                `Use an empty directory or omit project_path to create a temporary directory.`,
            },
          ],
          isError: true,
        };
      }

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
        cwd: targetDir,
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
        `Project scaffolded in ${targetDir}.`,
      ];
      if (createdTmp) {
        sections.push(`(created temporary directory)`);
      }
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
