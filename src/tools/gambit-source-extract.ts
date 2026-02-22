import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { execFile } from 'node:child_process';
import { access, stat } from 'node:fs/promises';
import { constants } from 'node:fs';
import { findGxi } from '../gxi.js';
import { dirname, join } from 'node:path';

/**
 * Find the gsc binary by deriving from gxi path.
 */
async function findGsc(): Promise<string> {
  // Try deriving from gxi path
  try {
    const gxiPath = await findGxi();
    if (gxiPath !== 'gxi') {
      const gscPath = join(dirname(gxiPath), 'gsc');
      await access(gscPath, constants.X_OK);
      return gscPath;
    }
  } catch {
    // fall through
  }

  // Try common locations
  const candidates = ['/opt/gerbil/bin/gsc', 'gsc'];
  for (const candidate of candidates) {
    try {
      await access(candidate, constants.X_OK);
      return candidate;
    } catch {
      // try next
    }
  }

  return 'gsc';
}

/**
 * Run a command and return stdout/stderr.
 */
function runCmd(
  cmd: string,
  args: string[],
  options?: { cwd?: string; timeout?: number },
): Promise<{ stdout: string; stderr: string; exitCode: number }> {
  return new Promise((resolve) => {
    execFile(
      cmd,
      args,
      {
        timeout: options?.timeout ?? 30000,
        maxBuffer: 1024 * 1024,
        cwd: options?.cwd,
      },
      (error, stdout, stderr) => {
        if (error) {
          const exitCode =
            typeof error.code === 'number'
              ? error.code
              : (error as NodeJS.ErrnoException).code === 'ENOENT'
                ? 127
                : 1;
          resolve({ stdout: stdout ?? '', stderr: stderr ?? '', exitCode });
        } else {
          resolve({ stdout: stdout ?? '', stderr: stderr ?? '', exitCode: 0 });
        }
      },
    );
  });
}

export function registerGambitSourceExtractTool(server: McpServer): void {
  server.registerTool(
    'gerbil_gambit_source_extract',
    {
      title: 'Extract Gambit Source for Current Version',
      description:
        'Identify the exact Gambit commit that Gerbil was built with (from gsc -v output) ' +
        'and optionally extract the matching source tree from a Gambit git repository. ' +
        'Reports version string, commit hash, build date, and architecture. ' +
        'When gambit_repo_path is provided, extracts source files matching the exact commit. ' +
        'Critical for embedding gambitgsc modules where source/binary version must match.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: true,
      },
      inputSchema: {
        gambit_repo_path: z
          .string()
          .optional()
          .describe(
            'Path to a local Gambit git repository. If provided, extracts source files ' +
            'matching the detected Gambit version commit hash.',
          ),
        output_dir: z
          .string()
          .optional()
          .describe(
            'Directory to extract source files into (required when gambit_repo_path is provided)',
          ),
        subdirs: z
          .array(z.string())
          .optional()
          .describe(
            'Subdirectories to extract from the Gambit repo (default: ["gsc", "lib"])',
          ),
      },
    },
    async ({ gambit_repo_path, output_dir, subdirs }) => {
      // Step 1: Find and run gsc -v
      const gscPath = await findGsc();
      const versionResult = await runCmd(gscPath, ['-v']);

      // gsc -v outputs to stderr
      const versionOutput = (versionResult.stderr || versionResult.stdout).trim();

      if (versionResult.exitCode === 127) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Cannot find gsc binary. Ensure Gambit is installed and gsc is in PATH.',
            },
          ],
          isError: true,
        };
      }

      // Parse version output
      // Format: v4.9.7-6-g64f4d369 20250817174740 x86_64-pc-linux-gnu "./configure ..."
      const versionMatch = versionOutput.match(
        /^(v[\d.]+-\d+-g([0-9a-f]+))\s+(\d+)\s+(\S+)/,
      );

      if (!versionMatch) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Could not parse gsc -v output:\n${versionOutput}`,
            },
          ],
          isError: true,
        };
      }

      const [, versionStr, commitHash, buildDate, arch] = versionMatch;

      const sections: string[] = [];
      sections.push('Gambit Version Info:');
      sections.push(`  version: ${versionStr}`);
      sections.push(`  commit: ${commitHash}`);
      sections.push(`  build_date: ${buildDate}`);
      sections.push(`  arch: ${arch}`);

      // If no repo path, just report version info
      if (!gambit_repo_path) {
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      // Validate repo path
      try {
        const st = await stat(gambit_repo_path);
        if (!st.isDirectory()) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Not a directory: ${gambit_repo_path}`,
              },
            ],
            isError: true,
          };
        }
      } catch {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Directory not found: ${gambit_repo_path}`,
            },
          ],
          isError: true,
        };
      }

      // Check it's a git repo
      const gitCheck = await runCmd('git', ['rev-parse', '--git-dir'], {
        cwd: gambit_repo_path,
      });
      if (gitCheck.exitCode !== 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Not a git repository: ${gambit_repo_path}`,
            },
          ],
          isError: true,
        };
      }

      // Resolve the short hash to a full commit
      const revParseResult = await runCmd(
        'git',
        ['rev-parse', commitHash],
        { cwd: gambit_repo_path },
      );
      if (revParseResult.exitCode !== 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text:
                `Commit ${commitHash} not found in ${gambit_repo_path}. ` +
                `Try running 'git fetch' in the Gambit repository.\n\n` +
                sections.join('\n'),
            },
          ],
          isError: true,
        };
      }

      const fullCommit = revParseResult.stdout.trim();
      sections.push(`  full_commit: ${fullCommit}`);

      if (!output_dir) {
        return {
          content: [
            {
              type: 'text' as const,
              text:
                sections.join('\n') +
                '\n\nCommit found in repo. Provide output_dir to extract source files.',
            },
          ],
        };
      }

      // Extract source files via git archive
      const dirs = subdirs ?? ['gsc', 'lib'];
      const archiveArgs = ['archive', fullCommit, '--', ...dirs];

      // Create output directory
      const mkdirResult = await runCmd('mkdir', ['-p', output_dir]);
      if (mkdirResult.exitCode !== 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to create output directory: ${output_dir}\n${mkdirResult.stderr}`,
            },
          ],
          isError: true,
        };
      }

      // git archive | tar xf -
      const extractResult = await new Promise<{
        stdout: string;
        stderr: string;
        exitCode: number;
      }>((resolve) => {
        const gitProc = execFile(
          'git',
          archiveArgs,
          {
            cwd: gambit_repo_path,
            maxBuffer: 100 * 1024 * 1024, // 100MB for source trees
            encoding: 'buffer' as BufferEncoding,
            timeout: 60000,
          },
          (error, stdout) => {
            if (error) {
              resolve({
                stdout: '',
                stderr: error.message,
                exitCode: 1,
              });
              return;
            }
            // Pipe to tar
            const tarProc = execFile(
              'tar',
              ['xf', '-', '-C', output_dir],
              { timeout: 60000 },
              (tarError, _tarStdout, tarStderr) => {
                if (tarError) {
                  resolve({
                    stdout: '',
                    stderr: tarStderr ?? tarError.message,
                    exitCode: 1,
                  });
                } else {
                  resolve({ stdout: '', stderr: '', exitCode: 0 });
                }
              },
            );
            tarProc.stdin!.write(stdout);
            tarProc.stdin!.end();
          },
        );
      });

      if (extractResult.exitCode !== 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text:
                `Failed to extract source files:\n${extractResult.stderr}\n\n` +
                sections.join('\n'),
            },
          ],
          isError: true,
        };
      }

      sections.push('');
      sections.push(`Extracted ${dirs.join(', ')} to ${output_dir}`);

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
