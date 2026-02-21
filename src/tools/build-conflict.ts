import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { execSync } from 'node:child_process';
import { readFile, stat } from 'node:fs/promises';
import { join, resolve } from 'node:path';

interface BuildProcess {
  pid: number;
  command: string;
  cwd?: string;
}

/**
 * Find running gerbil build processes using ps.
 */
function findGerbilBuildProcesses(): BuildProcess[] {
  const processes: BuildProcess[] = [];
  try {
    // Use ps to find gerbil-related processes
    const output = execSync(
      'ps aux 2>/dev/null || true',
      { encoding: 'utf-8', timeout: 5000 },
    );
    const lines = output.split('\n');
    for (const line of lines) {
      // Match gerbil build, gxc, and gxi processes that look like compilation
      if (
        /gerbil\s+build/i.test(line) ||
        /gxc\s/i.test(line) ||
        /gerbil-build/i.test(line)
      ) {
        const parts = line.trim().split(/\s+/);
        const pid = parseInt(parts[1], 10);
        if (!isNaN(pid)) {
          const command = parts.slice(10).join(' ');
          // Try to get cwd from /proc
          let cwd: string | undefined;
          try {
            cwd = execSync(`readlink /proc/${pid}/cwd 2>/dev/null || true`, {
              encoding: 'utf-8',
              timeout: 2000,
            }).trim();
          } catch {
            // Can't read cwd
          }
          processes.push({ pid, command, cwd: cwd || undefined });
        }
      }
    }
  } catch {
    // ps not available
  }
  return processes;
}

/**
 * Check for lock files in .gerbil/ directory that indicate an active build.
 */
async function checkLockFiles(
  projectPath: string,
): Promise<string[]> {
  const lockPaths: string[] = [];
  const gerbilDir = join(projectPath, '.gerbil');

  // Check for common lock patterns
  const lockPatterns = [
    join(gerbilDir, 'lock'),
    join(gerbilDir, 'build.lock'),
    join(gerbilDir, 'lib', '.lock'),
  ];

  for (const lockPath of lockPatterns) {
    try {
      const s = await stat(lockPath);
      if (s.isFile()) {
        const ageMs = Date.now() - s.mtimeMs;
        lockPaths.push(
          `${lockPath} (age: ${Math.round(ageMs / 1000)}s)`,
        );
      }
    } catch {
      // Lock file doesn't exist
    }
  }

  return lockPaths;
}

export function registerBuildConflictTool(server: McpServer): void {
  server.registerTool(
    'gerbil_build_conflict_check',
    {
      title: 'Detect Parallel Build Conflicts',
      description:
        'Detect when multiple gerbil build processes are running on the same ' +
        'project directory. Checks for: (1) running gerbil/gxc processes via ps, ' +
        '(2) lock files in .gerbil/, (3) matching working directories. ' +
        'Warns about potential conflicts with suggestions to kill old builds.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        project_path: z
          .string()
          .describe('Project directory to check for build conflicts'),
      },
    },
    async ({ project_path }) => {
      const absPath = resolve(project_path);
      const sections: string[] = [];
      let hasConflict = false;

      // Step 1: Find running gerbil build processes
      const processes = findGerbilBuildProcesses();
      const conflicting = processes.filter(
        (p) => p.cwd && resolve(p.cwd) === absPath,
      );

      sections.push(`Build conflict check: ${absPath}`);
      sections.push('');

      if (conflicting.length > 0) {
        hasConflict = true;
        sections.push(
          `WARNING: ${conflicting.length} gerbil build process(es) found for this directory:`,
        );
        for (const p of conflicting) {
          sections.push(`  PID ${p.pid}: ${p.command}`);
        }
        sections.push('');
        sections.push(
          'To avoid conflicts, kill existing builds before starting a new one:',
        );
        for (const p of conflicting) {
          sections.push(`  kill ${p.pid}`);
        }
        sections.push('');
      }

      // Also report other gerbil processes (not in this directory)
      const otherProcesses = processes.filter(
        (p) => !p.cwd || resolve(p.cwd) !== absPath,
      );
      if (otherProcesses.length > 0) {
        sections.push(
          `Other gerbil build processes (${otherProcesses.length}):`,
        );
        for (const p of otherProcesses) {
          sections.push(
            `  PID ${p.pid}: ${p.command}${p.cwd ? ` (${p.cwd})` : ''}`,
          );
        }
        sections.push('');
      }

      // Step 2: Check for lock files
      const locks = await checkLockFiles(absPath);
      if (locks.length > 0) {
        hasConflict = true;
        sections.push(`Lock files found (${locks.length}):`);
        for (const lock of locks) {
          sections.push(`  ${lock}`);
        }
        sections.push('');
        sections.push(
          'Lock files may indicate an active or crashed build. ' +
          'If no build is running, delete the lock files and retry.',
        );
        sections.push('');
      }

      if (!hasConflict && processes.length === 0) {
        sections.push('No build conflicts detected. Safe to build.');
      } else if (!hasConflict) {
        sections.push(
          'No conflicts for this specific project. Safe to build.',
        );
      }

      return {
        content: [
          { type: 'text' as const, text: sections.join('\n') },
        ],
        isError: hasConflict,
      };
    },
  );
}
