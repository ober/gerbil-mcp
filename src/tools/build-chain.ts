import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFile, stat, readdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join, resolve, dirname, basename } from 'node:path';
import { z } from 'zod';
import { runGerbilCmd } from '../gxi.js';

/**
 * Extract dependency project names from gerbil.pkg depend: entries.
 */
async function extractPkgDepends(projectPath: string): Promise<string[]> {
  try {
    const content = await readFile(join(projectPath, 'gerbil.pkg'), 'utf-8');
    // Match depend: (dep1 dep2 ...) or depend: dep1
    const match = content.match(/\bdepend:\s*\(([^)]+)\)/);
    if (match) {
      return match[1].split(/\s+/).filter(s => s.length > 0);
    }
    const single = content.match(/\bdepend:\s+(\S+)/);
    if (single) {
      return [single[1]];
    }
  } catch {
    // No gerbil.pkg or can't read it
  }
  return [];
}

/**
 * Extract GERBIL_LOADPATH entries from a Makefile.
 * Looks for patterns like: GERBIL_LOADPATH=... or export GERBIL_LOADPATH
 */
async function extractMakefileLoadpath(projectPath: string): Promise<string[]> {
  try {
    const content = await readFile(join(projectPath, 'Makefile'), 'utf-8');
    const paths: string[] = [];
    // Match GERBIL_LOADPATH=path1:path2 or GERBIL_LOADPATH := path1:path2
    const patterns = [
      /GERBIL_LOADPATH\s*[:?]?=\s*(.+)/g,
      /--loadpath\s+(\S+)/g,
    ];
    for (const pattern of patterns) {
      let match;
      while ((match = pattern.exec(content)) !== null) {
        const raw = match[1].trim();
        // Split on : and resolve relative paths
        for (const p of raw.split(':')) {
          const trimmed = p.trim().replace(/\$\(HOME\)/g, process.env.HOME || '~')
            .replace(/\$HOME/g, process.env.HOME || '~')
            .replace(/\$\(PWD\)/g, projectPath)
            .replace(/\$\(shell pwd\)/g, projectPath);
          if (trimmed && !trimmed.startsWith('#')) {
            const resolved = trimmed.startsWith('/')
              ? trimmed
              : resolve(projectPath, trimmed);
            paths.push(resolved);
          }
        }
      }
    }
    return [...new Set(paths)];
  } catch {
    return [];
  }
}

/**
 * Discover upstream project directories from loadpath entries.
 * A loadpath like /home/user/gerbil-qt/.gerbil/lib -> project is /home/user/gerbil-qt
 */
function loadpathToProjectDirs(loadpaths: string[]): string[] {
  const dirs: string[] = [];
  for (const lp of loadpaths) {
    // Pattern: <project>/.gerbil/lib
    if (lp.endsWith('/.gerbil/lib')) {
      dirs.push(lp.slice(0, -'/.gerbil/lib'.length));
    } else if (basename(lp) === 'lib' && basename(dirname(lp)) === '.gerbil') {
      dirs.push(dirname(dirname(lp)));
    }
  }
  return dirs;
}

/**
 * Check if any .ss source file in a project is newer than its .gerbil directory.
 */
async function needsRebuild(projectPath: string): Promise<boolean> {
  const gerbilDir = join(projectPath, '.gerbil');
  if (!existsSync(gerbilDir)) return true;

  let latestArtifact = 0;
  try {
    const gstat = await stat(gerbilDir);
    latestArtifact = gstat.mtimeMs;
  } catch {
    return true;
  }

  // Check if any .ss file is newer than .gerbil dir
  try {
    const files = await readdir(projectPath);
    for (const f of files) {
      if (f.endsWith('.ss') || f.endsWith('.scm')) {
        const fstat = await stat(join(projectPath, f));
        if (fstat.mtimeMs > latestArtifact) return true;
      }
    }
  } catch {
    return true;
  }
  return false;
}

export function registerBuildChainTool(server: McpServer): void {
  server.registerTool(
    'gerbil_build_chain',
    {
      title: 'Build Project Chain',
      description:
        'Build a chain of dependent Gerbil projects in dependency order. ' +
        'Reads gerbil.pkg depend: entries and GERBIL_LOADPATH from the Makefile ' +
        'to identify upstream projects. Checks if upstream projects need rebuilding ' +
        '(source newer than artifacts) and builds them in order before building ' +
        'the target project. Eliminates manual upstream build steps.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        project_path: z
          .string()
          .describe('Path to the target project directory to build'),
        force: z
          .boolean()
          .optional()
          .describe('Force rebuild of all dependencies even if up-to-date (default: false)'),
        dry_run: z
          .boolean()
          .optional()
          .describe('Show what would be built without actually building (default: false)'),
      },
    },
    async ({ project_path, force, dry_run }) => {
      if (!existsSync(join(project_path, 'gerbil.pkg'))) {
        return {
          content: [{
            type: 'text' as const,
            text: `No gerbil.pkg found in ${project_path}. Not a Gerbil project.`,
          }],
          isError: true,
        };
      }

      // Discover dependency chain
      const pkgDepends = await extractPkgDepends(project_path);
      const makefileLoadpaths = await extractMakefileLoadpath(project_path);
      const upstreamDirs = loadpathToProjectDirs(makefileLoadpaths);

      // Collect all upstream projects that have gerbil.pkg
      const upstreamProjects: Array<{ path: string; name: string; needsBuild: boolean }> = [];
      for (const dir of upstreamDirs) {
        if (existsSync(join(dir, 'gerbil.pkg'))) {
          const needs = force || await needsRebuild(dir);
          upstreamProjects.push({
            path: dir,
            name: basename(dir),
            needsBuild: needs,
          });
        }
      }

      const sections: string[] = [];

      // Report discovery
      sections.push(`Target: ${basename(project_path)}`);
      if (pkgDepends.length > 0) {
        sections.push(`Package dependencies: ${pkgDepends.join(', ')}`);
      }
      if (upstreamProjects.length > 0) {
        sections.push(`Upstream projects found: ${upstreamProjects.map(p => p.name).join(', ')}`);
      } else {
        sections.push('No upstream project dependencies detected.');
      }
      sections.push('');

      if (dry_run) {
        // Dry run mode — just report what would happen
        for (const proj of upstreamProjects) {
          const status = proj.needsBuild ? 'NEEDS REBUILD' : 'up-to-date';
          sections.push(`  ${proj.name}: ${status} (${proj.path})`);
        }
        const targetNeeds = force || await needsRebuild(project_path);
        sections.push(`  ${basename(project_path)}: ${targetNeeds ? 'NEEDS BUILD' : 'up-to-date'}`);
        sections.push('');
        sections.push('(dry-run mode — no builds performed)');
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      // Build upstream projects that need it
      let allSuccess = true;
      for (const proj of upstreamProjects) {
        if (!proj.needsBuild) {
          sections.push(`  ${proj.name}: up-to-date (skipped)`);
          continue;
        }

        sections.push(`  Building ${proj.name}...`);

        // Try make first, then gerbil build
        let buildResult;
        if (existsSync(join(proj.path, 'Makefile'))) {
          buildResult = await runMakeBuild(proj.path);
        } else {
          buildResult = await runGerbilCmd(['build'], { cwd: proj.path, timeout: 120_000 });
        }

        if (buildResult.exitCode === 0) {
          sections.push(`  ${proj.name}: build succeeded`);
        } else {
          const errOutput = [buildResult.stdout, buildResult.stderr].filter(Boolean).join('\n').trim();
          sections.push(`  ${proj.name}: BUILD FAILED`);
          if (errOutput) {
            sections.push(`    ${errOutput.split('\n').slice(0, 5).join('\n    ')}`);
          }
          allSuccess = false;
          break; // Stop on first failure
        }
      }

      if (!allSuccess) {
        sections.push('');
        sections.push('Build chain stopped due to upstream failure.');
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
          isError: true,
        };
      }

      // Build the target project
      sections.push('');
      sections.push(`  Building ${basename(project_path)}...`);

      let targetResult;
      if (existsSync(join(project_path, 'Makefile'))) {
        targetResult = await runMakeBuild(project_path);
      } else {
        targetResult = await runGerbilCmd(['build'], { cwd: project_path, timeout: 120_000 });
      }

      if (targetResult.exitCode === 0) {
        sections.push(`  ${basename(project_path)}: build succeeded`);
        sections.push('');
        const builtCount = upstreamProjects.filter(p => p.needsBuild).length + 1;
        sections.push(`Build chain complete: ${builtCount} project(s) built.`);
      } else {
        const errOutput = [targetResult.stdout, targetResult.stderr].filter(Boolean).join('\n').trim();
        sections.push(`  ${basename(project_path)}: BUILD FAILED`);
        if (errOutput) {
          sections.push(`    ${errOutput.split('\n').slice(0, 10).join('\n    ')}`);
        }
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
          isError: true,
        };
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

import { execFile } from 'node:child_process';

function runMakeBuild(
  cwd: string,
): Promise<{ stdout: string; stderr: string; exitCode: number }> {
  return new Promise((resolve) => {
    execFile(
      'make',
      ['build'],
      { timeout: 120_000, maxBuffer: 1024 * 1024, cwd },
      (error, stdout, stderr) => {
        if (error) {
          resolve({
            stdout: stdout ?? '',
            stderr: stderr ?? '',
            exitCode: typeof error.code === 'number' ? error.code : 2,
          });
        } else {
          resolve({ stdout: stdout ?? '', stderr: stderr ?? '', exitCode: 0 });
        }
      },
    );
  });
}
