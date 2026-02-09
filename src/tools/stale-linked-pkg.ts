import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readdir, stat, readlink, realpath } from 'node:fs/promises';
import { join, relative } from 'node:path';
import { homedir } from 'node:os';

interface StalePkgFile {
  sourcePath: string;
  compiledPath: string;
  sourceMtime: Date;
  compiledMtime: Date;
}

interface LinkedPkgReport {
  pkgName: string;
  linkTarget: string;
  staleFiles: StalePkgFile[];
  totalSourceFiles: number;
  totalCompiledFiles: number;
}

/**
 * Recursively find all .ss files in a directory.
 */
async function findSsFiles(dir: string): Promise<string[]> {
  const results: string[] = [];
  let entries: string[];
  try {
    entries = await readdir(dir);
  } catch {
    return results;
  }
  for (const name of entries) {
    if (name.startsWith('.')) continue;
    const full = join(dir, name);
    try {
      const st = await stat(full);
      if (st.isDirectory()) {
        results.push(...await findSsFiles(full));
      } else if (st.isFile() && name.endsWith('.ss') && !name.endsWith('-test.ss')) {
        results.push(full);
      }
    } catch {
      // skip
    }
  }
  return results;
}

/**
 * Find compiled artifacts (.ssi, .scm) for a given source file in the lib directory.
 * Maps source path like "qt.ss" to compiled patterns like "qt.ssi", "qt__0.scm" etc.
 */
async function findCompiledArtifacts(
  sourceBaseName: string,
  libDir: string,
): Promise<Map<string, number>> {
  const artifacts = new Map<string, number>();
  // Convert source relative path to compiled prefix
  // e.g. "qt.ss" -> "qt", "subdir/foo.ss" -> "subdir_foo"
  const prefix = sourceBaseName.replace(/\.ss$/, '').replace(/\//g, '_');
  let entries: string[];
  try {
    entries = await readdir(libDir);
  } catch {
    return artifacts;
  }
  for (const name of entries) {
    if (
      (name === `${prefix}.ssi`) ||
      (name.startsWith(`${prefix}__`) && name.endsWith('.scm')) ||
      (name.startsWith(`${prefix}__`) && name.endsWith('.ssi'))
    ) {
      try {
        const st = await stat(join(libDir, name));
        artifacts.set(name, st.mtimeMs);
      } catch {
        // skip
      }
    }
  }
  return artifacts;
}

export function registerStaleLinkedPkgTool(server: McpServer): void {
  server.registerTool(
    'gerbil_stale_linked_pkg',
    {
      title: 'Detect Stale Linked Packages',
      description:
        'Detect stale linked packages whose source is newer than compiled artifacts. ' +
        'Checks .gerbil/pkg/ for symlinked packages, compares source .ss files against ' +
        'compiled .ssi/.scm artifacts in .gerbil/lib/, and reports which linked packages ' +
        'need rebuilding with `gerbil pkg build`.',
      inputSchema: {
        project_path: z
          .string()
          .describe('Path to the Gerbil project directory'),
        gerbil_path: z
          .string()
          .optional()
          .describe(
            'Override for the global Gerbil path. Defaults to $GERBIL_PATH or ~/.gerbil',
          ),
      },
    },
    async ({ project_path, gerbil_path }) => {
      const globalBase =
        gerbil_path ??
        process.env.GERBIL_PATH ??
        join(homedir(), '.gerbil');

      // Find linked packages in .gerbil/pkg/
      const pkgDir = join(project_path, '.gerbil', 'pkg');
      let pkgEntries: string[];
      try {
        pkgEntries = await readdir(pkgDir);
      } catch {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No .gerbil/pkg/ directory found in ${project_path}.\nNo linked packages to check.`,
            },
          ],
        };
      }

      const linkedPkgs: Array<{ name: string; target: string }> = [];
      for (const name of pkgEntries) {
        const pkgPath = join(pkgDir, name);
        try {
          const linkTarget = await readlink(pkgPath);
          const resolved = await realpath(pkgPath);
          linkedPkgs.push({ name, target: resolved });
        } catch {
          // Not a symlink or can't resolve — skip
        }
      }

      if (linkedPkgs.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No linked packages found in ${pkgDir}.\nAll packages are installed normally.`,
            },
          ],
        };
      }

      // Check each linked package
      const reports: LinkedPkgReport[] = [];

      for (const pkg of linkedPkgs) {
        const sourceFiles = await findSsFiles(pkg.target);

        // Look for compiled artifacts in both project-local and global lib dirs
        const libDirs = [
          join(project_path, '.gerbil', 'lib'),
          join(globalBase, 'lib'),
        ];

        const staleFiles: StalePkgFile[] = [];

        for (const sourceFile of sourceFiles) {
          const relPath = relative(pkg.target, sourceFile);
          const sourceStat = await stat(sourceFile).catch(() => null);
          if (!sourceStat) continue;

          for (const libDir of libDirs) {
            const compiled = await findCompiledArtifacts(relPath, libDir);
            for (const [artifactName, compiledMtime] of compiled) {
              if (sourceStat.mtimeMs > compiledMtime) {
                staleFiles.push({
                  sourcePath: relPath,
                  compiledPath: artifactName,
                  sourceMtime: new Date(sourceStat.mtimeMs),
                  compiledMtime: new Date(compiledMtime),
                });
              }
            }
          }
        }

        reports.push({
          pkgName: pkg.name,
          linkTarget: pkg.target,
          staleFiles,
          totalSourceFiles: sourceFiles.length,
          totalCompiledFiles: 0, // not critical for output
        });
      }

      // Format output
      const sections: string[] = [
        `Stale Linked Package Report`,
        ``,
        `Project: ${project_path}`,
        `Linked packages: ${linkedPkgs.length}`,
        '',
      ];

      const staleReports = reports.filter((r) => r.staleFiles.length > 0);
      const freshReports = reports.filter((r) => r.staleFiles.length === 0);

      if (staleReports.length === 0) {
        sections.push('All linked packages are up to date.');
      } else {
        sections.push(`${staleReports.length} package(s) have stale compiled artifacts:`);
        sections.push('');

        for (const report of staleReports) {
          sections.push(`  STALE: ${report.pkgName} (${report.linkTarget})`);
          sections.push(`    ${report.staleFiles.length} stale file(s):`);
          // Deduplicate by source path
          const bySource = new Map<string, StalePkgFile>();
          for (const f of report.staleFiles) {
            if (!bySource.has(f.sourcePath) ||
                f.compiledMtime < bySource.get(f.sourcePath)!.compiledMtime) {
              bySource.set(f.sourcePath, f);
            }
          }
          for (const [sourcePath, f] of bySource) {
            const srcTime = f.sourceMtime.toISOString().replace('T', ' ').replace(/\.\d+Z$/, '');
            const compTime = f.compiledMtime.toISOString().replace('T', ' ').replace(/\.\d+Z$/, '');
            sections.push(`      ${sourcePath}: source ${srcTime} > compiled ${compTime}`);
          }
          sections.push(`    Fix: gerbil pkg build ${report.pkgName}`);
          sections.push('');
        }
      }

      if (freshReports.length > 0) {
        sections.push('');
        sections.push(`Up-to-date packages: ${freshReports.map((r) => r.pkgName).join(', ')}`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: staleReports.length > 0,
      };
    },
  );
}
