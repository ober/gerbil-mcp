import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readdir, stat, readlink, realpath, copyFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join, relative, basename } from 'node:path';
import { homedir } from 'node:os';

interface ArtifactComparison {
  artifact: string;
  localPath: string;
  globalPath: string;
  localMtime: Date;
  globalMtime: Date;
  localSize: number;
  globalSize: number;
}

interface PkgSyncReport {
  pkgName: string;
  linkTarget: string;
  staleArtifacts: ArtifactComparison[];
  missingGlobal: string[];
  upToDate: number;
}

/**
 * Recursively find artifact files (.ssi, .scm, .so, .o1) in a directory.
 */
async function findArtifacts(dir: string, extensions: Set<string>): Promise<Map<string, string>> {
  const results = new Map<string, string>(); // relative path -> absolute path
  await scanArtifacts(dir, dir, results, extensions);
  return results;
}

async function scanArtifacts(
  baseDir: string,
  currentDir: string,
  results: Map<string, string>,
  extensions: Set<string>,
): Promise<void> {
  let entries: string[];
  try {
    entries = await readdir(currentDir);
  } catch {
    return;
  }
  for (const name of entries) {
    if (name.startsWith('.')) continue;
    const full = join(currentDir, name);
    try {
      const st = await stat(full);
      if (st.isDirectory()) {
        await scanArtifacts(baseDir, full, results, extensions);
      } else if (st.isFile()) {
        const ext = name.slice(name.lastIndexOf('.'));
        if (extensions.has(ext)) {
          const rel = relative(baseDir, full);
          results.set(rel, full);
        }
      }
    } catch {
      // skip inaccessible
    }
  }
}

const ARTIFACT_EXTENSIONS = new Set(['.ssi', '.scm', '.so', '.o1']);

export function registerPkgLinkSyncTool(server: McpServer): void {
  server.registerTool(
    'gerbil_pkg_link_sync',
    {
      title: 'Sync Linked Package Artifacts',
      description:
        'Detect and optionally sync stale linked-package artifacts (.ssi, .so, .scm) ' +
        'between a linked package\'s local .gerbil/lib/ and the global ~/.gerbil/lib/. ' +
        'After rebuilding a linked package, the global copies may be outdated, causing ' +
        '"unbound identifier" or "undefined symbol" errors in downstream projects. ' +
        'Reports which files are stale and can copy them with dry_run safety.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: true,
      },
      inputSchema: {
        pkg_name: z
          .string()
          .describe('Name of the linked package to check (as shown by `gerbil pkg list`)'),
        pkg_path: z
          .string()
          .optional()
          .describe('Path to the package source directory (auto-detected from ~/.gerbil/pkg/ symlink if omitted)'),
        gerbil_path: z
          .string()
          .optional()
          .describe('Override for the global Gerbil path (default: $GERBIL_PATH or ~/.gerbil)'),
        sync: z
          .boolean()
          .optional()
          .describe('Actually copy stale artifacts from local to global (default: false, dry-run)'),
        dry_run: z
          .boolean()
          .optional()
          .describe('Show what would be synced without copying (default: true)'),
      },
    },
    async ({ pkg_name, pkg_path, gerbil_path, sync, dry_run }) => {
      const globalBase = gerbil_path ?? process.env.GERBIL_PATH ?? join(homedir(), '.gerbil');
      const shouldSync = sync === true && dry_run !== true;

      // Resolve the package source directory
      let pkgSourceDir: string;
      if (pkg_path) {
        pkgSourceDir = pkg_path;
      } else {
        // Try to find via ~/.gerbil/pkg/ symlink
        const pkgLinkPath = join(globalBase, 'pkg', pkg_name);
        try {
          pkgSourceDir = await realpath(pkgLinkPath);
        } catch {
          return {
            content: [{
              type: 'text' as const,
              text: `Package "${pkg_name}" not found as a linked package.\n` +
                `Checked: ${pkgLinkPath}\n` +
                `Provide pkg_path explicitly or ensure the package is linked via \`gerbil pkg link\`.`,
            }],
            isError: true,
          };
        }
      }

      // Find local artifacts in the package's .gerbil/lib/
      const localLibDir = join(pkgSourceDir, '.gerbil', 'lib');
      if (!existsSync(localLibDir)) {
        return {
          content: [{
            type: 'text' as const,
            text: `No .gerbil/lib/ directory found in ${pkgSourceDir}.\n` +
              `The package may need to be built first: \`gerbil pkg build ${pkg_name}\``,
          }],
          isError: true,
        };
      }

      // Find global artifacts
      const globalLibDir = join(globalBase, 'lib');
      if (!existsSync(globalLibDir)) {
        return {
          content: [{
            type: 'text' as const,
            text: `No global lib directory found at ${globalLibDir}.`,
          }],
          isError: true,
        };
      }

      // Scan both directories
      const localArtifacts = await findArtifacts(localLibDir, ARTIFACT_EXTENSIONS);
      const globalArtifacts = await findArtifacts(globalLibDir, ARTIFACT_EXTENSIONS);

      // Compare: find local artifacts that are also in global but newer
      const staleArtifacts: ArtifactComparison[] = [];
      const missingGlobal: string[] = [];
      let upToDate = 0;

      for (const [relPath, localPath] of localArtifacts) {
        // Only check artifacts that belong to this package
        // Package artifacts are typically under pkg_name/ subdir or prefixed
        const globalPath = join(globalLibDir, relPath);

        if (!existsSync(globalPath)) {
          // Check if the artifact belongs to this package
          if (relPath.startsWith(pkg_name + '/') || relPath.startsWith(pkg_name + '_')) {
            missingGlobal.push(relPath);
          }
          continue;
        }

        try {
          const localStat = await stat(localPath);
          const globalStat = await stat(globalPath);

          if (localStat.mtimeMs > globalStat.mtimeMs || localStat.size !== globalStat.size) {
            staleArtifacts.push({
              artifact: relPath,
              localPath,
              globalPath,
              localMtime: new Date(localStat.mtimeMs),
              globalMtime: new Date(globalStat.mtimeMs),
              localSize: localStat.size,
              globalSize: globalStat.size,
            });
          } else {
            upToDate++;
          }
        } catch {
          // skip comparison failures
        }
      }

      // Report
      const sections: string[] = [];
      sections.push(`Package: ${pkg_name}`);
      sections.push(`Source: ${pkgSourceDir}`);
      sections.push(`Local lib: ${localLibDir}`);
      sections.push(`Global lib: ${globalLibDir}`);
      sections.push(`Local artifacts: ${localArtifacts.size}`);
      sections.push('');

      if (staleArtifacts.length === 0 && missingGlobal.length === 0) {
        sections.push(`All ${upToDate} matching artifacts are up to date.`);
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      if (staleArtifacts.length > 0) {
        sections.push(`Stale artifacts (${staleArtifacts.length}):`);
        for (const sa of staleArtifacts) {
          const localTime = sa.localMtime.toISOString().replace('T', ' ').replace(/\.\d+Z$/, '');
          const globalTime = sa.globalMtime.toISOString().replace('T', ' ').replace(/\.\d+Z$/, '');
          const sizeNote = sa.localSize !== sa.globalSize
            ? ` (size: ${sa.localSize} vs ${sa.globalSize})`
            : '';
          sections.push(`  ${sa.artifact}: local ${localTime} > global ${globalTime}${sizeNote}`);
        }
        sections.push('');
      }

      if (missingGlobal.length > 0) {
        sections.push(`Missing from global (${missingGlobal.length}):`);
        for (const m of missingGlobal) {
          sections.push(`  ${m}`);
        }
        sections.push('');
      }

      // Sync if requested
      if (shouldSync && staleArtifacts.length > 0) {
        sections.push('Syncing stale artifacts...');
        let synced = 0;
        let failed = 0;
        for (const sa of staleArtifacts) {
          try {
            await copyFile(sa.localPath, sa.globalPath);
            synced++;
          } catch (err) {
            failed++;
            sections.push(`  FAILED: ${sa.artifact}: ${err instanceof Error ? err.message : String(err)}`);
          }
        }
        sections.push(`  Synced: ${synced}, Failed: ${failed}`);
      } else if (staleArtifacts.length > 0) {
        sections.push('Run with sync: true to copy local artifacts to global.');
        sections.push(`Or rebuild: gerbil pkg build ${pkg_name}`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: !shouldSync && staleArtifacts.length > 0,
      };
    },
  );
}
