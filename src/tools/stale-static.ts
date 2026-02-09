import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readdir, stat } from 'node:fs/promises';
import { join } from 'node:path';
import { homedir } from 'node:os';

interface FileInfo {
  name: string;
  size: number;
  mtimeMs: number;
}

interface StaleEntry {
  name: string;
  globalSize: number;
  localSize: number;
  globalMtime: Date;
  localMtime: Date;
  stale: boolean;
}

async function scanDir(dir: string): Promise<Map<string, FileInfo>> {
  const map = new Map<string, FileInfo>();
  let entries: string[];
  try {
    entries = await readdir(dir);
  } catch {
    return map; // directory doesn't exist
  }
  for (const name of entries) {
    try {
      const st = await stat(join(dir, name));
      if (st.isFile()) {
        map.set(name, { name, size: st.size, mtimeMs: st.mtimeMs });
      }
    } catch {
      // skip files we can't stat
    }
  }
  return map;
}

function formatSize(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
  return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
}

function formatDate(d: Date): string {
  return d.toISOString().replace('T', ' ').replace(/\.\d+Z$/, '');
}

export function registerStaleStaticTool(server: McpServer): void {
  server.registerTool(
    'gerbil_stale_static',
    {
      title: 'Detect Stale Global Static Files',
      description:
        'Compare compiled artifacts (.scm, .c, .o) in the global $GERBIL_PATH/lib/static/ ' +
        'against project-local .gerbil/lib/static/ to detect stale global copies that could ' +
        'shadow the current project build during executable linking. Reports mismatched sizes ' +
        'and timestamps. Stale global artifacts are a common cause of segfaults and #!unbound ' +
        'errors in compiled Gerbil executables that don\'t reproduce in the REPL.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        project_path: z
          .string()
          .describe('Path to the Gerbil project directory (contains .gerbil/lib/static/)'),
        gerbil_path: z
          .string()
          .optional()
          .describe(
            'Override for the global Gerbil path. Defaults to $GERBIL_PATH or ~/.gerbil',
          ),
        extensions: z
          .array(z.string())
          .optional()
          .describe(
            'File extensions to check (default: [".scm", ".c", ".o"])',
          ),
      },
    },
    async ({ project_path, gerbil_path, extensions }) => {
      const exts = extensions ?? ['.scm', '.c', '.o'];

      // Resolve global gerbil path
      const globalBase =
        gerbil_path ??
        process.env.GERBIL_PATH ??
        join(homedir(), '.gerbil');
      const globalDir = join(globalBase, 'lib', 'static');
      const localDir = join(project_path, '.gerbil', 'lib', 'static');

      // Scan both directories
      const [globalFiles, localFiles] = await Promise.all([
        scanDir(globalDir),
        scanDir(localDir),
      ]);

      if (localFiles.size === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No local static files found in ${localDir}\nHas the project been built?`,
            },
          ],
        };
      }

      if (globalFiles.size === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No global static files found in ${globalDir}\nNo shadowing possible.`,
            },
          ],
        };
      }

      // Find overlapping files and compare
      const entries: StaleEntry[] = [];
      for (const [name, local] of localFiles) {
        // Filter by extension
        if (!exts.some((ext) => name.endsWith(ext))) continue;

        const global = globalFiles.get(name);
        if (!global) continue; // no global copy — no shadowing

        const stale =
          global.size !== local.size || global.mtimeMs < local.mtimeMs;

        entries.push({
          name,
          globalSize: global.size,
          localSize: local.size,
          globalMtime: new Date(global.mtimeMs),
          localMtime: new Date(local.mtimeMs),
          stale,
        });
      }

      if (entries.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text:
                'No overlapping static files found between global and local directories.\n' +
                `Global: ${globalDir} (${globalFiles.size} files)\n` +
                `Local:  ${localDir} (${localFiles.size} files)`,
            },
          ],
        };
      }

      // Sort: stale first, then by name
      entries.sort((a, b) => {
        if (a.stale !== b.stale) return a.stale ? -1 : 1;
        return a.name.localeCompare(b.name);
      });

      const staleCount = entries.filter((e) => e.stale).length;
      const okCount = entries.length - staleCount;

      const lines: string[] = [];
      lines.push(
        `Stale Static File Report`,
        ``,
        `Global: ${globalDir}`,
        `Local:  ${localDir}`,
        ``,
        `Found ${entries.length} overlapping files: ${staleCount} STALE, ${okCount} ok`,
        ``,
      );

      if (staleCount > 0) {
        lines.push('STALE files (global copy differs from local — may shadow build):');
        lines.push('');
        for (const e of entries.filter((e) => e.stale)) {
          const sizeMatch = e.globalSize === e.localSize;
          const sizeInfo = sizeMatch
            ? `size: ${formatSize(e.localSize)}`
            : `global: ${formatSize(e.globalSize)}, local: ${formatSize(e.localSize)}`;
          lines.push(`  STALE  ${e.name}`);
          lines.push(`         ${sizeInfo}`);
          lines.push(
            `         global mtime: ${formatDate(e.globalMtime)}`,
          );
          lines.push(
            `         local mtime:  ${formatDate(e.localMtime)}`,
          );
          lines.push('');
        }

        lines.push('To fix: delete the stale global copies, or run:');
        lines.push(`  rm ${globalDir}/<stale-file>`);
        lines.push('Then rebuild the project.');
      }

      if (okCount > 0) {
        lines.push('');
        lines.push(`Matching files (${okCount} files with same size, global not older): ok`);
      }

      return {
        content: [{ type: 'text' as const, text: lines.join('\n') }],
      };
    },
  );
}
