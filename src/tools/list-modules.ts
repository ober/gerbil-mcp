import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readdir, stat } from 'node:fs/promises';
import { join, relative } from 'node:path';

async function findSsiFiles(dir: string, files: string[] = []): Promise<string[]> {
  let entries;
  try {
    entries = await readdir(dir);
  } catch {
    return files;
  }
  for (const entry of entries) {
    const fullPath = join(dir, entry);
    let s;
    try {
      s = await stat(fullPath);
    } catch {
      continue;
    }
    if (s.isDirectory()) {
      await findSsiFiles(fullPath, files);
    } else if (entry.endsWith('.ssi')) {
      files.push(fullPath);
    }
  }
  return files;
}

function resolveLibDir(): string {
  const gerbilHome = process.env.GERBIL_HOME ?? '/opt/gerbil';
  return join(gerbilHome, 'lib');
}

export function registerListModulesTool(server: McpServer): void {
  server.registerTool(
    'gerbil_list_std_modules',
    {
      title: 'List Gerbil Modules',
      description:
        'List available Gerbil standard library modules by scanning the installation. ' +
        'Optionally filter by prefix (e.g. "std/text", "std/net", "gerbil"). ' +
        'Returns module paths like :std/text/json, :std/iter, etc.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        prefix: z
          .string()
          .optional()
          .describe('Filter modules by prefix (e.g. "std/text", "std/net")'),
      },
    },
    async ({ prefix }) => {
      const libDir = resolveLibDir();
      const ssiFiles = await findSsiFiles(libDir);

      let modules = ssiFiles
        .map((f) => {
          const rel = relative(libDir, f);
          // Remove .ssi extension, convert to module path
          const modPath = rel.replace(/\.ssi$/, '');
          return `:${modPath}`;
        })
        .filter((m) => {
          // Skip internal modules (starting with underscore)
          const parts = m.split('/');
          return !parts.some((p) => p.startsWith('_'));
        })
        .sort();

      if (prefix) {
        const normalizedPrefix = prefix.startsWith(':') ? prefix : `:${prefix}`;
        modules = modules.filter((m) => m.startsWith(normalizedPrefix));
      }

      if (modules.length === 0) {
        const msg = prefix
          ? `No modules found matching prefix "${prefix}".`
          : 'No modules found. Is GERBIL_HOME set correctly?';
        return {
          content: [{ type: 'text' as const, text: msg }],
        };
      }

      // Group by top-level category
      const groups = new Map<string, string[]>();
      for (const mod of modules) {
        // :std/text/json -> std
        const parts = mod.slice(1).split('/');
        const category = parts[0];
        if (!groups.has(category)) groups.set(category, []);
        groups.get(category)!.push(mod);
      }

      const lines: string[] = [];
      if (prefix) {
        lines.push(`Modules matching "${prefix}" (${modules.length} total):`);
      } else {
        lines.push(`Available modules (${modules.length} total):`);
      }
      lines.push('');

      for (const [category, mods] of groups) {
        lines.push(`[${category}] (${mods.length} modules)`);
        for (const m of mods) {
          lines.push(`  ${m}`);
        }
        lines.push('');
      }

      return {
        content: [{ type: 'text' as const, text: lines.join('\n') }],
      };
    },
  );
}
