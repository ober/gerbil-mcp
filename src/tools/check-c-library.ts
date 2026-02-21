import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { execSync } from 'node:child_process';
import { join } from 'node:path';

interface LibraryStatus {
  name: string;
  flag: string;
  available: boolean;
  pkgConfigName?: string;
  suggestedPackage?: string;
}

/**
 * Well-known library name to package mappings for common distros.
 */
const KNOWN_PACKAGES: Record<string, { deb: string; rpm?: string }> = {
  pcre2: { deb: 'libpcre2-dev', rpm: 'pcre2-devel' },
  'pcre2-8': { deb: 'libpcre2-dev', rpm: 'pcre2-devel' },
  sqlite3: { deb: 'libsqlite3-dev', rpm: 'sqlite-devel' },
  ssl: { deb: 'libssl-dev', rpm: 'openssl-devel' },
  crypto: { deb: 'libssl-dev', rpm: 'openssl-devel' },
  z: { deb: 'zlib1g-dev', rpm: 'zlib-devel' },
  pthread: { deb: '' }, // usually available
  m: { deb: '' }, // math, always available
  dl: { deb: '' }, // dynamic loader, always available
  rt: { deb: '' }, // realtime, always available
  curl: { deb: 'libcurl4-openssl-dev', rpm: 'libcurl-devel' },
  xml2: { deb: 'libxml2-dev', rpm: 'libxml2-devel' },
  png: { deb: 'libpng-dev', rpm: 'libpng-devel' },
  jpeg: { deb: 'libjpeg-dev', rpm: 'libjpeg-turbo-devel' },
  uuid: { deb: 'uuid-dev', rpm: 'libuuid-devel' },
  ev: { deb: 'libev-dev', rpm: 'libev-devel' },
  lmdb: { deb: 'liblmdb-dev', rpm: 'lmdb-devel' },
  leveldb: { deb: 'libleveldb-dev', rpm: 'leveldb-devel' },
  yaml: { deb: 'libyaml-dev', rpm: 'libyaml-devel' },
};

/**
 * Extract -lXXX linker flags from build.ss content.
 */
function extractLinkerFlags(content: string): string[] {
  const flags: string[] = [];
  // Match -lXXX in ld-options strings
  const matches = content.matchAll(/-l(\S+)/g);
  for (const match of matches) {
    flags.push(match[1]);
  }
  return [...new Set(flags)]; // deduplicate
}

/**
 * Check if a library is available via pkg-config.
 */
function checkPkgConfig(libName: string): boolean {
  try {
    execSync(`pkg-config --exists ${libName} 2>/dev/null`, {
      timeout: 5000,
    });
    return true;
  } catch {
    return false;
  }
}

/**
 * Check if a library is available via ldconfig -p or by trying to find the .so.
 */
function checkLdconfig(libName: string): boolean {
  try {
    const output = execSync(
      `ldconfig -p 2>/dev/null | grep -i "lib${libName}" || true`,
      { encoding: 'utf-8', timeout: 5000 },
    );
    return output.trim().length > 0;
  } catch {
    return false;
  }
}

export function registerCheckCLibraryTool(server: McpServer): void {
  server.registerTool(
    'gerbil_check_c_library',
    {
      title: 'Check C Library Availability',
      description:
        'Scan build.ss for -lXXX linker flags and check if those C libraries ' +
        'are available on the system via pkg-config or ldconfig. Reports missing ' +
        'libraries with suggested package install commands. Use before building ' +
        'a project with FFI dependencies to catch missing libraries early.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        project_path: z
          .string()
          .optional()
          .describe(
            'Project directory containing build.ss (default: current directory)',
          ),
        build_file: z
          .string()
          .optional()
          .describe(
            'Path to build.ss file (alternative to project_path)',
          ),
        libraries: z
          .array(z.string())
          .optional()
          .describe(
            'List of library names to check directly (alternative to scanning build.ss). ' +
            'e.g., ["pcre2-8", "sqlite3"]',
          ),
      },
    },
    async ({ project_path, build_file, libraries }) => {
      let libNames: string[] = [];

      if (libraries && libraries.length > 0) {
        libNames = libraries;
      } else {
        // Read build.ss to extract -lXXX flags
        const buildPath = build_file ?? join(project_path ?? '.', 'build.ss');
        let content: string;
        try {
          content = await readFile(buildPath, 'utf-8');
        } catch {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Cannot read build file: ${buildPath}. Provide project_path, build_file, or libraries parameter.`,
              },
            ],
            isError: true,
          };
        }

        libNames = extractLinkerFlags(content);
        if (libNames.length === 0) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `No -lXXX linker flags found in ${buildPath}.`,
              },
            ],
          };
        }
      }

      // Check each library
      const results: LibraryStatus[] = [];
      for (const lib of libNames) {
        // Skip always-available system libraries
        if (['m', 'pthread', 'dl', 'rt', 'c'].includes(lib)) {
          results.push({ name: lib, flag: `-l${lib}`, available: true });
          continue;
        }

        const available =
          checkPkgConfig(lib) ||
          checkPkgConfig(`lib${lib}`) ||
          checkLdconfig(lib);

        const known = KNOWN_PACKAGES[lib];

        results.push({
          name: lib,
          flag: `-l${lib}`,
          available,
          pkgConfigName: lib,
          suggestedPackage: known?.deb || undefined,
        });
      }

      // Format output
      const sections: string[] = [];
      const missing = results.filter((r) => !r.available);
      const found = results.filter((r) => r.available);

      sections.push(
        `C Library Check: ${results.length} libraries, ${missing.length} missing`,
      );
      sections.push('');

      if (found.length > 0) {
        sections.push(`Available (${found.length}):`);
        for (const r of found) {
          sections.push(`  ${r.flag} — OK`);
        }
        sections.push('');
      }

      if (missing.length > 0) {
        sections.push(`Missing (${missing.length}):`);
        for (const r of missing) {
          let line = `  ${r.flag} — NOT FOUND`;
          if (r.suggestedPackage) {
            line += ` (install: sudo apt install ${r.suggestedPackage})`;
          }
          sections.push(line);
        }
        sections.push('');
        sections.push(
          'Install missing libraries before building to avoid link failures.',
        );
      }

      return {
        content: [
          { type: 'text' as const, text: sections.join('\n') },
        ],
        isError: missing.length > 0,
      };
    },
  );
}
