import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, readdir } from 'node:fs/promises';
import { join } from 'node:path';
import { homedir } from 'node:os';
import { runGxi, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

/**
 * Convert a module path like ":foo/bar/baz" to the static file base name "foo__bar__baz".
 */
function modulePathToStaticBase(modPath: string): string {
  const stripped = modPath.startsWith(':') ? modPath.slice(1) : modPath;
  return stripped.replace(/\//g, '__');
}

/**
 * Convert a module path like ":foo/bar/baz" to the definition prefix "foo/bar/baz#".
 */
function modulePathToDefPrefix(modPath: string): string {
  const stripped = modPath.startsWith(':') ? modPath.slice(1) : modPath;
  return stripped + '#';
}

/**
 * Fallback: scan compiled .scm files for exported symbols when the expander fails.
 * Looks in $GERBIL_PATH/lib/static/ for the compiled module file and extracts
 * all (define module-prefix#symbol ...) definitions.
 */
async function fallbackFromCompiledScm(modPath: string): Promise<string[] | null> {
  const gerbilPath = process.env.GERBIL_PATH ?? join(homedir(), '.gerbil');
  const staticDir = join(gerbilPath, 'lib', 'static');
  const baseName = modulePathToStaticBase(modPath);
  const defPrefix = modulePathToDefPrefix(modPath);

  // Try to find the .scm file — could be baseName.scm or with extra underscores for sub-modules
  let scmFile: string | null = null;
  try {
    const files = await readdir(staticDir);
    // Exact match first
    const exactName = baseName + '.scm';
    if (files.includes(exactName)) {
      scmFile = join(staticDir, exactName);
    } else {
      // Look for files that start with the base name (for sub-module aggregations)
      const candidates = files.filter(
        (f) => f.startsWith(baseName) && f.endsWith('.scm'),
      );
      if (candidates.length === 1) {
        scmFile = join(staticDir, candidates[0]);
      }
    }
  } catch {
    return null;
  }

  if (!scmFile) return null;

  try {
    const content = await readFile(scmFile, 'utf-8');
    const symbols: string[] = [];
    const seen = new Set<string>();

    // Match (define module/path#symbol-name ...) patterns
    const definePattern = /\(define\s+/g;
    let match: RegExpExecArray | null;
    while ((match = definePattern.exec(content)) !== null) {
      const rest = content.slice(match.index + match[0].length);
      // Extract the symbol name
      const symEnd = rest.search(/[\s()]/);
      if (symEnd <= 0) continue;
      const fullSym = rest.slice(0, symEnd);
      if (!fullSym.startsWith(defPrefix)) continue;

      const symName = fullSym.slice(defPrefix.length);
      // Skip internal symbols (containing #, ::, or starting with %)
      if (symName.includes('#') || symName.startsWith('%')) continue;
      // Skip timestamp
      if (symName === 'timestamp') continue;
      // Strip ::t, ::? etc. type descriptor suffixes for cleaner output
      // but keep the base symbol name
      if (!seen.has(symName)) {
        seen.add(symName);
        symbols.push(symName);
      }
    }

    return symbols.length > 0 ? symbols : null;
  } catch {
    return null;
  }
}

export function registerModuleExportsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_module_exports',
    {
      title: 'List Module Exports',
      description:
        'List all exported symbols from a Gerbil module. ' +
        'Example: module_path ":std/text/json" returns read-json, write-json, etc. ' +
        'Handles modules that use Gambit (declare ...) forms by falling back to ' +
        'scanning compiled .scm files when the expander fails.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        module_path: z
          .string()
          .describe('Module path (e.g. ":std/text/json", ":std/sugar", ":std/iter")'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution ' +
            '(e.g. ["/path/to/project/.gerbil/lib"])',
          ),
        project_path: z
          .string()
          .optional()
          .describe(
            'Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib',
          ),
      },
    },
    async ({ module_path, loadpath, project_path }) => {
      const modPath = module_path.startsWith(':') ? module_path : `:${module_path}`;

      const exprs = [
        '(import :gerbil/expander)',
        [
          '(with-catch',
          '  (lambda (e)',
          `    (display "${ERROR_MARKER}\\n")`,
          '    (display-exception e (current-output-port)))',
          '  (lambda ()',
          `    (let ((mod (import-module (quote ${modPath}) #f #t)))`,
          '      (let ((exports (module-context-export mod)))',
          '        (for-each',
          '          (lambda (e)',
          '            (displayln (module-export-name e)))',
          '          exports)))))',
        ].join(' '),
      ];

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const env = effectiveLoadpath.length > 0 ? buildLoadpathEnv(effectiveLoadpath) : undefined;

      const result = await runGxi(exprs, { env });

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Module introspection timed out.' }],
          isError: true,
        };
      }

      // Check for errors — if so, try fallback before reporting
      const hasExitError = result.exitCode !== 0 && result.stderr;
      const hasMarkerError = result.stdout.indexOf(ERROR_MARKER) !== -1;

      if (hasExitError || hasMarkerError) {
        // Try fallback from compiled .scm files
        const fallbackSymbols = await fallbackFromCompiledScm(modPath);
        if (fallbackSymbols && fallbackSymbols.length > 0) {
          const formatted = [
            `Module ${modPath} exports ${fallbackSymbols.length} symbol(s) (from compiled artifacts):`,
            '',
            ...fallbackSymbols.map((s) => `  ${s}`),
          ].join('\n');
          return {
            content: [{ type: 'text' as const, text: formatted }],
          };
        }

        // Fallback failed too — report original error
        if (hasExitError) {
          return {
            content: [
              { type: 'text' as const, text: `Failed to load module ${modPath}:\n${result.stderr.trim()}` },
            ],
            isError: true,
          };
        }
        const errorMsg = result.stdout.slice(result.stdout.indexOf(ERROR_MARKER) + ERROR_MARKER.length).trim();
        return {
          content: [{ type: 'text' as const, text: `Error introspecting ${modPath}:\n${errorMsg}` }],
          isError: true,
        };
      }

      const symbols = result.stdout
        .trim()
        .split('\n')
        .map((s) => s.trim())
        .filter(Boolean);

      if (symbols.length === 0) {
        return {
          content: [{ type: 'text' as const, text: `Module ${modPath} exports no symbols (or could not be introspected).` }],
        };
      }

      const formatted = [
        `Module ${modPath} exports ${symbols.length} symbol(s):`,
        '',
        ...symbols.map((s) => `  ${s}`),
      ].join('\n');

      return {
        content: [{ type: 'text' as const, text: formatted }],
      };
    },
  );
}
