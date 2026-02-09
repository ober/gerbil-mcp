import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxc, runGxi, buildLoadpathEnv, escapeSchemeString, ERROR_MARKER } from '../gxi.js';
import { parseGxcErrors } from './parse-utils.js';
import { STD_MODULES } from './suggest-imports.js';

const FOUND_MARKER = 'GERBIL-MCP-FOUND:';

export function registerResolveImportsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_resolve_imports',
    {
      title: 'Resolve Imports',
      description:
        'Analyze a Gerbil source file for unbound identifiers (via gxc compilation), ' +
        'then search standard library modules for matching exports. ' +
        'Returns a suggested import block and lists unresolved symbols.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .describe('The .ss file to analyze for unbound identifiers'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution',
          ),
      },
    },
    async ({ file_path, loadpath }) => {
      const loadpathEnv = loadpath ? buildLoadpathEnv(loadpath) : undefined;

      // Step 1: Compile the file to get errors
      const compResult = await runGxc(file_path, {
        timeout: 30_000,
        env: loadpathEnv,
      });

      if (compResult.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Compilation timed out after 30 seconds.',
            },
          ],
          isError: true,
        };
      }

      if (compResult.exitCode === 127) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'gxc not found. Ensure Gerbil is installed.',
            },
          ],
          isError: true,
        };
      }

      // If compilation succeeds, no unbound identifiers
      if (compResult.exitCode === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `${file_path} compiles cleanly — no unbound identifiers found.`,
            },
          ],
        };
      }

      // Step 2: Parse errors and extract unbound identifiers
      const combined = [compResult.stdout, compResult.stderr]
        .filter(Boolean)
        .join('\n');

      const unboundSymbols = extractUnboundSymbols(combined);

      if (unboundSymbols.length === 0) {
        // Errors exist but none are unbound identifiers
        const diagnostics = parseGxcErrors(combined, file_path);
        const sections = [
          `${file_path} has compilation errors, but no unbound identifiers detected:`,
          '',
        ];
        for (const d of diagnostics) {
          sections.push(`  [${d.severity.toUpperCase()}] ${d.message}`);
        }
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
          isError: true,
        };
      }

      // Step 3: Search standard library for all unbound symbols at once
      const resolved = await resolveSymbols(unboundSymbols);

      // Step 4: Format output
      const resolvedCount = Object.keys(resolved).filter(
        (s) => resolved[s].length > 0,
      ).length;
      const unresolvedSyms = unboundSymbols.filter(
        (s) => !resolved[s] || resolved[s].length === 0,
      );

      const sections: string[] = [
        `Resolved imports for ${file_path}`,
        '',
        `  ${unboundSymbols.length} unbound identifier(s) found, ${resolvedCount} resolved`,
        '',
      ];

      // Group symbols by module
      const moduleSymbols = new Map<string, string[]>();
      for (const sym of unboundSymbols) {
        const modules = resolved[sym] || [];
        if (modules.length > 0) {
          // Use the first (most common) module
          const mod = modules[0];
          const existing = moduleSymbols.get(mod) || [];
          existing.push(sym);
          moduleSymbols.set(mod, existing);
        }
      }

      if (moduleSymbols.size > 0) {
        sections.push('  Suggested import block:');
        for (const [mod, syms] of moduleSymbols) {
          const comment = syms.join(', ');
          sections.push(`    (import ${mod})    ;; ${comment}`);
        }
      }

      if (unresolvedSyms.length > 0) {
        sections.push('');
        sections.push('  Unresolved (not in standard library):');
        for (const sym of unresolvedSyms) {
          sections.push(`    ${sym}`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

/**
 * Extract unbound identifier names from compiler error output.
 * Handles multiple error formats from gxc/Gambit:
 *   - "Unbound identifier: sym"
 *   - "Unbound variable: sym"
 *   - "Reference to unbound identifier" + "... detail: sym at ..."
 */
function extractUnboundSymbols(output: string): string[] {
  const symbols = new Set<string>();
  const lines = output.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Pattern 1: "Unbound identifier: symbol" or "Unbound variable: symbol"
    const match1 = line.match(/Unbound (?:identifier|variable):\s*(\S+)/i);
    if (match1) {
      symbols.add(match1[1]);
      continue;
    }

    // Pattern 2: "Reference to unbound identifier" (Gambit/gxc format)
    // The actual symbol appears in "... detail: sym at ..." a few lines below
    if (line.includes('Reference to unbound identifier') || line.includes('reference to unbound identifier')) {
      // Search following lines for detail
      for (let j = i + 1; j < lines.length && j < i + 6; j++) {
        const detailMatch = lines[j].match(/\.\.\.\s*detail:\s*(\S+)\s+at\s/);
        if (detailMatch) {
          symbols.add(detailMatch[1]);
          break;
        }
      }
    }
  }

  return [...symbols];
}

/**
 * Search standard library modules for all given symbols in a single gxi call.
 */
async function resolveSymbols(
  symbols: string[],
): Promise<Record<string, string[]>> {
  const moduleList = STD_MODULES.map((m) => `(quote ${m})`).join(' ');
  const symbolList = symbols
    .map((s) => `"${escapeSchemeString(s)}"`)
    .join(' ');

  const expr = [
    '(import :gerbil/expander)',
    '(with-catch',
    '  (lambda (e)',
    `    (display "${ERROR_MARKER}\\n")`,
    '    (display-exception e (current-output-port)))',
    '  (lambda ()',
    `    (let* ((modules (list ${moduleList}))`,
    `           (targets (map string->symbol (list ${symbolList}))))`,
    '      (for-each',
    '        (lambda (mod-sym)',
    '          (with-catch',
    '            (lambda (e) (void))',
    '            (lambda ()',
    '              (let* ((mod (import-module mod-sym #f #t))',
    '                     (exports (module-context-export mod)))',
    '                (for-each',
    '                  (lambda (target)',
    '                    (for-each',
    '                      (lambda (ex)',
    '                        (when (eq? (module-export-name ex) target)',
    `                          (display "${FOUND_MARKER}")`,
    '                          (display target)',
    '                          (display "\\t")' ,
    '                          (display mod-sym)',
    '                          (newline)))',
    '                      exports))',
    '                  targets)))))',
    '        modules))))',
  ].join(' ');

  const result = await runGxi([expr], { timeout: 60_000 });

  const resolved: Record<string, string[]> = {};
  for (const sym of symbols) {
    resolved[sym] = [];
  }

  for (const line of result.stdout.split('\n')) {
    if (line.startsWith(FOUND_MARKER)) {
      const rest = line.slice(FOUND_MARKER.length);
      const tabIdx = rest.indexOf('\t');
      if (tabIdx !== -1) {
        const sym = rest.slice(0, tabIdx);
        const mod = rest.slice(tabIdx + 1).trim();
        if (resolved[sym] && !resolved[sym].includes(mod)) {
          resolved[sym].push(mod);
        }
      }
    }
  }

  return resolved;
}
