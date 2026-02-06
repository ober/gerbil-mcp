import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { relative } from 'node:path';
import { runGxi, buildLoadpathEnv, escapeSchemeString } from '../gxi.js';
import {
  scanSchemeFiles,
  extractCallSites,
} from './parse-utils.js';

const ARITY_MARKER = 'GERBIL-MCP-TEST-ARITY:';

export function registerCheckTestArityTool(server: McpServer): void {
  server.registerTool(
    'gerbil_check_test_arity',
    {
      title: 'Check Test Arity',
      description:
        'Detect test files affected by function signature changes. ' +
        'Given a function name and its module, finds all *-test.ss files ' +
        'that call that function and reports whether their call sites match ' +
        'the current arity. Useful after modifying a function signature ' +
        'to quickly find which tests need updating.',
      inputSchema: {
        symbol: z
          .string()
          .describe('Function name to check (e.g. "find-edits-in-line")'),
        module_path: z
          .string()
          .describe(
            'Module that exports the function (e.g. ":myproject/rename"). ' +
            'Used to look up the current arity at runtime.',
          ),
        directory: z
          .string()
          .describe('Directory to scan for *-test.ss files (absolute path)'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for module resolution',
          ),
      },
    },
    async ({ symbol, module_path, directory, loadpath }) => {
      const modPath = module_path.startsWith(':')
        ? module_path
        : `:${module_path}`;

      // Step 1: Find test files (cheap, do first)
      const allFiles = await scanSchemeFiles(directory);
      const testFiles = allFiles.filter((f) => f.endsWith('-test.ss'));

      if (testFiles.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No *-test.ss files found in ${directory}.`,
            },
          ],
        };
      }

      // Step 2: Scan test files for calls to the symbol
      interface TestCallSite {
        file: string;
        line: number;
        column: number;
        argCount: number;
      }

      const callSites: TestCallSite[] = [];

      for (const f of testFiles) {
        let content: string;
        try {
          content = await readFile(f, 'utf-8');
        } catch {
          continue;
        }

        const sites = extractCallSites(content);
        for (const site of sites) {
          if (site.symbol === symbol) {
            callSites.push({
              file: f,
              line: site.line,
              column: site.column,
              argCount: site.argCount,
            });
          }
        }
      }

      if (callSites.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No calls to ${symbol} found in ${testFiles.length} test file(s).`,
            },
          ],
        };
      }

      // Step 3: Look up current arity from the module (only if we found calls)
      const arityResult = await fetchSymbolArity(symbol, modPath, loadpath);
      if (arityResult.error) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Cannot resolve arity for ${symbol} from ${modPath}: ${arityResult.error}`,
            },
          ],
          isError: true,
        };
      }

      const expectedArity = arityResult.arity!;

      // Step 4: Compare and report
      const mismatches: Array<TestCallSite & { expected: string }> = [];

      for (const site of callSites) {
        if (site.argCount < expectedArity.minArity) {
          mismatches.push({
            ...site,
            expected: formatExpected(expectedArity),
          });
        } else if (
          expectedArity.maxArity !== null &&
          site.argCount > expectedArity.maxArity
        ) {
          mismatches.push({
            ...site,
            expected: formatExpected(expectedArity),
          });
        }
      }

      if (mismatches.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `All ${callSites.length} call(s) to ${symbol} in test files match the current arity (${formatExpected(expectedArity)}).`,
            },
          ],
        };
      }

      const sections: string[] = [
        `${mismatches.length} test call site(s) have arity mismatches for ${symbol} (expects ${formatExpected(expectedArity)}):`,
        '',
      ];

      for (const m of mismatches) {
        const relFile = relative(directory, m.file);
        const args = m.argCount === 1 ? 'arg' : 'args';
        sections.push(
          `  ${relFile}:${m.line} — ${symbol} called with ${m.argCount} ${args}, expects ${m.expected}`,
        );
      }

      sections.push('');
      sections.push(
        `${callSites.length} total call(s) found, ${mismatches.length} mismatch(es).`,
      );

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: true,
      };
    },
  );
}

interface ArityLookup {
  arity?: { minArity: number; maxArity: number | null };
  error?: string;
}

/**
 * Look up the arity of a single symbol from a module using runtime introspection.
 */
async function fetchSymbolArity(
  symbol: string,
  modPath: string,
  loadpath?: string[],
): Promise<ArityLookup> {
  // Separate imports and check into different -e args to avoid
  // the gxi output swallowing issue with (import :gerbil/expander)
  const importExpr = `(import :gerbil/expander ${modPath})`;
  const checkExpr = [
    '(with-catch',
    '  (lambda (e)',
    '    (display "ERROR\\t")',
    '    (display-exception e (current-output-port)))',
    '  (lambda ()',
    `    (let ((val (eval (quote ${symbol}))))`,
    '      (if (procedure? val)',
    '        (begin',
    `          (display "${ARITY_MARKER}")`,
    '          (display (##subprocedure-nb-parameters val))',
    '          (newline))',
    `        (begin (display "ERROR\\tnot a procedure") (newline))))))`
  ].join(' ');

  const env = loadpath && loadpath.length > 0 ? buildLoadpathEnv(loadpath) : undefined;
  const result = await runGxi([importExpr, checkExpr], { timeout: 30_000, env });

  const output = result.stdout;

  // Check for errors
  const errorLine = output.split('\n').find((l) => l.startsWith('ERROR\t'));
  if (errorLine) {
    return { error: errorLine.slice(6).trim() };
  }

  // Parse arity
  const arityLine = output.split('\n').find((l) => l.startsWith(ARITY_MARKER));
  if (!arityLine) {
    if (result.exitCode !== 0) {
      const errMsg = result.stderr?.trim() || 'module or symbol not found';
      return { error: errMsg };
    }
    return { error: 'could not determine arity' };
  }

  const paramCount = parseInt(arityLine.slice(ARITY_MARKER.length).trim(), 10);
  if (isNaN(paramCount)) {
    return { error: 'invalid arity response' };
  }

  // Use paramCount as exact expected arity (conservative: allow fewer for optional params)
  return {
    arity: { minArity: 0, maxArity: paramCount },
  };
}

function formatExpected(arity: { minArity: number; maxArity: number | null }): string {
  if (arity.maxArity === null) {
    return `at least ${arity.minArity}`;
  }
  if (arity.minArity === arity.maxArity) {
    return `exactly ${arity.minArity}`;
  }
  return `${arity.minArity}..${arity.maxArity}`;
}
