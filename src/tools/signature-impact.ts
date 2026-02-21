import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { relative } from 'node:path';
import { runGxi, buildLoadpathEnv } from '../gxi.js';
import {
  scanSchemeFiles,
  extractCallSites,
  findSymbolOccurrences,
} from './parse-utils.js';

const ARITY_MARKER = 'GERBIL-MCP-SIG-IMPACT:';

interface CallSiteInfo {
  file: string;
  line: number;
  column: number;
  argCount: number;
  isTestFile: boolean;
}

export function registerSignatureImpactTool(server: McpServer): void {
  server.registerTool(
    'gerbil_signature_impact',
    {
      title: 'Pre-Signature-Change Impact Report',
      description:
        'Find all call sites for a function before changing its signature. ' +
        'Combines find_callers + check_test_arity + check_arity in a single call. ' +
        'Shows all source and test files that reference the function, with arity ' +
        'mismatch detection against the current (or proposed new) arity. ' +
        'Use before modifying a function to know exactly which call sites need updating.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        symbol: z
          .string()
          .describe('Function name to check (e.g. "classify-symbol-token")'),
        directory: z
          .string()
          .describe(
            'Project directory to scan for .ss files (absolute path)',
          ),
        module_path: z
          .string()
          .optional()
          .describe(
            'Module that exports the function (e.g. ":myproject/rename"). ' +
            'Used to look up the current arity at runtime. If omitted, ' +
            'only call sites are reported without arity checking.',
          ),
        new_arity: z
          .number()
          .optional()
          .describe(
            'Proposed new arity (number of parameters). When provided, ' +
            'reports which call sites would break with the new arity. ' +
            'If omitted, checks against the current runtime arity.',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for module resolution',
          ),
      },
    },
    async ({ symbol, directory, module_path, new_arity, loadpath }) => {
      // Step 1: Scan all .ss files in the directory
      const allFiles = await scanSchemeFiles(directory);

      if (allFiles.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No .ss files found in ${directory}.`,
            },
          ],
        };
      }

      // Step 2: Find all call sites and references across all files
      const callSites: CallSiteInfo[] = [];
      const referenceFiles: Map<string, number[]> = new Map();

      for (const f of allFiles) {
        let content: string;
        try {
          content = await readFile(f, 'utf-8');
        } catch {
          continue;
        }

        const isTestFile = f.endsWith('-test.ss');

        // Extract structured call sites with arity info
        const sites = extractCallSites(content);
        for (const site of sites) {
          if (site.symbol === symbol) {
            callSites.push({
              file: f,
              line: site.line,
              column: site.column,
              argCount: site.argCount,
              isTestFile,
            });
          }
        }

        // Also find bare references (not just call sites)
        const occurrences = findSymbolOccurrences(content, symbol);
        if (occurrences.length > 0) {
          referenceFiles.set(
            f,
            occurrences.map((o) => o.line),
          );
        }
      }

      if (callSites.length === 0 && referenceFiles.size === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No references to "${symbol}" found in ${allFiles.length} file(s).`,
            },
          ],
        };
      }

      // Step 3: Determine arity to check against
      let checkArity: { min: number; max: number | null } | null = null;
      let aritySource = '';

      if (new_arity !== undefined) {
        // User specified the proposed new arity
        checkArity = { min: new_arity, max: new_arity };
        aritySource = `proposed new arity: ${new_arity}`;
      } else if (module_path) {
        // Look up current arity from module
        const modPath = module_path.startsWith(':')
          ? module_path
          : `:${module_path}`;
        const arityResult = await fetchSymbolArity(
          symbol,
          modPath,
          loadpath,
        );
        if (arityResult.arity) {
          checkArity = {
            min: arityResult.arity.minArity,
            max: arityResult.arity.maxArity,
          };
          aritySource = `current arity from ${modPath}: ${formatArity(checkArity)}`;
        } else {
          aritySource = `arity lookup failed: ${arityResult.error}`;
        }
      }

      // Step 4: Build report
      const sections: string[] = [];
      sections.push(
        `Impact report for "${symbol}" — ${callSites.length} call site(s) in ${referenceFiles.size} file(s)`,
      );
      if (aritySource) {
        sections.push(`Arity: ${aritySource}`);
      }
      sections.push('');

      // Separate test and source call sites
      const testSites = callSites.filter((s) => s.isTestFile);
      const sourceSites = callSites.filter((s) => !s.isTestFile);

      // Report source call sites
      if (sourceSites.length > 0) {
        sections.push(`Source call sites (${sourceSites.length}):`);
        for (const site of sourceSites) {
          const relFile = relative(directory, site.file);
          const arityFlag = checkArity
            ? getArityFlag(site.argCount, checkArity)
            : '';
          sections.push(
            `  ${relFile}:${site.line} — ${site.argCount} arg(s)${arityFlag}`,
          );
        }
        sections.push('');
      }

      // Report test call sites
      if (testSites.length > 0) {
        sections.push(`Test call sites (${testSites.length}):`);
        for (const site of testSites) {
          const relFile = relative(directory, site.file);
          const arityFlag = checkArity
            ? getArityFlag(site.argCount, checkArity)
            : '';
          sections.push(
            `  ${relFile}:${site.line} — ${site.argCount} arg(s)${arityFlag}`,
          );
        }
        sections.push('');
      }

      // Summary
      const mismatches = checkArity
        ? callSites.filter(
            (s) =>
              s.argCount < checkArity!.min ||
              (checkArity!.max !== null && s.argCount > checkArity!.max),
          )
        : [];

      if (checkArity && mismatches.length > 0) {
        sections.push(
          `BREAKING: ${mismatches.length} of ${callSites.length} call site(s) would break (need ${formatArity(checkArity)})`,
        );
      } else if (checkArity && mismatches.length === 0) {
        sections.push(
          `OK: All ${callSites.length} call site(s) are compatible with ${formatArity(checkArity)}`,
        );
      } else {
        sections.push(`Total: ${callSites.length} call site(s) found`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: mismatches.length > 0,
      };
    },
  );
}

function getArityFlag(
  argCount: number,
  arity: { min: number; max: number | null },
): string {
  if (argCount < arity.min) {
    return ` [BREAK: too few, needs ${formatArity(arity)}]`;
  }
  if (arity.max !== null && argCount > arity.max) {
    return ` [BREAK: too many, needs ${formatArity(arity)}]`;
  }
  return ' [OK]';
}

function formatArity(arity: { min: number; max: number | null }): string {
  if (arity.max === null) return `at least ${arity.min}`;
  if (arity.min === arity.max) return `exactly ${arity.min}`;
  return `${arity.min}..${arity.max}`;
}

interface ArityLookup {
  arity?: { minArity: number; maxArity: number | null };
  error?: string;
}

async function fetchSymbolArity(
  symbol: string,
  modPath: string,
  loadpath?: string[],
): Promise<ArityLookup> {
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
    '        (begin (display "ERROR\\tnot a procedure") (newline))))))',
  ].join(' ');

  const env =
    loadpath && loadpath.length > 0 ? buildLoadpathEnv(loadpath) : undefined;
  const result = await runGxi([importExpr, checkExpr], {
    timeout: 30_000,
    env,
  });

  const output = result.stdout;

  const errorLine = output.split('\n').find((l) => l.startsWith('ERROR\t'));
  if (errorLine) {
    return { error: errorLine.slice(6).trim() };
  }

  const arityLine = output.split('\n').find((l) => l.startsWith(ARITY_MARKER));
  if (!arityLine) {
    if (result.exitCode !== 0) {
      const errMsg = result.stderr?.trim() || 'module or symbol not found';
      return { error: errMsg };
    }
    return { error: 'could not determine arity' };
  }

  const paramCount = parseInt(
    arityLine.slice(ARITY_MARKER.length).trim(),
    10,
  );
  if (isNaN(paramCount)) {
    return { error: 'invalid arity response' };
  }

  return {
    arity: { minArity: 0, maxArity: paramCount },
  };
}
