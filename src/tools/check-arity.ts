import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { relative } from 'node:path';
import { runGxi, buildLoadpathEnv, escapeSchemeString, ERROR_MARKER } from '../gxi.js';
import {
  parseDefinitions,
  scanSchemeFiles,
  extractCallSites,
  extractLocalArities,
  extractModulePaths,
  type ArityInfo,
  type CallSite,
} from './parse-utils.js';

const ARITY_MARKER = 'GERBIL-MCP-ARITY:';

interface ArityIssue {
  file: string;
  line: number;
  column: number;
  symbol: string;
  argCount: number;
  expected: string;
}

export function registerCheckArityTool(server: McpServer): void {
  server.registerTool(
    'gerbil_check_arity',
    {
      title: 'Check Arity',
      description:
        'Project-wide call-site arity checker. Statically extracts function call sites ' +
        'from Gerbil source files, resolves arities from local definitions and imported ' +
        'modules, and reports mismatches where a function is called with the wrong ' +
        'number of arguments.',
      inputSchema: {
        project_path: z
          .string()
          .describe('Project directory to check'),
        file_path: z
          .string()
          .optional()
          .describe('Single file to check instead of the entire project'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for module resolution',
          ),
      },
    },
    async ({ project_path, file_path, loadpath }) => {
      // Determine files to check
      let files: string[];
      if (file_path) {
        files = [file_path];
      } else {
        files = await scanSchemeFiles(project_path);
        if (files.length === 0) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `No .ss files found in ${project_path}.`,
              },
            ],
          };
        }
      }

      // Phase 1: Static extraction
      const allCallSites: Array<CallSite & { file: string }> = [];
      const localArities = new Map<string, ArityInfo>();
      const importedModules = new Set<string>();

      for (const f of files) {
        let content: string;
        try {
          content = await readFile(f, 'utf-8');
        } catch {
          continue;
        }

        // Extract call sites
        const sites = extractCallSites(content);
        for (const site of sites) {
          allCallSites.push({ ...site, file: f });
        }

        // Extract local definitions and their arities
        const analysis = parseDefinitions(content);
        const arities = extractLocalArities(content, analysis.definitions);
        for (const a of arities) {
          localArities.set(a.name, a);
        }

        // Collect imported module paths
        for (const imp of analysis.imports) {
          const paths = extractModulePaths(imp.raw);
          for (const p of paths) {
            importedModules.add(p);
          }
        }
      }

      if (allCallSites.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No function call sites found in ${file_path || project_path}.`,
            },
          ],
        };
      }

      // Phase 2: Runtime arity lookup for imported modules
      const importedArities = new Map<string, ArityInfo>();
      const modulesToCheck = [...importedModules].filter(
        (m) => m.startsWith(':'),
      );

      if (modulesToCheck.length > 0) {
        const runtimeArities = await fetchImportedArities(
          modulesToCheck,
          loadpath,
        );
        for (const [name, info] of runtimeArities) {
          // Don't override local definitions
          if (!localArities.has(name)) {
            importedArities.set(name, info);
          }
        }
      }

      // Phase 3: Compare call sites against known arities
      const issues: ArityIssue[] = [];

      for (const site of allCallSites) {
        const arity =
          localArities.get(site.symbol) || importedArities.get(site.symbol);
        if (!arity) continue; // Unknown function — skip
        if (arity.isMacro) continue; // Skip macros
        if (arity.isCaseLambda && arity.caseArities) {
          // Check against known case arities
          if (!arity.caseArities.includes(site.argCount)) {
            issues.push({
              file: site.file,
              line: site.line,
              column: site.column,
              symbol: site.symbol,
              argCount: site.argCount,
              expected: `one of (${arity.caseArities.join(', ')})`,
            });
          }
          continue;
        }

        if (site.argCount < arity.minArity) {
          issues.push({
            file: site.file,
            line: site.line,
            column: site.column,
            symbol: site.symbol,
            argCount: site.argCount,
            expected: formatExpected(arity),
          });
        } else if (
          arity.maxArity !== null &&
          site.argCount > arity.maxArity
        ) {
          issues.push({
            file: site.file,
            line: site.line,
            column: site.column,
            symbol: site.symbol,
            argCount: site.argCount,
            expected: formatExpected(arity),
          });
        }
      }

      // Format output
      const target = file_path || project_path;
      if (issues.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Arity check: ${target} \u2014 no issues found (${allCallSites.length} call site(s) checked)`,
            },
          ],
        };
      }

      const sections: string[] = [
        `Arity check: ${target} \u2014 ${issues.length} issue(s) found`,
        '',
      ];

      for (const issue of issues) {
        const relFile = file_path
          ? issue.file
          : relative(project_path, issue.file);
        const args = issue.argCount === 1 ? 'arg' : 'args';
        sections.push(
          `  [WARNING] ${relFile}:${issue.line} \u2014 ${issue.symbol} called with ${issue.argCount} ${args}, expects ${issue.expected}`,
        );
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: true,
      };
    },
  );
}

function formatExpected(arity: ArityInfo): string {
  if (arity.maxArity === null) {
    return `at least ${arity.minArity}`;
  }
  if (arity.minArity === arity.maxArity) {
    return `exactly ${arity.minArity}`;
  }
  return `${arity.minArity}..${arity.maxArity}`;
}

/**
 * Fetch arity information for symbols exported by the given modules.
 * Uses a single gxi call to introspect all modules.
 */
async function fetchImportedArities(
  modules: string[],
  loadpath?: string[],
): Promise<Map<string, ArityInfo>> {
  const moduleList = modules.map((m) => `(quote ${m})`).join(' ');

  const expr = [
    '(import :gerbil/expander)',
    '(with-catch',
    '  (lambda (e) (void))',
    '  (lambda ()',
    `    (let ((modules (list ${moduleList})))`,
    '      (for-each',
    '        (lambda (mod-sym)',
    '          (with-catch',
    '            (lambda (e) (void))',
    '            (lambda ()',
    '              (let* ((mod (import-module mod-sym #f #t))',
    '                     (exports (module-context-export mod)))',
    '                (for-each',
    '                  (lambda (e)',
    '                    (let ((name (module-export-name e)))',
    '                      (with-catch',
    '                        (lambda (ex) (void))',
    '                        (lambda ()',
    '                          (let ((val (eval name)))',
    '                            (when (procedure? val)',
    `                              (display "${ARITY_MARKER}")`,
    '                              (display name)',
    '                              (display "\\t")' ,
    '                              (display (##subprocedure-nb-parameters val))',
    '                              (newline)))))))',
    '                  exports)))))',
    '        modules))))',
  ].join(' ');

  const env = loadpath ? buildLoadpathEnv(loadpath) : undefined;
  const result = await runGxi([expr], { timeout: 60_000, env });

  const arities = new Map<string, ArityInfo>();

  for (const line of result.stdout.split('\n')) {
    if (line.startsWith(ARITY_MARKER)) {
      const rest = line.slice(ARITY_MARKER.length);
      const tabIdx = rest.indexOf('\t');
      if (tabIdx !== -1) {
        const name = rest.slice(0, tabIdx);
        const paramCount = parseInt(rest.slice(tabIdx + 1), 10);
        if (!isNaN(paramCount)) {
          // Gambit ##subprocedure-nb-parameters includes the self parameter
          // for closures, but for our purposes we use it as a rough arity.
          // It represents total parameters including optional, so we use it
          // as maxArity and set minArity = 0 as a conservative default.
          // This avoids false positives for functions with optional params.
          arities.set(name, {
            name,
            minArity: 0,
            maxArity: paramCount,
            isMacro: false,
            isCaseLambda: false,
          });
        }
      }
    }
  }

  return arities;
}
