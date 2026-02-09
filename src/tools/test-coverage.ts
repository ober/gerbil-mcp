import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, stat } from 'node:fs/promises';
import { join, basename, dirname } from 'node:path';
import { runGxi, escapeSchemeString, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';
import { findSymbolOccurrences } from './parse-utils.js';

const RESULT_MARKER = 'GERBIL-MCP-TCOV:';

export function registerTestCoverageTool(server: McpServer): void {
  server.registerTool(
    'gerbil_test_coverage',
    {
      title: 'Test Coverage Summary',
      description:
        'Compare a module\'s exports against its test file to identify exported symbols ' +
        'that have no corresponding test cases. Scans *-test.ss files for references ' +
        'to each exported symbol and reports which are covered and which are missing.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        module_path: z
          .string()
          .describe(
            'Module to check coverage for (e.g. ":std/text/json", ":myproject/handler")',
          ),
        test_file: z
          .string()
          .optional()
          .describe(
            'Path to the test file. If omitted, auto-discovers by looking for *-test.ss next to the module.',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution',
          ),
        project_path: z
          .string()
          .optional()
          .describe(
            'Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib',
          ),
      },
    },
    async ({ module_path, test_file, loadpath, project_path }) => {
      const modPath = module_path.startsWith(':')
        ? module_path
        : `:${module_path}`;

      // Build loadpath
      const finalLoadpath = project_path
        ? [join(project_path, '.gerbil/lib'), ...(loadpath || [])]
        : loadpath;
      const env = finalLoadpath ? buildLoadpathEnv(finalLoadpath) : undefined;

      // 1. Get module exports
      const exprs = [
        `(import :gerbil/expander)`,
        `(import ${modPath})`,
        [
          '(with-catch',
          '  (lambda (e)',
          `    (display "${ERROR_MARKER}\\n")`,
          '    (display-exception e (current-output-port)))',
          '  (lambda ()',
          `    (let* ((mod (import-module (quote ${modPath}) #f #t))`,
          '           (exports (module-context-export mod)))',
          '      (for-each',
          '        (lambda (e)',
          '          (let ((name (module-export-name e)))',
          `            (display "${RESULT_MARKER}")`,
          '            (display name)',
          '            (newline)))',
          '        exports))))',
        ].join(' '),
      ];

      const result = await runGxi(exprs, { env });

      if (result.timedOut) {
        return {
          content: [
            { type: 'text' as const, text: 'Module introspection timed out.' },
          ],
          isError: true,
        };
      }

      const stdout = result.stdout;
      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1 || result.exitCode !== 0) {
        const errorMsg = errorIdx !== -1
          ? stdout.slice(errorIdx + ERROR_MARKER.length).trim()
          : (result.stderr || result.stdout).trim();
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to load module ${modPath}:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      const exportNames = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER))
        .map((l) => l.slice(RESULT_MARKER.length).trim())
        .filter((n) => n.length > 0);

      if (exportNames.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Module ${modPath} has no exports to check.`,
            },
          ],
        };
      }

      // 2. Find test file
      let testContent = '';
      let testFilePath = test_file || '';

      if (!testFilePath) {
        // Auto-discover: try common patterns
        // :pkg/foo/bar -> look for foo/bar-test.ss in loadpath dirs
        const modSuffix = modPath.replace(/^:[^/]+\//, '');
        const candidates: string[] = [];

        if (project_path) {
          candidates.push(join(project_path, `${modSuffix}-test.ss`));
        }
        if (finalLoadpath) {
          for (const lp of finalLoadpath) {
            candidates.push(join(lp, '..', `${modSuffix}-test.ss`));
          }
        }
        // Also try relative to CWD
        candidates.push(`${modSuffix}-test.ss`);

        for (const cand of candidates) {
          try {
            await stat(cand);
            testFilePath = cand;
            break;
          } catch {
            // continue
          }
        }
      }

      if (testFilePath) {
        try {
          testContent = await readFile(testFilePath, 'utf-8');
        } catch {
          testFilePath = '';
        }
      }

      if (!testContent) {
        const sections: string[] = [
          `Module: ${modPath}`,
          `Exports: ${exportNames.length}`,
          '',
          'No test file found.',
          '',
          `Untested exports (${exportNames.length}):`,
        ];
        for (const name of exportNames.sort()) {
          sections.push(`  - ${name}`);
        }
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      // 3. Check which exports appear in the test file
      const covered: string[] = [];
      const uncovered: string[] = [];

      for (const name of exportNames) {
        const occurrences = findSymbolOccurrences(testContent, name);
        if (occurrences.length > 0) {
          covered.push(name);
        } else {
          uncovered.push(name);
        }
      }

      // 4. Format output
      const coveragePercent =
        exportNames.length > 0
          ? Math.round((covered.length / exportNames.length) * 100)
          : 100;

      const sections: string[] = [
        `Module: ${modPath}`,
        `Test file: ${testFilePath}`,
        `Exports: ${exportNames.length}`,
        `Covered: ${covered.length} (${coveragePercent}%)`,
        `Uncovered: ${uncovered.length}`,
      ];

      if (uncovered.length > 0) {
        sections.push('');
        sections.push('Untested exports:');
        for (const name of uncovered.sort()) {
          sections.push(`  - ${name}`);
        }
      }

      if (covered.length > 0) {
        sections.push('');
        sections.push('Covered exports:');
        for (const name of covered.sort()) {
          sections.push(`  + ${name}`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
