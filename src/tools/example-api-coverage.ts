/**
 * gerbil_example_api_coverage — Check which module exports are used in example/doc files.
 * Similar to test-coverage but generalized to scan any .ss files.
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { runGxi, escapeSchemeString, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';
import { findSymbolOccurrences, scanSchemeFiles } from './parse-utils.js';

const RESULT_MARKER = 'GERBIL-MCP-EXCOV:';

export function registerExampleApiCoverageTool(server: McpServer): void {
  server.registerTool(
    'gerbil_example_api_coverage',
    {
      title: 'Example API Coverage',
      description:
        'Check which module exports are referenced in example/documentation files. ' +
        'Scans .ss files in a directory (or explicit file list) for references to each ' +
        'exported symbol. Reports coverage percentage and per-export usage.',
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
        directory: z
          .string()
          .optional()
          .describe(
            'Directory to scan for .ss files (recursive). Mutually exclusive with files.',
          ),
        files: z
          .array(z.string())
          .optional()
          .describe(
            'Explicit list of .ss file paths to check.',
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
    async ({ module_path, directory, files, loadpath, project_path }) => {
      if (!directory && (!files || files.length === 0)) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Either directory or files must be provided.',
            },
          ],
          isError: true,
        };
      }

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
          content: [{ type: 'text' as const, text: 'Module introspection timed out.' }],
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
            { type: 'text' as const, text: `Module ${modPath} has no exports to check.` },
          ],
        };
      }

      // 2. Find files to scan
      let filePaths: string[] = [];
      if (directory) {
        filePaths = await scanSchemeFiles(directory);
      }
      if (files) {
        filePaths.push(...files);
      }

      if (filePaths.length === 0) {
        return {
          content: [
            { type: 'text' as const, text: 'No .ss files found to scan.' },
          ],
        };
      }

      // 3. Read all files
      const fileContents = new Map<string, string>();
      for (const fp of filePaths) {
        try {
          fileContents.set(fp, await readFile(fp, 'utf-8'));
        } catch {
          // skip unreadable files
        }
      }

      // 4. Check each export
      const covered: Array<{ name: string; files: string[] }> = [];
      const uncovered: string[] = [];

      for (const name of exportNames) {
        const foundIn: string[] = [];
        for (const [fp, content] of fileContents) {
          const occurrences = findSymbolOccurrences(content, name);
          if (occurrences.length > 0) {
            foundIn.push(fp);
          }
        }
        if (foundIn.length > 0) {
          covered.push({ name, files: foundIn });
        } else {
          uncovered.push(name);
        }
      }

      // 5. Format output
      const coveragePercent =
        exportNames.length > 0
          ? Math.round((covered.length / exportNames.length) * 100)
          : 100;

      const sections: string[] = [
        `Module: ${modPath}`,
        `Files scanned: ${fileContents.size}`,
        `Exports: ${exportNames.length}`,
        `Referenced: ${covered.length} (${coveragePercent}%)`,
        `Unreferenced: ${uncovered.length}`,
      ];

      if (uncovered.length > 0) {
        sections.push('');
        sections.push('Unreferenced exports:');
        for (const name of uncovered.sort()) {
          sections.push(`  - ${name}`);
        }
      }

      if (covered.length > 0) {
        sections.push('');
        sections.push('Referenced exports:');
        for (const { name, files: refs } of covered.sort((a, b) => a.name.localeCompare(b.name))) {
          sections.push(`  + ${name} (in ${refs.length} file${refs.length > 1 ? 's' : ''})`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
