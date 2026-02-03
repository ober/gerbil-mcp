import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGerbilCmd } from '../gxi.js';
import { parseGxcErrors, type Diagnostic } from './parse-utils.js';

export function registerBuildAndReportTool(server: McpServer): void {
  server.registerTool(
    'gerbil_build_and_report',
    {
      title: 'Build and Report',
      description:
        'Run `gerbil build` on a project directory and return structured diagnostics. ' +
        'On success, reports a summary. On failure, parses compiler errors into ' +
        'structured file:line:column diagnostics. ' +
        'Uses the modern `gerbil` CLI (not gxpkg).',
      inputSchema: {
        project_path: z
          .string()
          .describe(
            'Directory containing the Gerbil project (with gerbil.pkg)',
          ),
        flags: z
          .array(z.string())
          .optional()
          .describe(
            'Extra build flags: "--release", "--optimized", "--debug"',
          ),
      },
    },
    async ({ project_path, flags }) => {
      const args = ['build', ...(flags ?? [])];

      const result = await runGerbilCmd(args, {
        cwd: project_path,
        timeout: 120_000,
      });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Build timed out after 120 seconds.',
            },
          ],
          isError: true,
        };
      }

      if (result.exitCode === 127) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'gerbil CLI not found. Ensure Gerbil is installed and the gerbil binary is in PATH.',
            },
          ],
          isError: true,
        };
      }

      // Success path
      if (result.exitCode === 0) {
        const output = [result.stdout, result.stderr]
          .filter(Boolean)
          .join('\n')
          .trim();

        const flagStr =
          flags && flags.length > 0 ? ` (${flags.join(', ')})` : '';
        const sections: string[] = [
          `Build succeeded${flagStr}.`,
        ];
        if (output) {
          sections.push('');
          sections.push(output);
        }

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      // Failure path — parse stderr into diagnostics
      const combined = [result.stdout, result.stderr]
        .filter(Boolean)
        .join('\n')
        .trim();

      const diagnostics: Diagnostic[] = parseGxcErrors(
        combined,
        project_path,
      );

      if (diagnostics.length === 0) {
        // Could not parse structured errors — return raw output
        return {
          content: [
            {
              type: 'text' as const,
              text: `Build failed (exit code ${result.exitCode}):\n\n${combined}`,
            },
          ],
          isError: true,
        };
      }

      const errors = diagnostics.filter((d) => d.severity === 'error');
      const warnings = diagnostics.filter((d) => d.severity === 'warning');

      const sections: string[] = [
        `Build failed: ${errors.length} error(s), ${warnings.length} warning(s)`,
        '',
      ];

      for (const d of diagnostics) {
        const loc = d.line
          ? `${d.file}:${d.line}${d.column ? ':' + d.column : ''}`
          : d.file;
        sections.push(`  [${d.severity.toUpperCase()}] ${loc} \u2014 ${d.message}`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: true,
      };
    },
  );
}
