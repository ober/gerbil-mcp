import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { runGxi, ERROR_MARKER } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-VER:';

export function registerVersionTool(server: McpServer): void {
  server.registerTool(
    'gerbil_version',
    {
      title: 'Gerbil Environment Info',
      description:
        'Report Gerbil and Gambit versions, installation path, and system type. ' +
        'Useful for diagnostics and ensuring compatibility.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {},
    },
    async () => {
      const expr = [
        '(with-catch',
        '  (lambda (e)',
        `    (display "${ERROR_MARKER}\\n")`,
        '    (display-exception e (current-output-port)))',
        '  (lambda ()',
        `    (display "${RESULT_MARKER}gerbil-version\\t")`,
        '    (display (gerbil-version-string))',
        '    (newline)',
        `    (display "${RESULT_MARKER}gambit-version\\t")`,
        '    (display (system-version-string))',
        '    (newline)',
        `    (display "${RESULT_MARKER}gerbil-home\\t")`,
        '    (display (path-normalize (gerbil-home)))',
        '    (newline)',
        `    (display "${RESULT_MARKER}system-type\\t")`,
        '    (write (system-type))',
        '    (newline)))',
      ].join(' ');

      const result = await runGxi([expr]);

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Version check timed out.',
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
              text: 'gxi not found. Ensure Gerbil is installed and gxi is in PATH.',
            },
          ],
          isError: true,
        };
      }

      const stdout = result.stdout;
      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        return {
          content: [
            {
              type: 'text' as const,
              text: `Error retrieving version info:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse result lines
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      const info: Record<string, string> = {};
      for (const line of lines) {
        const payload = line.slice(RESULT_MARKER.length);
        const tabIdx = payload.indexOf('\t');
        if (tabIdx !== -1) {
          info[payload.slice(0, tabIdx)] = payload.slice(tabIdx + 1).trim();
        }
      }

      if (Object.keys(info).length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'No version information collected.',
            },
          ],
          isError: true,
        };
      }

      const sections: string[] = ['Gerbil Environment', ''];
      if (info['gerbil-version']) {
        sections.push(`Gerbil version: ${info['gerbil-version']}`);
      }
      if (info['gambit-version']) {
        sections.push(`Gambit version: ${info['gambit-version']}`);
      }
      if (info['gerbil-home']) {
        sections.push(`Gerbil home: ${info['gerbil-home']}`);
      }
      if (info['system-type']) {
        sections.push(`System type: ${info['system-type']}`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
