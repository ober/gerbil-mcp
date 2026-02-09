import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-DEF:';

// Keywords that define named top-level forms
const DEF_KEYWORDS = [
  'define',
  'def',
  'defstruct',
  'defclass',
  'definterface',
  'defrule',
  'defsyntax',
  'defsyntax-call',
  'defmethod',
  'defmacro',
  'defalias',
  'deftype',
  'defconst',
  'defvalues',
  'def/c',
];

export function registerLoadFileTool(server: McpServer): void {
  server.registerTool(
    'gerbil_load_file',
    {
      title: 'Analyze Gerbil Source File',
      description:
        'Read a Gerbil source file (.ss, .scm, or .sld) and extract its top-level definitions. ' +
        'Shows imports, exports, and all defined symbols categorized by type (procedure, struct, class, macro, etc.). ' +
        'Does NOT execute the file — only parses it.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .describe(
            'Absolute path to a Gerbil source file (e.g. "/path/to/file.ss")',
          ),
      },
    },
    async ({ file_path }) => {
      // Read file contents via Node
      let contents: string;
      try {
        contents = await readFile(file_path, 'utf-8');
      } catch (err) {
        const msg =
          err instanceof Error ? err.message : 'Unknown error reading file';
        return {
          content: [
            { type: 'text' as const, text: `Failed to read file: ${msg}` },
          ],
          isError: true,
        };
      }

      if (!contents.trim()) {
        return {
          content: [{ type: 'text' as const, text: 'File is empty.' }],
        };
      }

      const escaped = escapeSchemeString(contents);

      // Build a Scheme program that reads all top-level forms and classifies them
      const defKeywordsList = DEF_KEYWORDS.map((k) => `"${k}"`).join(' ');

      const schemeExpr = [
        '(with-catch',
        '  (lambda (e)',
        `    (display "${ERROR_MARKER}\\n")`,
        '    (display-exception e (current-output-port)))',
        '  (lambda ()',
        `    (let* ((port (open-input-string "${escaped}"))`,
        `           (def-keywords (list ${defKeywordsList})))`,
        '      (let loop ((form (read port)))',
        '        (unless (eof-object? form)',
        '          (when (pair? form)',
        '            (let ((head (symbol->string (car form))))',
        '              (cond',
        // import forms
        '                ((string=? head "import")',
        '                 (for-each (lambda (x)',
        `                   (display "${RESULT_MARKER}import\\t")`,
        '                   (write x)',
        '                   (newline))',
        '                   (cdr form)))',
        // export forms
        '                ((string=? head "export")',
        '                 (for-each (lambda (x)',
        `                   (display "${RESULT_MARKER}export\\t")`,
        '                   (write x)',
        '                   (newline))',
        '                   (cdr form)))',
        // definition forms
        '                ((member head def-keywords)',
        '                 (let ((name (cadr form)))',
        `                   (display "${RESULT_MARKER}")`,
        '                   (display head)',
        '                   (display "\\t")',
        '                   (cond',
        '                     ((pair? name) (write (car name)))',
        '                     (else (write name)))',
        '                   (newline))))))',
        '          (loop (read port)))))))',
      ].join(' ');

      const result = await runGxi([schemeExpr]);

      if (result.timedOut) {
        return {
          content: [
            { type: 'text' as const, text: 'File analysis timed out.' },
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
              text: `Error parsing file:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse result lines
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      if (lines.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No top-level definitions found in ${file_path}.`,
            },
          ],
        };
      }

      // Group by category
      const imports: string[] = [];
      const exports: string[] = [];
      const definitions: Array<{ kind: string; name: string }> = [];

      for (const line of lines) {
        const payload = line.slice(RESULT_MARKER.length);
        const tabIdx = payload.indexOf('\t');
        if (tabIdx === -1) continue;
        const kind = payload.slice(0, tabIdx);
        const name = payload.slice(tabIdx + 1).trim();

        if (kind === 'import') {
          imports.push(name);
        } else if (kind === 'export') {
          exports.push(name);
        } else {
          definitions.push({ kind, name });
        }
      }

      const sections: string[] = [`File: ${file_path}`, ''];

      if (imports.length > 0) {
        sections.push(`Imports (${imports.length}):`);
        for (const imp of imports) {
          sections.push(`  ${imp}`);
        }
        sections.push('');
      }

      if (exports.length > 0) {
        sections.push(`Exports (${exports.length}):`);
        for (const exp of exports) {
          sections.push(`  ${exp}`);
        }
        sections.push('');
      }

      if (definitions.length > 0) {
        sections.push(`Definitions (${definitions.length}):`);
        for (const def of definitions) {
          sections.push(`  ${def.name}  (${def.kind})`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
