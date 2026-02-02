import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { runGxi, escapeSchemeString } from '../gxi.js';

const FORM_MARKER = 'GERBIL-MCP-FORM:';
const ERROR_MARKER = 'GERBIL-MCP-READ-ERROR:';

interface FormInfo {
  index: number;
  startLine: number;
  endLine: number;
  summary: string;
}

function buildReadFormsExpr(escaped: string): string {
  // Scheme code that reads all forms from a string port, tracking line numbers.
  // For each form, outputs: MARKER index \t startLine \t endLine \t summary
  return [
    '(let* ((src "' + escaped + '")',
    '       (port (open-input-string src))',
    '       (idx 0))',
    '  (let loop ()',
    '    (let ((start-line (input-port-line port)))',
    '      (with-catch',
    '        (lambda (e)',
    '          (let ((eline (input-port-line port))',
    '                (ecol (input-port-column port)))',
    '            (display "' + ERROR_MARKER + '")',
    '            (display eline) (display "\\t")',
    '            (display ecol) (display "\\t")',
    '            (display-exception e (current-output-port))',
    '            (newline)))',
    '        (lambda ()',
    '          (let ((form (read port)))',
    '            (if (eof-object? form)',
    '              (void)',
    '              (let ((end-line (input-port-line port))',
    '                    (summary (cond',
    '                               ((pair? form)',
    '                                (let ((hd (car form)))',
    '                                  (if (symbol? hd)',
    '                                    (symbol->string hd)',
    '                                    "(...)")))',
    '                               ((symbol? form) (symbol->string form))',
    '                               ((string? form) "\\"...\\"" )',
    '                               ((number? form) (number->string form))',
    '                               ((boolean? form) (if form "#t" "#f"))',
    '                               (else (object->string form)))))',
    '                (display "' + FORM_MARKER + '")',
    '                (display idx) (display "\\t")',
    '                (display start-line) (display "\\t")',
    '                (display end-line) (display "\\t")',
    '                (displayln summary)',
    '                (set! idx (+ idx 1))',
    '                (loop)))))))))',
  ].join(' ');
}

function parseFormLines(output: string): { forms: FormInfo[]; error: string | null } {
  const forms: FormInfo[] = [];
  let error: string | null = null;

  for (const line of output.split('\n')) {
    if (line.startsWith(FORM_MARKER)) {
      const rest = line.slice(FORM_MARKER.length);
      const parts = rest.split('\t');
      if (parts.length >= 4) {
        forms.push({
          index: parseInt(parts[0], 10),
          startLine: parseInt(parts[1], 10),
          endLine: parseInt(parts[2], 10),
          summary: parts[3],
        });
      }
    } else if (line.startsWith(ERROR_MARKER)) {
      const rest = line.slice(ERROR_MARKER.length);
      const parts = rest.split('\t');
      if (parts.length >= 3) {
        error = `Reader error at line ${parts[0]}, col ${parts[1]}: ${parts.slice(2).join('\t').trim()}`;
      } else {
        error = `Reader error: ${rest.trim()}`;
      }
    }
  }

  return { forms, error };
}

export function registerReadFormsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_read_forms',
    {
      title: 'Read Top-Level Forms',
      description:
        'Read a Gerbil source file using the actual Gerbil reader and list all top-level forms ' +
        'with their index, start/end line numbers, and a summary (car of list or type). ' +
        'On reader error, reports the error position plus any forms read before the error.',
      inputSchema: {
        file_path: z
          .string()
          .describe('Path to a Gerbil source file to read'),
      },
    },
    async ({ file_path }) => {
      let source: string;
      try {
        source = await readFile(file_path, 'utf-8');
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [{ type: 'text' as const, text: `Failed to read file: ${msg}` }],
          isError: true,
        };
      }

      if (!source.trim()) {
        return {
          content: [{ type: 'text' as const, text: 'File is empty — no forms to read.' }],
        };
      }

      const escaped = escapeSchemeString(source);
      const expr = buildReadFormsExpr(escaped);
      const result = await runGxi([expr]);

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Reader timed out.' }],
          isError: true,
        };
      }

      const { forms, error } = parseFormLines(result.stdout);

      const sections: string[] = [`File: ${file_path}`, ''];

      if (forms.length > 0) {
        sections.push(`Forms (${forms.length}):`);
        for (const f of forms) {
          const lineRange =
            f.startLine === f.endLine
              ? `L${f.startLine}`
              : `L${f.startLine}-${f.endLine}`;
          sections.push(`  [${f.index}] ${lineRange}: (${f.summary} ...)`);
        }
      }

      if (error) {
        sections.push('');
        sections.push(error);
      }

      if (forms.length === 0 && !error) {
        sections.push('No forms found.');
      }

      const hasError = !!error;
      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: hasError,
      };
    },
  );
}
