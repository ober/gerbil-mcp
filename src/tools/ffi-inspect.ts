import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-FFI:';

export function registerFfiInspectTool(server: McpServer): void {
  server.registerTool(
    'gerbil_ffi_inspect',
    {
      title: 'Inspect FFI Bindings',
      description:
        'Inspect a Gerbil module\'s FFI (Foreign Function Interface) bindings. ' +
        'Classifies exports as C constants (UPPERCASE), C-style functions (underscore_names), ' +
        'or Gerbil wrappers. Shows values for constants and arity for functions. ' +
        'Optionally provide a source file_path to also extract begin-foreign and extern declarations.',
      inputSchema: {
        module_path: z
          .string()
          .describe('Module path to inspect (e.g. ":std/os/signal")'),
        file_path: z
          .string()
          .optional()
          .describe(
            'Source file for deeper analysis of begin-foreign/extern blocks',
          ),
      },
    },
    async ({ module_path, file_path }) => {
      const modPath = module_path.startsWith(':')
        ? module_path
        : `:${module_path}`;

      // Phase 1: Runtime inspection — classify all exports
      const exprs = [
        `(import :gerbil/expander)`,
        `(import ${modPath})`,
        buildInspectExpr(modPath),
      ];

      const result = await runGxi(exprs);

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'FFI inspection timed out.',
            },
          ],
          isError: true,
        };
      }

      if (result.exitCode !== 0 && result.stderr) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to load module ${modPath}:\n${result.stderr.trim()}`,
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
              text: `Error inspecting ${modPath}:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse result lines: MARKER name \t kind \t detail
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      const constants: Array<{ name: string; value: string }> = [];
      const cFunctions: Array<{ name: string; arity: string }> = [];
      const wrappers: Array<{ name: string; detail: string }> = [];

      for (const line of lines) {
        const payload = line.slice(RESULT_MARKER.length);
        const parts = payload.split('\t');
        const name = parts[0] || '';
        const kind = parts[1] || '';
        const detail = parts[2] || '';

        if (kind === 'constant') {
          constants.push({ name, value: detail });
        } else if (kind === 'c-function') {
          cFunctions.push({ name, arity: detail });
        } else {
          wrappers.push({ name, detail });
        }
      }

      // Build output
      const sections: string[] = [
        `FFI inspection of ${modPath}:`,
        '',
      ];

      if (constants.length > 0) {
        sections.push(`C Constants (${constants.length}):`);
        for (const c of constants) {
          sections.push(`  ${c.name} = ${c.value}`);
        }
        sections.push('');
      }

      if (cFunctions.length > 0) {
        sections.push(`C-style Functions (${cFunctions.length}):`);
        for (const f of cFunctions) {
          sections.push(`  ${f.name}  arity:${f.arity}`);
        }
        sections.push('');
      }

      if (wrappers.length > 0) {
        sections.push(`Gerbil Wrappers (${wrappers.length}):`);
        for (const w of wrappers) {
          sections.push(`  ${w.name}  ${w.detail}`);
        }
        sections.push('');
      }

      if (constants.length === 0 && cFunctions.length === 0 && wrappers.length === 0) {
        sections.push('No exports found.');
      }

      // Phase 2: Source analysis if file_path provided
      if (file_path) {
        try {
          const source = await readFile(file_path, 'utf-8');
          const foreignSections = extractForeignBlocks(source);
          if (foreignSections.length > 0) {
            sections.push('Foreign Declarations (from source):');
            for (const block of foreignSections) {
              sections.push(`  ${block}`);
            }
          }
        } catch {
          sections.push(`(Could not read source file: ${file_path})`);
        }
      }

      return {
        content: [
          { type: 'text' as const, text: sections.join('\n') },
        ],
      };
    },
  );
}

function buildInspectExpr(modPath: string): string {
  // Classify each export based on naming convention and runtime type
  return [
    '(with-catch',
    '  (lambda (e)',
    `    (display "${ERROR_MARKER}\\n")`,
    '    (display-exception e (current-output-port)))',
    '  (lambda ()',
    `    (let* ((mod (import-module (quote ${modPath}) #f #t))`,
    '           (exports (module-context-export mod)))',
    '      (for-each',
    '        (lambda (e)',
    '          (let* ((name (module-export-name e))',
    '                 (name-str (symbol->string name)))',
    '            (with-catch',
    '              (lambda (ex)',
    `                (display "${RESULT_MARKER}")`,
    '                (display name)',
    '                (display "\\twrapper\\tmacro/syntax")',
    '                (newline))',
    '              (lambda ()',
    '                (let ((val (eval name)))',
    '                  (cond',
    // UPPERCASE names → C constants
    '                    ((let check-upper ((chars (string->list name-str)))',
    '                       (cond ((null? chars) #t)',
    '                             ((or (char-upper-case? (car chars))',
    '                                  (char=? (car chars) #\\_ )',
    '                                  (char-numeric? (car chars)))',
    '                              (check-upper (cdr chars)))',
    '                             (else #f)))',
    `                     (display "${RESULT_MARKER}")`,
    '                     (display name)',
    '                     (display "\\tconstant\\t")' ,
    '                     (write val)',
    '                     (newline))',
    // underscore-containing lowercase → C-style functions
    '                    ((and (procedure? val)',
    '                          (string-index name-str #\\_))',
    `                     (display "${RESULT_MARKER}")`,
    '                     (display name)',
    '                     (display "\\tc-function\\t")' ,
    '                     (display (##subprocedure-nb-parameters val))',
    '                     (newline))',
    // Regular procedures
    '                    ((procedure? val)',
    `                     (display "${RESULT_MARKER}")`,
    '                     (display name)',
    '                     (display "\\twrapper\\tprocedure arity:")' ,
    '                     (display (##subprocedure-nb-parameters val))',
    '                     (newline))',
    // Other values
    '                    (else',
    `                     (display "${RESULT_MARKER}")`,
    '                     (display name)',
    '                     (display "\\twrapper\\t")' ,
    '                     (display (type-of val))',
    '                     (newline))))))))',
    '        exports))))',
  ].join(' ');
}

function extractForeignBlocks(source: string): string[] {
  const results: string[] = [];
  const lines = source.split('\n');

  let inForeign = false;
  let depth = 0;
  let foreignBlock: string[] = [];

  for (const line of lines) {
    const trimmed = line.trim();

    if (trimmed.startsWith('(begin-foreign')) {
      inForeign = true;
      depth = 0;
      foreignBlock = [];
    }

    if (inForeign) {
      foreignBlock.push(trimmed);
      for (const ch of trimmed) {
        if (ch === '(') depth++;
        if (ch === ')') depth--;
      }
      if (depth <= 0) {
        results.push(foreignBlock.join(' '));
        inForeign = false;
      }
    }

    // Also capture standalone extern declarations
    if (trimmed.startsWith('(extern ') && !inForeign) {
      results.push(trimmed);
    }
  }

  return results;
}
