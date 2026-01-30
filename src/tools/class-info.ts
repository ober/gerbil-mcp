import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-CLASS:';

export function registerClassInfoTool(server: McpServer): void {
  server.registerTool(
    'gerbil_class_info',
    {
      title: 'Inspect Class/Struct Types',
      description:
        'Inspect a Gerbil defclass/defstruct type descriptor. Shows type name, slots, ' +
        'own fields, struct vs class, super type, precedence list (MRO), and constructor. ' +
        'The type_name should be the base name (e.g. "Error", "JSON") — the tool appends ::t automatically.',
      inputSchema: {
        type_name: z
          .string()
          .describe(
            'Type name without ::t suffix (e.g. "Error", "JSON", "point")',
          ),
        module_path: z
          .string()
          .optional()
          .describe(
            'Module to import to bring the type in scope (e.g. ":std/error")',
          ),
      },
    },
    async ({ type_name, module_path }) => {
      const importExpr = module_path
        ? `(import ${module_path.startsWith(':') ? module_path : `:${module_path}`})`
        : '';

      const typeSym = `${type_name}::t`;

      const exprs = [
        ...(importExpr ? [importExpr] : []),
        buildInspectExpr(typeSym),
      ];

      const result = await runGxi(exprs);

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Class inspection timed out.',
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
              text: `Failed to inspect type ${typeSym}:\n${result.stderr.trim()}`,
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
              text: `Error inspecting ${typeSym}:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse result lines: MARKER key \t value
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      const info: Record<string, string> = {};
      for (const line of lines) {
        const payload = line.slice(RESULT_MARKER.length);
        const tabIdx = payload.indexOf('\t');
        if (tabIdx !== -1) {
          const key = payload.slice(0, tabIdx);
          const value = payload.slice(tabIdx + 1);
          info[key] = value;
        }
      }

      if (Object.keys(info).length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `${typeSym} is not a class/struct type descriptor, or was not found.`,
            },
          ],
          isError: true,
        };
      }

      // Build formatted output
      const isStruct = info['struct'] === '#t';
      const sections: string[] = [
        `Type: ${info['name'] || type_name} (${isStruct ? 'struct' : 'class'})`,
        '',
      ];

      if (info['slots']) {
        sections.push(`Slots: ${info['slots']}`);
      }

      if (info['own-fields']) {
        sections.push(`Own fields: ${info['own-fields']}`);
      }

      if (info['super']) {
        sections.push(`Super: ${info['super']}`);
      }

      if (info['precedence']) {
        sections.push(`Precedence: ${info['precedence']}`);
      }

      if (info['constructor']) {
        sections.push(`Constructor: ${info['constructor']}`);
      }

      return {
        content: [
          { type: 'text' as const, text: sections.join('\n') },
        ],
      };
    },
  );
}

function buildInspectExpr(typeSym: string): string {
  return [
    '(with-catch',
    '  (lambda (e)',
    `    (display "${ERROR_MARKER}\\n")`,
    '    (display-exception e (current-output-port)))',
    '  (lambda ()',
    `    (let ((t (eval (quote ${typeSym}))))`,
    '      (if (not (class-type? t))',
    '        (begin',
    `          (display "${ERROR_MARKER}\\n")`,
    `          (display "${escapeSchemeString(typeSym)} is not a class-type descriptor"))`,
    '        (begin',
    // name
    `          (display "${RESULT_MARKER}name\\t")`,
    '          (display (class-type-name t))',
    '          (newline)',
    // struct?
    `          (display "${RESULT_MARKER}struct\\t")`,
    '          (display (class-type-struct? t))',
    '          (newline)',
    // slots
    `          (display "${RESULT_MARKER}slots\\t")`,
    '          (let ((slots (class-type-slot-list t)))',
    '            (if (null? slots)',
    '              (display "(none)")',
    '              (for-each (lambda (s) (display s) (display " ")) slots)))',
    '          (newline)',
    // own fields — extract names from the fields vector (every 3rd element starting at 0)
    `          (display "${RESULT_MARKER}own-fields\\t")`,
    '          (let ((fv (class-type-fields t)))',
    '            (if (= (vector-length fv) 0)',
    '              (display "(none)")',
    '              (let loop ((i 0))',
    '                (when (< i (vector-length fv))',
    '                  (display (vector-ref fv i))',
    '                  (display " ")',
    '                  (loop (+ i 3))))))',
    '          (newline)',
    // super
    `          (display "${RESULT_MARKER}super\\t")`,
    '          (let ((s (class-type-super t)))',
    '            (cond',
    '              ((not s) (display "(none)"))',
    '              ((class-type? s) (display (class-type-name s)))',
    '              ((list? s)',
    '               (for-each (lambda (p)',
    '                 (if (class-type? p)',
    '                   (begin (display (class-type-name p)) (display " "))',
    '                   (begin (display p) (display " "))))',
    '                 s))',
    '              (else (display s))))',
    '          (newline)',
    // precedence list
    `          (display "${RESULT_MARKER}precedence\\t")`,
    '          (let ((pl (class-type-precedence-list t)))',
    '            (let loop ((rest pl))',
    '              (when (pair? rest)',
    '                (let ((p (car rest)))',
    '                  (if (class-type? p)',
    '                    (display (class-type-name p))',
    '                    (display p))',
    '                  (when (pair? (cdr rest))',
    '                    (display " -> "))',
    '                  (loop (cdr rest))))))',
    '          (newline)',
    // constructor
    `          (display "${RESULT_MARKER}constructor\\t")`,
    '          (let ((c (class-type-constructor t)))',
    '            (if c (display c) (display "(none)")))',
    '          (newline)',
    '        )))))',
  ].join(' ');
}
