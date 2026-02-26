import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi } from '../gxi.js';

export function registerGambitPrimitiveLookupTool(server: McpServer): void {
  server.registerTool(
    'gerbil_gambit_primitive_lookup',
    {
      title: 'Gambit Primitive Lookup',
      description:
        'Search and inspect Gambit internal primitives in the ## namespace (e.g., ' +
        '##set-gambitdir!, ##shell-command, ##eval-top, ##global-var-set!). ' +
        'These are undocumented C-level or kernel-level primitives that Gerbil code ' +
        'sometimes needs to call directly. Checks if a symbol is bound, its type, ' +
        'and arity. Use the search parameter to find primitives by substring.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        symbol: z
          .string()
          .optional()
          .describe(
            'Exact Gambit primitive name to look up (e.g., "##set-gambitdir!", "##shell-command")',
          ),
        search: z
          .string()
          .optional()
          .describe(
            'Search term to find matching ## primitives by substring (e.g., "gambitdir", "process")',
          ),
      },
    },
    async ({ symbol, search }) => {
      if (!symbol && !search) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Either "symbol" or "search" parameter is required.',
            },
          ],
          isError: true,
        };
      }

      if (symbol) {
        // Direct lookup of a specific primitive
        const sym = symbol.startsWith('##') ? symbol : `##${symbol}`;
        // Use a single expression to avoid cross-phase issues with ## primitives
        const code = [
          `(with-catch`,
          `  (lambda (e) (display "BOUND:false") (newline))`,
          `  (lambda ()`,
          `    (let ((val ${sym}))`,
          `      (display "BOUND:true") (newline)`,
          `      (display "TYPE:")`,
          `      (displayln (cond ((procedure? val) "procedure") ((boolean? val) "boolean") ((number? val) "number") ((string? val) "string") ((pair? val) "pair") ((vector? val) "vector") ((null? val) "null") ((symbol? val) "symbol") ((char? val) "char") (else "other")))`,
          `      (when (procedure? val)`,
          `        (with-catch (lambda (e2) (void))`,
          `          (lambda ()`,
          `            (let ((np (##subprocedure-nb-parameters val)))`,
          `              (display "ARITY:") (displayln np))))))))`,
        ].join(' ');
        const exprs = [code];

        const result = await runGxi(exprs, { timeout: 10_000 });

        if (result.timedOut) {
          return {
            content: [
              { type: 'text' as const, text: `Lookup timed out for ${sym}` },
            ],
            isError: true,
          };
        }

        const lines = result.stdout.split('\n');
        let bound = false;
        let type = '';
        let arity = '';

        for (const line of lines) {
          const trimmed = line.trim();
          if (trimmed === 'BOUND:true') bound = true;
          if (trimmed.startsWith('TYPE:')) type = trimmed.slice('TYPE:'.length);
          if (trimmed.startsWith('ARITY:')) arity = trimmed.slice('ARITY:'.length);
        }

        if (!bound) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Gambit primitive "${sym}" is not bound in the current environment.`,
              },
            ],
          };
        }

        const sections: string[] = [
          `Gambit primitive: ${sym}`,
          `  Type: ${type}`,
        ];

        if (arity) {
          const arityNum = parseInt(arity, 10);
          if (arityNum >= 0) {
            sections.push(`  Arity: ${arityNum} parameter(s)`);
          } else if (arityNum === -1) {
            sections.push('  Arity: unknown (C primitive)');
          } else {
            sections.push(
              `  Arity: ${Math.abs(arityNum) - 1}+ (variadic, ${Math.abs(arityNum) - 1} required + rest)`,
            );
          }
        }

        // Add common notes for well-known primitives
        const notes = getKnownPrimitiveNotes(sym);
        if (notes) {
          sections.push('');
          sections.push('Notes:');
          sections.push(`  ${notes}`);
        }

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      // Search mode: find matching ## primitives
      const searchTerm = search!;
      const exprs = [
        '(import :gerbil/expander)',
        [
          '(let ((results \'()))',
          '  (##for-each-symkey',
          '    (lambda (sym)',
          '      (let ((name (symbol->string sym)))',
          `        (when (and (string-contains name "##")`,
          `                   (string-contains name "${searchTerm.replace(/"/g, '\\"')}"))`,
          '          (display "MATCH:") (displayln name)))))',
          '  (void))',
        ].join(' '),
      ];

      const result = await runGxi(exprs, { timeout: 15_000 });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Search timed out for "${searchTerm}"`,
            },
          ],
          isError: true,
        };
      }

      const matches: string[] = [];
      for (const line of result.stdout.split('\n')) {
        const trimmed = line.trim();
        if (trimmed.startsWith('MATCH:')) {
          matches.push(trimmed.slice('MATCH:'.length));
        }
      }

      if (matches.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No Gambit primitives found matching "${searchTerm}".`,
            },
          ],
        };
      }

      matches.sort();
      const sections: string[] = [
        `Found ${matches.length} Gambit primitive(s) matching "${searchTerm}":`,
        '',
        ...matches.slice(0, 50).map((m) => `  ${m}`),
      ];

      if (matches.length > 50) {
        sections.push(`  ... and ${matches.length - 50} more`);
      }

      sections.push('');
      sections.push(
        'Use gerbil_gambit_primitive_lookup with symbol: "##name" to inspect a specific primitive.',
      );

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

/**
 * Return known notes about well-known Gambit primitives.
 */
function getKnownPrimitiveNotes(sym: string): string | null {
  const notes: Record<string, string> = {
    '##set-gambitdir!':
      'Takes a UCS-2STRING (not char*). Use (##set-gambitdir! (path-expand "~~")) or the C-level ___set_gambitdir which expects UCS-2STRING type.',
    '##shell-command':
      'Low-level shell command execution. Returns exit code. Prefer :std/misc/process run-process for safer alternatives.',
    '##eval-top':
      'Evaluates a form in the top-level environment. Used internally by the REPL.',
    '##global-var-set!':
      'Set a global variable by its interned symbol. Takes (symbol, value).',
    '##make-global-var':
      'Create a new global variable binding.',
    '##interaction-cte':
      'The compile-time environment for interactive REPL evaluation.',
    '##subprocedure-nb-parameters':
      'Returns arity: positive = fixed, -1 = C primitive, negative = -(required+1) for variadic.',
    '##current-directory':
      'Get/set the current working directory. Gambit-specific alternative to current-directory.',
    '##process-status':
      'Get process status. May hang if SIGCHLD is blocked by signalfd.',
    '##open-process':
      'Low-level process creation. Returns a port for I/O.',
  };
  return notes[sym] ?? null;
}
