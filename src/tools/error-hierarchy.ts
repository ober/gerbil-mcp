import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, ERROR_MARKER } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-HIER:';

export function registerErrorHierarchyTool(server: McpServer): void {
  server.registerTool(
    'gerbil_error_hierarchy',
    {
      title: 'Error Type Hierarchy',
      description:
        'Show the full Gerbil exception/error class hierarchy as a tree. ' +
        'Displays all known error types from :std/error and their inheritance relationships. ' +
        'Optionally import additional modules to include their error types.',
      inputSchema: {
        modules: z
          .array(z.string())
          .optional()
          .describe(
            'Additional modules to import for scanning error types beyond :std/error (e.g. [":std/os/error"])',
          ),
      },
    },
    async ({ modules }) => {
      const exprs: string[] = ['(import :std/error)'];
      if (modules && modules.length > 0) {
        for (const mod of modules) {
          exprs.push(`(import ${mod})`);
        }
      }

      exprs.push(buildHierarchyExpr(modules));

      const result = await runGxi(exprs);

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Error hierarchy inspection timed out.',
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
              text: `Error inspecting hierarchy:\n${result.stderr.trim()}`,
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
              text: `Error inspecting hierarchy:\n${errorMsg}`,
            },
          ],
          isError: true,
        };
      }

      // Parse result lines: name\tprec1,prec2,...
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      interface TypeInfo {
        name: string;
        precedence: string[];
      }

      const types: TypeInfo[] = [];
      for (const line of lines) {
        const payload = line.slice(RESULT_MARKER.length);
        const tabIdx = payload.indexOf('\t');
        if (tabIdx !== -1) {
          const name = payload.slice(0, tabIdx);
          const prec = payload
            .slice(tabIdx + 1)
            .split(',')
            .filter(Boolean);
          types.push({ name, precedence: prec });
        }
      }

      if (types.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'No error types found.',
            },
          ],
          isError: true,
        };
      }

      // Build parent map: each type's direct parent is precedence[0]
      const parentMap = new Map<string, string>();
      const allNames = new Set<string>();

      for (const t of types) {
        allNames.add(t.name);
        if (t.precedence.length > 0) {
          parentMap.set(t.name, t.precedence[0]);
          for (const p of t.precedence) {
            allNames.add(p);
          }
        }
      }

      // Build children map
      const childrenMap = new Map<string, string[]>();
      for (const [child, parent] of parentMap) {
        if (!childrenMap.has(parent)) childrenMap.set(parent, []);
        childrenMap.get(parent)!.push(child);
      }

      // Find roots (names with no parent in our map)
      const roots: string[] = [];
      for (const name of allNames) {
        if (!parentMap.has(name)) {
          roots.push(name);
        }
      }

      // Render tree
      const sections: string[] = ['Gerbil Error Type Hierarchy:', ''];

      function renderTree(
        name: string,
        prefix: string,
        isLast: boolean,
        isRoot: boolean,
      ): void {
        if (isRoot) {
          sections.push(name);
        } else {
          sections.push(`${prefix}${isLast ? '\\-- ' : '|-- '}${name}`);
        }

        const children = childrenMap.get(name) || [];
        for (let i = 0; i < children.length; i++) {
          const childPrefix = isRoot
            ? ''
            : `${prefix}${isLast ? '    ' : '|   '}`;
          renderTree(
            children[i],
            childPrefix,
            i === children.length - 1,
            false,
          );
        }
      }

      for (const root of roots) {
        renderTree(root, '', true, true);
      }

      // Add precedence details
      sections.push('');
      sections.push('Precedence lists:');
      for (const t of types) {
        sections.push(`  ${t.name}: ${t.precedence.join(' -> ')}`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

function buildHierarchyExpr(extraModules?: string[]): string {
  // Build list of type accessors — mix of direct ::t access and raise-based access
  return [
    '(with-catch',
    '  (lambda (e)',
    `    (display "${ERROR_MARKER}\\n")`,
    '    (display-exception e (current-output-port)))',
    '  (lambda ()',
    '    (define (get-type-via-raise thunk)',
    '      (with-catch (lambda (e) (object-type e)) thunk))',
    '    (define (emit-type name t)',
    '      (when (class-type? t)',
    `        (display "${RESULT_MARKER}")`,
    '        (display name)',
    '        (display "\\t")',
    '        (let ((pl (class-type-precedence-list t)))',
    '          (let loop ((rest pl) (first #t))',
    '            (when (pair? rest)',
    '              (unless first (display ","))',
    '              (if (class-type? (car rest))',
    '                (display (class-type-name (car rest)))',
    '                (display (car rest)))',
    '              (loop (cdr rest) #f))))',
    '        (newline)))',
    // Direct ::t types
    '    (emit-type "Exception" Exception::t)',
    '    (emit-type "StackTrace" StackTrace::t)',
    '    (emit-type "Error" Error::t)',
    '    (emit-type "RuntimeException" RuntimeException::t)',
    '    (emit-type "ContractViolation" ContractViolation::t)',
    '    (emit-type "UnboundKeyError" UnboundKeyError::t)',
    // Types via raise + catch
    '    (emit-type "IOError"',
    '      (get-type-via-raise (lambda () (raise-io-error \'x "x"))))',
    '    (emit-type "PrematureEndOfInput"',
    '      (get-type-via-raise (lambda () (raise-premature-end-of-input \'x "x"))))',
    '    (emit-type "Closed"',
    '      (get-type-via-raise (lambda () (raise-io-closed \'x "x"))))',
    '    (emit-type "Timeout"',
    '      (get-type-via-raise (lambda () (raise-timeout \'x "x"))))',
    '    (emit-type "ContextError"',
    '      (get-type-via-raise (lambda () (raise-context-error \'x "x"))))',
    buildExtraModuleScanning(extraModules),
    '  ))',
  ].join(' ');
}

function buildExtraModuleScanning(extraModules?: string[]): string {
  if (!extraModules || extraModules.length === 0) return '';

  // For extra modules, scan their exports for class-type? values
  // that have Exception or Error in their precedence list
  const parts: string[] = [];
  for (const mod of extraModules) {
    const modPath = mod.startsWith(':') ? mod : `:${mod}`;
    parts.push(
      [
        `(with-catch (lambda (e) (void))`,
        `  (lambda ()`,
        `    (let* ((m (import-module (quote ${modPath}) #f #t))`,
        `           (exports (module-context-export m)))`,
        `      (for-each`,
        `        (lambda (ex)`,
        `          (let ((name (module-export-name ex)))`,
        `            (with-catch (lambda (e) (void))`,
        `              (lambda ()`,
        `                (let ((val (eval name)))`,
        `                  (when (and (class-type? val)`,
        `                             (let check ((pl (class-type-precedence-list val)))`,
        `                               (and (pair? pl)`,
        `                                    (or (and (class-type? (car pl))`,
        `                                             (equal? (class-type-name (car pl)) "Exception"))`,
        `                                        (check (cdr pl)))))`,
        `                             (not (equal? (class-type-name val) "Exception")))`,
        `                    (emit-type (symbol->string name) val)))))))`,
        `        exports))))`,
      ].join(' '),
    );
  }

  return parts.join(' ');
}
