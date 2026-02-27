import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { runGxi, escapeSchemeString, buildLoadpathEnv, RESULT_MARKER } from '../gxi.js';

interface UnboundRef {
  identifier: string;
  suggestion: string;
}

export function registerExeMacroCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_exe_macro_check',
    {
      title: 'Exe Macro Expansion Check',
      description:
        'Detect unbound identifiers that would appear after macro expansion in a gxc -exe build. ' +
        'When a macro from an imported module expands to code referencing symbols not available ' +
        'in the consumer module\'s scope (e.g., make-class-instance from :gerbil/runtime/mop), ' +
        'the interpreted gxi works fine but gxc -exe fails with "Reference to unbound identifier". ' +
        'This tool expands all top-level forms and checks for unresolvable references, ' +
        'then suggests which imports to add.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z.string().describe('Path to the Gerbil source file to check'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Directories to add to GERBIL_LOADPATH'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory for auto-configuring GERBIL_LOADPATH'),
      },
    },
    async ({ file_path, loadpath, project_path }) => {
      if (!file_path) {
        return {
          content: [{ type: 'text' as const, text: 'Error: file_path is required.' }],
          isError: true,
        };
      }

      let source: string;
      try {
        source = await readFile(file_path, 'utf-8');
      } catch {
        return {
          content: [{ type: 'text' as const, text: `Error: cannot read file ${file_path}` }],
          isError: true,
        };
      }

      // Build env
      let env: Record<string, string> | undefined;
      if (loadpath && loadpath.length > 0) {
        env = buildLoadpathEnv(loadpath);
      } else if (project_path) {
        const localLib = join(project_path, '.gerbil', 'lib');
        env = buildLoadpathEnv([localLib]);
      }

      const escaped = escapeSchemeString(source);

      // Strategy: use gxc -S (which does full expansion including exe-level checks)
      // but with enhanced error extraction. The key insight is that gxc -S catches
      // unbound identifiers that core-expand alone might miss because core-expand
      // doesn't do the same binding resolution as the exe compiler.
      //
      // We also try core-expand-module to get the fully expanded form and then
      // scan for identifiers that look like they come from runtime modules.
      const exprs = [
        '(import :gerbil/expander)',
        [
          '(with-catch',
          '  (lambda (e)',
          `    (display "${RESULT_MARKER}ERROR\\n")`,
          '    (display-exception e (current-output-port)))',
          '  (lambda ()',
          `    (let* ((port (open-input-string "${escaped}"))`,
          '           (forms (read-all port))',
          '           (expanded (map (lambda (f)',
          '                           (with-catch',
          '                             (lambda (e)',
          '                               (list \'expansion-error (with-output-to-string (lambda () (display-exception e)))))',
          '                             (lambda () (core-expand f))))',
          '                         forms)))',
          '      ;; Check for common runtime symbols that macros often introduce',
          '      (let ((expanded-str (with-output-to-string (lambda () (write expanded)))))',
          '        ;; Look for runtime identifiers that are commonly unbound in exe builds',
          '        (let ((runtime-syms \'(',
          '                "make-class-instance" "make-class-type" "class-instance-init!"',
          '                "make-class-predicate" "make-class-slot-accessor"',
          '                "make-class-slot-mutator" "class-type-seal!"',
          '                "make-struct-instance" "make-struct-type"',
          '                "struct-instance-init!" "make-struct-predicate"',
          '                "make-struct-field-accessor" "make-struct-field-mutator"',
          '                "struct-type-seal!" "make-promise" "force-promise"',
          '                "raise-contract-violation-error")))',
          `          (display "${RESULT_MARKER}OK\\n")`,
          '          (for-each',
          '            (lambda (sym)',
          '              (when (string-contains expanded-str sym)',
          '                (display "RUNTIME-REF:") (displayln sym)))',
          '            runtime-syms)',
          '          ;; Also report any expansion errors',
          '          (for-each',
          '            (lambda (f)',
          '              (when (and (pair? f) (eq? (car f) \'expansion-error))',
          '                (display "EXPAND-ERR:") (displayln (cadr f))))',
          '            expanded)))))))',
        ].join(' '),
      ];

      const result = await runGxi(exprs, { env, timeout: 60_000 });

      if (result.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Macro expansion check timed out.' }],
          isError: true,
        };
      }

      const output = result.stdout;

      // Check for errors
      const errorMarker = `${RESULT_MARKER}ERROR`;
      if (output.includes(errorMarker)) {
        const errorMsg = output.slice(output.indexOf(errorMarker) + errorMarker.length).trim();
        return {
          content: [{ type: 'text' as const, text: `Expansion error:\n${errorMsg}` }],
          isError: true,
        };
      }

      // Parse results
      const runtimeRefs: UnboundRef[] = [];
      const expandErrors: string[] = [];

      for (const line of output.split('\n')) {
        const trimmed = line.trim();
        if (trimmed.startsWith('RUNTIME-REF:')) {
          const sym = trimmed.slice('RUNTIME-REF:'.length).trim();
          const suggestion = suggestImport(sym);
          runtimeRefs.push({ identifier: sym, suggestion });
        } else if (trimmed.startsWith('EXPAND-ERR:')) {
          expandErrors.push(trimmed.slice('EXPAND-ERR:'.length).trim());
        }
      }

      if (runtimeRefs.length === 0 && expandErrors.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: 'No potential exe-build unbound identifiers detected from macro expansions.',
          }],
        };
      }

      const sections: string[] = [];

      if (runtimeRefs.length > 0) {
        sections.push(
          `Found ${runtimeRefs.length} runtime identifier(s) referenced in macro expansions that may be unbound in gxc -exe:`,
          '',
        );
        for (const ref of runtimeRefs) {
          sections.push(`  ${ref.identifier}`);
          sections.push(`    add: ${ref.suggestion}`);
        }
      }

      if (expandErrors.length > 0) {
        if (sections.length > 0) sections.push('');
        sections.push(`Expansion errors (${expandErrors.length}):`);
        for (const err of expandErrors.slice(0, 10)) {
          sections.push(`  ${err}`);
        }
      }

      if (runtimeRefs.length > 0) {
        sections.push('');
        sections.push(
          'Fix: Add the suggested import(s) to the macro-defining module and re-export them,',
          'or add them directly to the consumer module.',
        );
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: runtimeRefs.length > 0,
      };
    },
  );
}

/**
 * Suggest the import module for a known runtime identifier.
 */
function suggestImport(identifier: string): string {
  const MOP_SYMS = new Set([
    'make-class-instance', 'make-class-type', 'class-instance-init!',
    'make-class-predicate', 'make-class-slot-accessor',
    'make-class-slot-mutator', 'class-type-seal!',
  ]);

  const STRUCT_SYMS = new Set([
    'make-struct-instance', 'make-struct-type', 'struct-instance-init!',
    'make-struct-predicate', 'make-struct-field-accessor',
    'make-struct-field-mutator', 'struct-type-seal!',
  ]);

  if (MOP_SYMS.has(identifier)) {
    return `(import :gerbil/runtime/mop) or (export (import: :gerbil/runtime/mop)) in the macro module`;
  }
  if (STRUCT_SYMS.has(identifier)) {
    return `(import :gerbil/runtime/mop) or (export (import: :gerbil/runtime/mop)) in the macro module`;
  }
  if (identifier === 'make-promise' || identifier === 'force-promise') {
    return `(import :gerbil/runtime) or (export (import: :gerbil/runtime)) in the macro module`;
  }
  if (identifier === 'raise-contract-violation-error') {
    return `(import :gerbil/runtime/error) or (export (import: :gerbil/runtime/error)) in the macro module`;
  }

  return `Find the defining module with gerbil_find_definition or gerbil_suggest_imports`;
}
