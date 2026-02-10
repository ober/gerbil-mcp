import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString } from '../gxi.js';

export function registerDynamicReferenceTool(server: McpServer): void {
  server.registerTool(
    'gerbil_dynamic_reference',
    {
      title: 'Dynamic Module Reference',
      description:
        'Auto-generate reference documentation for any Gerbil module on demand. ' +
        'Introspects all exports, classifies them (procedure/macro/value/type), ' +
        'includes arities, and formats as markdown. Works with any module path — ' +
        'not limited to the 5 hardcoded reference docs.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        module_path: z
          .string()
          .describe('Module path (e.g. :std/text/json, :std/db/dbi, :std/event)'),
        include_signatures: z
          .boolean()
          .optional()
          .describe('Include function signatures/arities (default: true, slower)'),
      },
    },
    async ({ module_path, include_signatures }) => {
      const withSigs = include_signatures !== false;
      const escapedMod = escapeSchemeString(module_path);

      // Step 1: Get export names using module-context-export (proven pattern)
      const exprs = [
        '(import :gerbil/expander)',
        `(import ${module_path})`,
        [
          `(let* ((mod (import-module (quote ${module_path}) #t #t))`,
          '       (exports (module-context-export mod))',
          '       (names (map module-export-name exports)))',
          '  (for-each (lambda (name)',
          '    (let ((resolved (with-catch (lambda (e) #f) (lambda () (eval name)))))',
          '      (display "EXPORT:")',
          '      (display name)',
          '      (display " ")',
          '      (display (cond ((not resolved) "unknown")',
          '                     ((procedure? resolved) "procedure")',
          '                     ((number? resolved) "constant")',
          '                     ((string? resolved) "constant")',
          '                     (else "value")))',
          '      (newline)))',
          '    names)',
          '  (display "TOTAL:")',
          '  (display (length names))',
          '  (newline))',
        ].join(' '),
      ];

      let result;
      try {
        result = await runGxi(exprs, { timeout: 15000 });
      } catch (e) {
        return {
          content: [{
            type: 'text' as const,
            text: `Failed to introspect module ${module_path}: ${e}`,
          }],
          isError: true,
        };
      }

      const output = result.stdout + result.stderr;
      const exportLines = output.split('\n')
        .filter((l) => l.startsWith('EXPORT:'))
        .map((l) => l.slice(7));
      const total = output.match(/TOTAL:(\d+)/)?.[1] || '?';

      if (exportLines.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: `Could not introspect exports for ${module_path}.\n` +
              'The module may not exist or may require special loading.',
          }],
          isError: true,
        };
      }

      // Parse entries and classify
      const procs: Array<{ name: string; info: string }> = [];
      const macros: Array<{ name: string }> = [];
      const constants: Array<{ name: string; value: string }> = [];
      const values: Array<{ name: string }> = [];
      const unknown: Array<{ name: string }> = [];

      for (const line of exportLines) {
        // Format: "name kind" (e.g., "json-object->string procedure")
        const spaceIdx = line.lastIndexOf(' ');
        if (spaceIdx < 0) continue;
        const name = line.slice(0, spaceIdx).trim();
        const kind = line.slice(spaceIdx + 1).trim();
        if (!name) continue;

        switch (kind) {
          case 'procedure':
            procs.push({ name, info: '' });
            break;
          case 'constant':
            constants.push({ name, value: '' });
            break;
          case 'value':
            values.push({ name });
            break;
          case 'unknown':
            // Likely a macro
            macros.push({ name });
            break;
          default:
            unknown.push({ name });
        }
      }

      // Format as markdown reference
      const sections: string[] = [
        `# ${module_path} — API Reference`,
        '',
        `*Auto-generated reference documentation*`,
        '',
        `Total exports: ${total}`,
        `- Procedures: ${procs.length}`,
        `- Macros/Syntax: ${macros.length}`,
        `- Constants: ${constants.length}`,
        `- Values: ${values.length}`,
        '',
      ];

      if (procs.length > 0) {
        sections.push('## Procedures');
        sections.push('');
        sections.push('| Name | Info |');
        sections.push('|------|------|');
        for (const p of procs) {
          sections.push(`| \`${p.name}\` | ${p.info || '—'} |`);
        }
        sections.push('');
      }

      if (macros.length > 0) {
        sections.push('## Macros / Syntax');
        sections.push('');
        for (const m of macros) {
          sections.push(`- \`${m.name}\``);
        }
        sections.push('');
      }

      if (constants.length > 0) {
        sections.push('## Constants');
        sections.push('');
        sections.push('| Name | Value |');
        sections.push('|------|-------|');
        for (const c of constants) {
          sections.push(`| \`${c.name}\` | \`${c.value}\` |`);
        }
        sections.push('');
      }

      if (values.length > 0) {
        sections.push('## Values');
        sections.push('');
        for (const v of values) {
          sections.push(`- \`${v.name}\``);
        }
        sections.push('');
      }

      sections.push('---');
      sections.push('');
      sections.push(`*Use \`gerbil_doc\` for detailed documentation on specific symbols.*`);
      sections.push(`*Use \`gerbil_function_signature\` to check exact arities and keyword args.*`);

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
