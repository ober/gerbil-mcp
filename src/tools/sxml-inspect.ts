import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, ERROR_MARKER, buildLoadpathEnv } from '../gxi.js';

const RESULT_MARKER = 'GERBIL-MCP-SXML:';

export function registerSxmlInspectTool(server: McpServer): void {
  server.registerTool(
    'gerbil_sxml_inspect',
    {
      title: 'SXML Tree Viewer',
      description:
        'Parse XML text or evaluate an expression that produces SXML, ' +
        'then display the tree structure with node types and summaries. ' +
        'Identifies DOCUMENT (*TOP*), PI (*PI*), ATTRIBUTES (@), NAMESPACES, ' +
        'ELEMENT, ATTR, and TEXT nodes. Exactly one of xml_text or expression is required.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        xml_text: z
          .string()
          .optional()
          .describe(
            'XML string to parse into SXML (e.g. "<root><item>hello</item></root>")',
          ),
        expression: z
          .string()
          .optional()
          .describe(
            'Gerbil expression that evaluates to an SXML tree',
          ),
        imports: z
          .array(z.string())
          .optional()
          .describe(
            'Module paths to import before evaluation (only used with expression mode)',
          ),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH for project-local module resolution',
          ),
      },
    },
    async ({ xml_text, expression, imports, loadpath }) => {
      // Validate: exactly one of xml_text or expression
      if (!xml_text && !expression) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Error: Exactly one of xml_text or expression is required.',
            },
          ],
          isError: true,
        };
      }
      if (xml_text && expression) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Error: Provide only one of xml_text or expression, not both.',
            },
          ],
          isError: true,
        };
      }

      const exprs: string[] = [];

      if (xml_text) {
        exprs.push('(import :std/markup/xml)');
      }

      if (imports && imports.length > 0) {
        for (const mod of imports) {
          exprs.push(`(import ${mod})`);
        }
      }

      const escaped = xml_text
        ? escapeSchemeString(xml_text)
        : escapeSchemeString(expression!);

      exprs.push(buildSxmlExpr(escaped, !!xml_text));

      const env =
        loadpath && loadpath.length > 0
          ? buildLoadpathEnv(loadpath)
          : undefined;

      const result = await runGxi(exprs, { timeout: 30_000, env });

      if (result.timedOut) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'SXML inspect timed out after 30 seconds.',
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
              text: `SXML inspect error:\n${result.stderr.trim()}`,
            },
          ],
          isError: true,
        };
      }

      const stdout = result.stdout;
      const errorIdx = stdout.indexOf(ERROR_MARKER);
      if (errorIdx !== -1) {
        const sideOutput = stdout.slice(0, errorIdx).trim();
        const errorMsg = stdout.slice(errorIdx + ERROR_MARKER.length).trim();
        const parts: string[] = [];
        if (sideOutput) parts.push(`Output:\n${sideOutput}`);
        parts.push(`Error:\n${errorMsg}`);
        return {
          content: [
            { type: 'text' as const, text: parts.join('\n\n') },
          ],
          isError: true,
        };
      }

      // Parse tree lines
      const lines = stdout
        .split('\n')
        .filter((l) => l.startsWith(RESULT_MARKER));

      if (lines.length === 0) {
        return {
          content: [
            { type: 'text' as const, text: 'No SXML tree produced.' },
          ],
          isError: true,
        };
      }

      const treeLines: string[] = ['SXML Tree:', ''];

      for (const line of lines) {
        const payload = line.slice(RESULT_MARKER.length);
        const parts = payload.split('\t');
        if (parts.length >= 3) {
          const depth = parseInt(parts[0], 10);
          const nodeType = parts[1];
          const summary = parts.slice(2).join('\t');
          const indent = '  '.repeat(depth);
          treeLines.push(`${indent}[${nodeType}] ${summary}`);
        }
      }

      return {
        content: [
          { type: 'text' as const, text: treeLines.join('\n') },
        ],
      };
    },
  );
}

function buildSxmlExpr(escaped: string, isXml: boolean): string {
  const parts: string[] = [];

  // Tree walker that emits depth/type/summary lines
  const walker = `
    (def (__mcp_walk node depth)
      (cond
        ((string? node)
         (let ((preview (if (> (string-length node) 80)
                          (string-append (substring node 0 80) "...")
                          node)))
           (display "${RESULT_MARKER}")
           (display depth) (display "\\t")
           (display "TEXT") (display "\\t")
           (display (object->string preview))
           (newline)))
        ((not (pair? node)) (void))
        (else
          (let ((tag (car node)))
            (cond
              ((eq? tag '*TOP*)
               (display "${RESULT_MARKER}")
               (display depth) (display "\\tDOCUMENT\\t*TOP*\\n")
               (for-each (lambda (c) (__mcp_walk c (+ depth 1))) (cdr node)))
              ((eq? tag '*PI*)
               (display "${RESULT_MARKER}")
               (display depth) (display "\\tPI\\t")
               (when (pair? (cdr node))
                 (display (cadr node))
                 (when (pair? (cddr node))
                   (display " ")
                   (let ((s (caddr node)))
                     (display (if (and (string? s) (> (string-length s) 60))
                                (string-append (substring s 0 60) "...")
                                s)))))
               (newline))
              ((eq? tag '@)
               (display "${RESULT_MARKER}")
               (display depth) (display "\\tATTRIBUTES\\t@\\n")
               (for-each
                 (lambda (a)
                   (when (pair? a)
                     (if (eq? (car a) '*NAMESPACES*)
                       (begin
                         (display "${RESULT_MARKER}")
                         (display (+ depth 1)) (display "\\tNAMESPACES\\t")
                         (display (length (cdr a))) (display " declarations\\n"))
                       (begin
                         (display "${RESULT_MARKER}")
                         (display (+ depth 1)) (display "\\tATTR\\t")
                         (display (car a))
                         (when (pair? (cdr a))
                           (display " = ")
                           (display (cadr a)))
                         (newline)))))
                 (cdr node)))
              ((symbol? tag)
               (display "${RESULT_MARKER}")
               (display depth) (display "\\tELEMENT\\t")
               (display tag)
               (let ((children (cdr node)))
                 (let ((text-count 0) (elem-count 0))
                   (for-each (lambda (c)
                     (cond ((string? c) (set! text-count (+ text-count 1)))
                           ((and (pair? c) (not (eq? (car c) '@)))
                            (set! elem-count (+ elem-count 1)))))
                     children)
                   (when (or (> text-count 0) (> elem-count 0))
                     (display " (")
                     (when (> elem-count 0)
                       (display elem-count) (display " children"))
                     (when (and (> text-count 0) (> elem-count 0))
                       (display ", "))
                     (when (> text-count 0)
                       (display text-count) (display " text"))
                     (display ")")))
                 (newline)
                 (for-each (lambda (c) (__mcp_walk c (+ depth 1))) children)))
              (else (void)))))))
  `.trim().replace(/\n\s*/g, ' ');

  parts.push('(with-catch');
  parts.push('  (lambda (e)');
  parts.push(`    (display "${ERROR_MARKER}\\n")`);
  parts.push('    (display-exception e (current-output-port)))');
  parts.push('  (lambda ()');
  parts.push(`    ${walker}`);

  if (isXml) {
    parts.push(
      `    (let ((tree (call-with-input-string "${escaped}" read-xml)))`,
    );
  } else {
    parts.push(
      `    (let ((tree (eval (read (open-input-string "${escaped}")))))`,
    );
  }

  parts.push('      (__mcp_walk tree 0))))');

  return parts.join(' ');
}
