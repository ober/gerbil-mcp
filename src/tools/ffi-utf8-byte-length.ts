import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { scanSchemeFiles } from './parse-utils.js';

interface LengthMismatch {
  function: string;
  parameter: string;
  line: number;
  file: string;
  context: string;
}

/**
 * Extract c-lambda declarations that have int/size_t types (potential length parameters).
 * Returns set of function names that take char-string + int parameters.
 */
function extractStringLengthFunctions(content: string): Set<string> {
  const funcs = new Set<string>();
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Match c-lambda or define-c-lambda declarations
    if (line.includes('c-lambda')) {
      // Check next few lines for type signature with char-string and int
      const lookAhead = lines.slice(i, Math.min(i + 5, lines.length)).join(' ');

      if (lookAhead.includes('char-string') &&
          (lookAhead.includes('int') || lookAhead.includes('size_t'))) {

        // Try to find the function name
        let funcName: string | null = null;

        // Look backwards for (def name (c-lambda ...))
        for (let j = i; j >= Math.max(0, i - 5); j--) {
          const defMatch = lines[j].match(/\(def\s+([a-zA-Z_!?<>=+\-*/][-a-zA-Z0-9_!?<>=+\-*/:~#]*)/);
          if (defMatch) {
            funcName = defMatch[1];
            break;
          }
        }

        // Or for define-c-lambda with name directly
        const defineCMatch = line.match(/\(define-c-lambda\s+([a-zA-Z_!?<>=+\-*/][-a-zA-Z0-9_!?<>=+\-*/:~#]*)/);
        if (defineCMatch) {
          funcName = defineCMatch[1];
        }

        if (funcName) {
          funcs.add(funcName);
        }
      }
    }
  }

  return funcs;
}

/**
 * Find call sites where string-length is passed as an argument.
 */
function findStringLengthUsage(
  content: string,
  filePath: string,
  funcs: Set<string>,
): LengthMismatch[] {
  const mismatches: LengthMismatch[] = [];
  const lines = content.split('\n');

  for (const funcName of funcs) {
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];

      // Look for calls to this function with string-length
      // Pattern: (funcName ... (string-length ...) ...)
      const callPattern = new RegExp(`\\(${funcName}\\b[^)]*\\(string-length\\s+([^)]+)\\)`, 'g');
      const match = callPattern.exec(line);

      if (match) {
        const stringArg = match[1];
        mismatches.push({
          function: funcName,
          parameter: 'length',
          line: i + 1,
          file: filePath,
          context: line.trim(),
        });
      }

      // Also check for let bindings that compute string-length then pass it
      // (let ((len (string-length str))) (funcName ... len ...))
      if (line.includes('string-length')) {
        const lookAhead = lines.slice(i, Math.min(i + 5, lines.length)).join('\n');
        if (lookAhead.includes(funcName)) {
          // Extract variable name bound to string-length
          const letMatch = line.match(/\((\w+)\s+\(string-length/);
          if (letMatch) {
            const varName = letMatch[1];
            // Check if this variable is passed to funcName
            if (new RegExp(`\\(${funcName}\\b[^)]*\\b${varName}\\b`).test(lookAhead)) {
              mismatches.push({
                function: funcName,
                parameter: varName,
                line: i + 1,
                file: filePath,
                context: line.trim(),
              });
            }
          }
        }
      }
    }
  }

  return mismatches;
}

export function registerFfiUtf8ByteLengthTool(server: McpServer): void {
  server.registerTool(
    'gerbil_ffi_utf8_byte_length_audit',
    {
      title: 'FFI UTF-8 Byte Length Audit',
      description:
        'Static analysis to detect when string-length is used where byte length is needed in FFI calls. ' +
        'In Gambit FFI, char-string arguments are UTF-8 encoded, so string-length returns character count ' +
        'while C functions typically expect byte count. Suggests using PCRE2_ZERO_TERMINATED, strlen() in C, ' +
        'or (u8vector-length (string->bytes s)) instead.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Path to a single .ss or .scm FFI file to audit'),
        project_path: z
          .string()
          .optional()
          .describe('Path to project directory to audit all FFI files'),
      },
    },
    async ({ file_path, project_path }) => {
      if (!file_path && !project_path) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Error: Must provide either file_path or project_path.',
            },
          ],
          isError: true,
        };
      }

      const filesToScan: string[] = [];

      if (file_path) {
        filesToScan.push(file_path);
      } else if (project_path) {
        const allFiles = await scanSchemeFiles(project_path);
        // Focus on FFI files (contain c-lambda or begin-ffi)
        for (const f of allFiles) {
          try {
            const content = await readFile(f, 'utf-8');
            if (content.includes('c-lambda') || content.includes('begin-ffi')) {
              filesToScan.push(f);
            }
          } catch {
            // skip unreadable files
          }
        }
      }

      if (filesToScan.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'No FFI files found to audit.',
            },
          ],
        };
      }

      const allMismatches: LengthMismatch[] = [];

      for (const f of filesToScan) {
        try {
          const content = await readFile(f, 'utf-8');
          const funcs = extractStringLengthFunctions(content);
          const mismatches = findStringLengthUsage(content, f, funcs);
          allMismatches.push(...mismatches);
        } catch (err) {
          const msg = err instanceof Error ? err.message : 'Unknown error';
          return {
            content: [
              {
                type: 'text' as const,
                text: `Failed to read file ${f}: ${msg}`,
              },
            ],
            isError: true,
          };
        }
      }

      if (allMismatches.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No string-length/byte-length mismatches detected in ${filesToScan.length} file(s).`,
            },
          ],
        };
      }

      const sections: string[] = [
        `FFI UTF-8 Byte Length Audit: ${allMismatches.length} potential mismatch(es)`,
        '',
      ];

      for (const m of allMismatches) {
        sections.push(`  ${m.file}:${m.line} — Function: ${m.function}`);
        sections.push(`    Using string-length where byte length expected`);
        sections.push(`    Context: ${m.context}`);
        sections.push(
          `    Fix: Use PCRE2_ZERO_TERMINATED, strlen() in C shim, or (u8vector-length (string->bytes s))`,
        );
        sections.push('');
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: allMismatches.length > 0,
      };
    },
  );
}
