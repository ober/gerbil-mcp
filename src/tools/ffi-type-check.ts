import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { scanSchemeFiles } from './parse-utils.js';

interface FfiDeclaration {
  name: string;
  paramTypes: string[];
  returnType: string;
  line: number;
  file: string;
}

interface FfiTypeDiagnostic {
  file: string;
  line: number;
  severity: 'error' | 'warning';
  message: string;
}

/**
 * FFI type compatibility rules.
 * Maps from Gambit FFI types to the Scheme types they accept.
 */
const FFI_TYPE_ACCEPTS: Record<string, string[]> = {
  'int': ['fixnum', 'integer'],
  'int64': ['fixnum', 'integer'],
  'unsigned-int64': ['fixnum', 'integer'],
  'float': ['flonum', 'number'],
  'double': ['flonum', 'number'],
  'char-string': ['string'],
  'UTF-8-string': ['string'],
  'nonnull-char-string': ['string'],
  'nonnull-UTF-8-string': ['string'],
  'scheme-object': ['any'],
  'bool': ['boolean'],
  'void': [],
  'void*': [],
};

/**
 * Known incompatible type combinations.
 * (scheme-value-type, ffi-param-type) -> warning message
 */
const INCOMPATIBLE_TYPES: Array<{
  schemeType: string;
  ffiType: string;
  message: string;
}> = [
  {
    schemeType: 'u8vector',
    ffiType: '(pointer void)',
    message: 'u8vector passed to (pointer void) — Gambit passes the raw object address, not the data. Use scheme-object with ___BODY(arg) in C, or (pointer unsigned-int8) with ##u8vector->cpointer.',
  },
  {
    schemeType: 'u8vector',
    ffiType: 'void*',
    message: 'u8vector passed to void* — use scheme-object and access data via U8_DATA() macro in C.',
  },
  {
    schemeType: 'string',
    ffiType: '(pointer void)',
    message: 'string passed to (pointer void) — use char-string or UTF-8-string for C string parameters.',
  },
  {
    schemeType: 'string',
    ffiType: 'int',
    message: 'string passed to int parameter — type mismatch.',
  },
  {
    schemeType: 'fixnum',
    ffiType: 'char-string',
    message: 'number passed to char-string parameter — expected a string.',
  },
  {
    schemeType: 'fixnum',
    ffiType: 'UTF-8-string',
    message: 'number passed to UTF-8-string parameter — expected a string.',
  },
];

/**
 * Extract c-lambda / define-c-lambda declarations from a file.
 */
function extractFfiDeclarations(content: string, filePath: string): FfiDeclaration[] {
  const decls: FfiDeclaration[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Match (define-c-lambda name (param-types...) return-type ...)
    const defineCLambda = line.match(
      /\(\s*define-c-lambda\s+([a-zA-Z_!?<>=+\-*/][a-zA-Z0-9_!?<>=+\-*/.]*)\s+\(([^)]*)\)\s+([^\s)]+)/,
    );
    if (defineCLambda) {
      const name = defineCLambda[1];
      const paramStr = defineCLambda[2].trim();
      const returnType = defineCLambda[3];
      const paramTypes = paramStr ? paramStr.split(/\s+/) : [];
      decls.push({ name, paramTypes, returnType, line: i + 1, file: filePath });
      continue;
    }

    // Match (c-lambda (param-types...) return-type ...) assigned via (def name (c-lambda ...))
    const defCLambda = line.match(
      /\(\s*(?:def|define)\s+([a-zA-Z_!?<>=+\-*/][a-zA-Z0-9_!?<>=+\-*/.]*)\s+\(\s*c-lambda\s+\(([^)]*)\)\s+([^\s)]+)/,
    );
    if (defCLambda) {
      const name = defCLambda[1];
      const paramStr = defCLambda[2].trim();
      const returnType = defCLambda[3];
      const paramTypes = paramStr ? paramStr.split(/\s+/) : [];
      decls.push({ name, paramTypes, returnType, line: i + 1, file: filePath });
    }
  }

  return decls;
}

/**
 * Try to infer the Scheme type of a value from how it's constructed.
 * Returns null if type can't be inferred.
 */
function inferType(expr: string): string | null {
  const trimmed = expr.trim();
  if (trimmed.startsWith('#u8(') || trimmed.startsWith('(make-u8vector') ||
      trimmed.startsWith('(u8vector') || trimmed.startsWith('(string->bytes')) {
    return 'u8vector';
  }
  if (trimmed.startsWith('"') || trimmed.startsWith('(string-append') ||
      trimmed.startsWith('(substring') || trimmed.startsWith('(number->string')) {
    return 'string';
  }
  if (/^\d+$/.test(trimmed) || trimmed.startsWith('(+') || trimmed.startsWith('(-') ||
      trimmed.startsWith('(*') || trimmed.startsWith('(string-length')) {
    return 'fixnum';
  }
  if (trimmed === '#t' || trimmed === '#f') return 'boolean';
  return null;
}

/**
 * Scan for call sites of FFI functions and check type compatibility.
 */
function checkCallSites(
  content: string,
  filePath: string,
  declarations: Map<string, FfiDeclaration>,
): FfiTypeDiagnostic[] {
  const diagnostics: FfiTypeDiagnostic[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();
    if (line.startsWith(';')) continue; // skip comments

    // Find function calls: (func-name arg1 arg2 ...)
    for (const [name, decl] of declarations) {
      // Simple pattern: find calls to this function
      const callPattern = new RegExp(
        `\\(\\s*${name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\s+(.+?)\\)`,
        'g',
      );
      let match;
      while ((match = callPattern.exec(line)) !== null) {
        // Skip if this is the declaration line itself
        if (i + 1 === decl.line && filePath === decl.file) continue;

        // Try to parse arguments (simplified)
        const argsStr = match[1];
        const args = splitArgs(argsStr);

        for (let argIdx = 0; argIdx < Math.min(args.length, decl.paramTypes.length); argIdx++) {
          const argType = inferType(args[argIdx]);
          if (!argType) continue;

          const paramType = decl.paramTypes[argIdx];
          // Check against known incompatible combinations
          for (const rule of INCOMPATIBLE_TYPES) {
            if (argType === rule.schemeType && paramType === rule.ffiType) {
              diagnostics.push({
                file: filePath,
                line: i + 1,
                severity: 'warning',
                message: `${name} arg ${argIdx + 1}: ${rule.message}`,
              });
            }
          }

          // Check for (pointer void) specifically
          if (argType === 'u8vector' && paramType.includes('pointer') && paramType.includes('void')) {
            const alreadyReported = diagnostics.some(
              (d) => d.line === i + 1 && d.message.includes(`${name} arg ${argIdx + 1}`),
            );
            if (!alreadyReported) {
              diagnostics.push({
                file: filePath,
                line: i + 1,
                severity: 'warning',
                message: `${name} arg ${argIdx + 1}: u8vector may be incompatible with pointer type "${paramType}" — consider scheme-object.`,
              });
            }
          }
        }
      }
    }
  }

  return diagnostics;
}

/**
 * Simple argument splitter for Scheme expressions.
 * Handles nested parens but not all edge cases.
 */
function splitArgs(text: string): string[] {
  const args: string[] = [];
  let depth = 0;
  let current = '';
  let inString = false;

  for (let i = 0; i < text.length; i++) {
    const ch = text[i];

    if (inString) {
      current += ch;
      if (ch === '"' && text[i - 1] !== '\\') inString = false;
      continue;
    }

    if (ch === '"') {
      inString = true;
      current += ch;
      continue;
    }

    if (ch === '(' || ch === '[') {
      depth++;
      current += ch;
    } else if (ch === ')' || ch === ']') {
      depth--;
      current += ch;
    } else if (/\s/.test(ch) && depth === 0) {
      if (current.trim()) args.push(current.trim());
      current = '';
    } else {
      current += ch;
    }
  }
  if (current.trim()) args.push(current.trim());
  return args;
}

export function registerFfiTypeCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_ffi_type_check',
    {
      title: 'FFI Type Safety Check',
      description:
        'Detect FFI type mismatches between c-lambda/define-c-lambda declarations and call sites. ' +
        'Flags known incompatible combinations like u8vector passed to (pointer void), ' +
        'string passed to int, etc. Static analysis — no subprocess required.',
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Single .ss file to check'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory to check all .ss files'),
      },
    },
    async ({ file_path, project_path }) => {
      if (!file_path && !project_path) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Either file_path or project_path is required.',
            },
          ],
          isError: true,
        };
      }

      // Collect files to check
      const files: string[] = [];
      if (file_path) {
        files.push(file_path);
      } else if (project_path) {
        const found = await scanSchemeFiles(project_path);
        files.push(...found);
      }

      // Phase 1: Extract all FFI declarations
      const allDeclarations = new Map<string, FfiDeclaration>();
      const fileContents = new Map<string, string>();

      for (const f of files) {
        try {
          const content = await readFile(f, 'utf-8');
          fileContents.set(f, content);
          const decls = extractFfiDeclarations(content, f);
          for (const d of decls) {
            allDeclarations.set(d.name, d);
          }
        } catch {
          // skip unreadable
        }
      }

      if (allDeclarations.size === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No FFI declarations (define-c-lambda / c-lambda) found.`,
            },
          ],
        };
      }

      // Phase 2: Check call sites against declarations
      const diagnostics: FfiTypeDiagnostic[] = [];
      for (const [f, content] of fileContents) {
        diagnostics.push(...checkCallSites(content, f, allDeclarations));
      }

      // Format output
      const sections: string[] = [
        `FFI Type Safety Check`,
        ``,
        `Declarations found: ${allDeclarations.size}`,
        `Files checked: ${fileContents.size}`,
        '',
      ];

      if (diagnostics.length === 0) {
        sections.push('No type mismatches detected.');
      } else {
        sections.push(`${diagnostics.length} potential type mismatch(es):`);
        sections.push('');
        for (const d of diagnostics) {
          sections.push(`  [${d.severity.toUpperCase()}] ${d.file}:${d.line} — ${d.message}`);
        }
      }

      // Also report declarations for reference
      sections.push('');
      sections.push('FFI declarations:');
      for (const [name, decl] of allDeclarations) {
        sections.push(`  ${name}  (${decl.paramTypes.join(' ')}) -> ${decl.returnType}  [${decl.file}:${decl.line}]`);
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: diagnostics.some((d) => d.severity === 'error'),
      };
    },
  );
}
