/**
 * gerbil_ffi_callback_debug — Static analysis of FFI c-define/extern linkage.
 * Parses .ss files for c-define forms and extern declarations inside begin-foreign
 * blocks, cross-references them, and reports issues.
 *
 * Pure TypeScript, no subprocess.
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';

interface CDefine {
  schemeName: string;
  cName: string;
  paramTypes: string;
  returnType: string;
  line: number;
  insideBeginForeign: boolean;
}

interface ExternDecl {
  cName: string;
  line: number;
}

interface FfiIssue {
  severity: 'error' | 'warning';
  line: number;
  message: string;
}

/**
 * Parse c-define forms from source.
 * (c-define (scheme-name param ...) (param-type ...) return-type "c-name" ...)
 */
function parseCDefines(lines: string[]): CDefine[] {
  const results: CDefine[] = [];
  let insideBeginForeign = false;
  let foreignDepth = 0;

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trimStart();

    // Track begin-foreign blocks
    if (trimmed.includes('begin-foreign')) {
      insideBeginForeign = true;
      foreignDepth++;
    }

    // Track depth to detect end of begin-foreign
    if (insideBeginForeign) {
      for (const ch of lines[i]) {
        if (ch === '(') foreignDepth++;
        else if (ch === ')') {
          foreignDepth--;
          if (foreignDepth <= 0) {
            insideBeginForeign = false;
            foreignDepth = 0;
          }
        }
      }
    }

    // Match (c-define ...)
    const match = trimmed.match(/^\(c-define\s+\((\S+)/);
    if (match) {
      const schemeName = match[1];

      // Gather full form to extract c-name
      let form = trimmed;
      let depth = 0;
      for (let j = i; j < Math.min(lines.length, i + 10); j++) {
        if (j > i) form += '\n' + lines[j];
        for (const ch of lines[j]) {
          if (ch === '(') depth++;
          else if (ch === ')') depth--;
        }
        if (depth <= 0) break;
      }

      // Extract c-name from the form — it's typically a quoted string
      const cNameMatch = form.match(/"([^"]+)"/);
      const cName = cNameMatch ? cNameMatch[1] : schemeName;

      // Extract param types and return type (heuristic)
      const paramTypesMatch = form.match(/\)\s*\(([^)]*)\)/);
      const paramTypes = paramTypesMatch ? paramTypesMatch[1].trim() : '';
      const returnMatch = form.match(/\)\s*\([^)]*\)\s+(\S+)/);
      const returnType = returnMatch ? returnMatch[1] : '';

      results.push({
        schemeName,
        cName,
        paramTypes,
        returnType,
        line: i + 1,
        insideBeginForeign,
      });
    }
  }

  return results;
}

/**
 * Parse extern declarations inside begin-foreign blocks.
 * Looks for patterns like: (##extern "c-name" scheme-name)
 * or c-declare with function declarations.
 */
function parseExternDecls(lines: string[]): ExternDecl[] {
  const results: ExternDecl[] = [];

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trimStart();

    // Match extern declarations
    const externMatch = trimmed.match(/\(extern\s+"?([^")\s]+)"?\s/);
    if (externMatch) {
      results.push({ cName: externMatch[1], line: i + 1 });
      continue;
    }

    // Match ##extern
    const hashExternMatch = trimmed.match(/\(##extern\s+"([^"]+)"/);
    if (hashExternMatch) {
      results.push({ cName: hashExternMatch[1], line: i + 1 });
    }
  }

  return results;
}

export function registerFfiCallbackDebugTool(server: McpServer): void {
  server.registerTool(
    'gerbil_ffi_callback_debug',
    {
      title: 'FFI Callback Debugger',
      description:
        'Analyze FFI c-define/extern linkage in a Gerbil source file. ' +
        'Detects: c-define without matching extern, extern without matching c-define, ' +
        'duplicate c-define C names, and c-define outside begin-foreign blocks.',
      inputSchema: {
        file_path: z
          .string()
          .describe('Absolute path to a Gerbil source file (.ss) to analyze'),
      },
    },
    async ({ file_path }) => {
      let content: string;
      try {
        content = await readFile(file_path, 'utf-8');
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [{ type: 'text' as const, text: `Failed to read file: ${msg}` }],
          isError: true,
        };
      }

      const lines = content.split('\n');
      const cDefines = parseCDefines(lines);
      const externDecls = parseExternDecls(lines);
      const issues: FfiIssue[] = [];

      // Build lookup sets
      const cDefineNames = new Map<string, CDefine[]>();
      for (const cd of cDefines) {
        const existing = cDefineNames.get(cd.cName) || [];
        existing.push(cd);
        cDefineNames.set(cd.cName, existing);
      }
      const externNames = new Set(externDecls.map((e) => e.cName));

      // Check 1: Duplicate c-define C names
      for (const [cName, defs] of cDefineNames) {
        if (defs.length > 1) {
          issues.push({
            severity: 'error',
            line: defs[1].line,
            message: `Duplicate c-define C name "${cName}" — first defined at line ${defs[0].line}`,
          });
        }
      }

      // Check 2: c-define without matching extern
      for (const cd of cDefines) {
        if (!externNames.has(cd.cName)) {
          issues.push({
            severity: 'warning',
            line: cd.line,
            message: `c-define "${cd.schemeName}" (C name "${cd.cName}") has no matching extern declaration`,
          });
        }
      }

      // Check 3: extern without matching c-define
      for (const ext of externDecls) {
        if (!cDefineNames.has(ext.cName)) {
          issues.push({
            severity: 'warning',
            line: ext.line,
            message: `extern "${ext.cName}" has no matching c-define`,
          });
        }
      }

      // Check 4: c-define outside begin-foreign
      for (const cd of cDefines) {
        if (!cd.insideBeginForeign) {
          issues.push({
            severity: 'warning',
            line: cd.line,
            message: `c-define "${cd.schemeName}" is outside begin-foreign block`,
          });
        }
      }

      // Format output
      const sections: string[] = [`FFI Callback Analysis: ${file_path}`, ''];

      // List callbacks found
      if (cDefines.length > 0) {
        sections.push(`Callbacks (${cDefines.length}):`);
        for (const cd of cDefines) {
          const linked = externNames.has(cd.cName) ? 'linked' : 'unlinked';
          const location = cd.insideBeginForeign ? '' : ' [outside begin-foreign]';
          sections.push(
            `  L${cd.line}: ${cd.schemeName} -> "${cd.cName}" (${linked})${location}`,
          );
        }
      } else {
        sections.push('No c-define callbacks found.');
      }

      if (externDecls.length > 0) {
        sections.push('');
        sections.push(`Extern declarations (${externDecls.length}):`);
        for (const ext of externDecls) {
          const linked = cDefineNames.has(ext.cName) ? 'linked' : 'orphan';
          sections.push(`  L${ext.line}: "${ext.cName}" (${linked})`);
        }
      }

      // Issues
      if (issues.length > 0) {
        sections.push('');
        sections.push(`Issues (${issues.length}):`);
        for (const issue of issues) {
          sections.push(
            `  [${issue.severity.toUpperCase()}] L${issue.line}: ${issue.message}`,
          );
        }
      } else if (cDefines.length > 0) {
        sections.push('');
        sections.push('No issues found.');
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: issues.some((i) => i.severity === 'error'),
      };
    },
  );
}
