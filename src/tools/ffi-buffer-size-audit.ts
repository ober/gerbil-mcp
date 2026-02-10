import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { relative } from 'node:path';
import { scanSchemeFiles } from './parse-utils.js';

interface BufferAlloc {
  name: string;
  size: number;
  line: number;
  file: string;
}

interface CWriteSize {
  funcName: string;
  outputSize: number | null;
  outputParam: string;
  line: number;
  file: string;
}

interface BufferMismatch {
  schemeFunc: string;
  allocSize: number;
  allocFile: string;
  allocLine: number;
  cFunc: string;
  writeSize: number;
  cFile: string;
  cLine: number;
}

/**
 * Extract make-u8vector allocations with their sizes from Scheme files.
 */
function extractBufferAllocs(content: string, filePath: string): BufferAlloc[] {
  const allocs: BufferAlloc[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    // (make-u8vector N) or (make-u8vector N 0) etc.
    const pattern = /\(make-u8vector\s+(\d+)/g;
    let match;
    while ((match = pattern.exec(line)) !== null) {
      const beforeMatch = line.slice(0, match.index);
      if (beforeMatch.includes(';')) continue;

      // Try to find the enclosing function name
      const funcName = findEnclosingDef(lines, i);
      allocs.push({
        name: funcName || '<anonymous>',
        size: parseInt(match[1], 10),
        line: i + 1,
        file: filePath,
      });
    }
  }

  return allocs;
}

/**
 * Find enclosing def/define function name.
 */
function findEnclosingDef(lines: string[], lineIdx: number): string | null {
  for (let i = lineIdx; i >= Math.max(0, lineIdx - 30); i--) {
    const line = lines[i].trim();
    const match = line.match(/\(\s*(?:def|define)\s+\(?([a-zA-Z_!?<>=+\-*/][a-zA-Z0-9_!?<>=+\-*/.]*)/);
    if (match) return match[1];
  }
  return null;
}

/**
 * Extract C function output buffer size annotations from .scm files.
 * Looks for known patterns: array size declarations, sizeof usage, etc.
 */
function extractCWriteSizes(content: string, filePath: string): CWriteSize[] {
  const sizes: CWriteSize[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Match byte array declarations: byte out[96]  or  unsigned char buf[192]
    const arrayMatch = line.match(/(?:byte|unsigned\s+char|uint8_t|char)\s+(\w+)\[(\d+)\]/);
    if (arrayMatch) {
      sizes.push({
        funcName: findCFuncName(lines, i) || '<unknown>',
        outputSize: parseInt(arrayMatch[2], 10),
        outputParam: arrayMatch[1],
        line: i + 1,
        file: filePath,
      });
    }

    // Match explicit size comments or annotations: /* writes N bytes */
    const commentMatch = line.match(/\/\*\s*writes?\s+(\d+)\s+bytes?\s*\*\//i);
    if (commentMatch) {
      sizes.push({
        funcName: findCFuncName(lines, i) || '<unknown>',
        outputSize: parseInt(commentMatch[1], 10),
        outputParam: 'output',
        line: i + 1,
        file: filePath,
      });
    }

    // Match memcpy with known sizes
    const memcpyMatch = line.match(/memcpy\s*\([^,]+,\s*[^,]+,\s*(\d+)\s*\)/);
    if (memcpyMatch) {
      sizes.push({
        funcName: findCFuncName(lines, i) || '<unknown>',
        outputSize: parseInt(memcpyMatch[1], 10),
        outputParam: 'memcpy dest',
        line: i + 1,
        file: filePath,
      });
    }
  }

  return sizes;
}

function findCFuncName(lines: string[], lineIdx: number): string | null {
  for (let i = lineIdx; i >= Math.max(0, lineIdx - 20); i--) {
    const line = lines[i];
    // Match define-c-lambda name
    const match = line.match(/define-c-lambda\s+([a-zA-Z_!?<>=+\-*/][a-zA-Z0-9_!?<>=+\-*/.]*)/);
    if (match) return match[1];
    // Match C function definition
    const cfunc = line.match(/^\s*(?:\w+\s+)+(\w+)\s*\([^)]*\)\s*\{/);
    if (cfunc) return cfunc[1];
  }
  return null;
}

/**
 * Cross-reference buffer allocations with C write sizes.
 */
function findMismatches(
  allocs: BufferAlloc[],
  cSizes: CWriteSize[],
): BufferMismatch[] {
  const mismatches: BufferMismatch[] = [];

  // Heuristic: match by function name similarity
  for (const alloc of allocs) {
    for (const cSize of cSizes) {
      if (cSize.outputSize === null) continue;

      // Check if the alloc function calls the C function
      if (
        alloc.name.includes(cSize.funcName) ||
        cSize.funcName.includes(alloc.name) ||
        areFuncNamesRelated(alloc.name, cSize.funcName)
      ) {
        if (alloc.size < cSize.outputSize) {
          mismatches.push({
            schemeFunc: alloc.name,
            allocSize: alloc.size,
            allocFile: alloc.file,
            allocLine: alloc.line,
            cFunc: cSize.funcName,
            writeSize: cSize.outputSize,
            cFile: cSize.file,
            cLine: cSize.line,
          });
        }
      }
    }
  }

  return mismatches;
}

function areFuncNamesRelated(a: string, b: string): boolean {
  // Strip common prefixes/suffixes and compare
  const normalize = (s: string) => s.replace(/[_-]/g, '').toLowerCase();
  const na = normalize(a);
  const nb = normalize(b);
  return na.includes(nb) || nb.includes(na);
}

export function registerFfiBufferSizeAuditTool(server: McpServer): void {
  server.registerTool(
    'gerbil_ffi_buffer_size_audit',
    {
      title: 'FFI Buffer Size Audit',
      description:
        'Cross-reference C function output buffer sizes with Scheme-side make-u8vector ' +
        'allocations. Detects cases where a C function writes more bytes than the Scheme ' +
        'buffer allocates — a buffer overflow. Scans .scm files for array sizes and .ss ' +
        'files for make-u8vector allocations.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Single .ss file to audit'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory to audit all FFI files'),
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

      const files: string[] = [];
      if (file_path) {
        files.push(file_path);
      }
      if (project_path) {
        files.push(...await scanSchemeFiles(project_path));
      }

      const allAllocs: BufferAlloc[] = [];
      const allCSizes: CWriteSize[] = [];

      for (const f of files) {
        try {
          const content = await readFile(f, 'utf-8');
          if (f.endsWith('.ss')) {
            allAllocs.push(...extractBufferAllocs(content, f));
          }
          // Both .ss and .scm can contain C code in c-declare blocks
          allCSizes.push(...extractCWriteSizes(content, f));
        } catch { /* skip */ }
      }

      const basePath = project_path || '';
      const sections: string[] = [
        '## FFI Buffer Size Audit',
        '',
        `Files scanned: ${files.length}`,
        `Buffer allocations found: ${allAllocs.length}`,
        `C write-size annotations found: ${allCSizes.length}`,
        '',
      ];

      if (allAllocs.length === 0) {
        sections.push('No make-u8vector allocations found.');
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      // List all allocations
      sections.push('### Buffer Allocations');
      for (const a of allAllocs) {
        const rel = basePath ? relative(basePath, a.file) : a.file;
        sections.push(`  ${a.name}: make-u8vector ${a.size} bytes (${rel}:${a.line})`);
      }
      sections.push('');

      // Cross-reference and find mismatches
      const mismatches = findMismatches(allAllocs, allCSizes);

      if (mismatches.length > 0) {
        sections.push(`### Buffer Overflow Risks (${mismatches.length})`);
        sections.push('');
        for (const m of mismatches) {
          const schemeRel = basePath ? relative(basePath, m.allocFile) : m.allocFile;
          const cRel = basePath ? relative(basePath, m.cFile) : m.cFile;
          sections.push(
            `  **${m.schemeFunc}** allocates ${m.allocSize} bytes (${schemeRel}:${m.allocLine})`,
          );
          sections.push(
            `  but ${m.cFunc} writes ${m.writeSize} bytes (${cRel}:${m.cLine})`,
          );
          sections.push(
            `  → Buffer overflow: ${m.writeSize - m.allocSize} bytes past end`,
          );
          sections.push('');
        }
      } else {
        sections.push('No buffer size mismatches detected.');
        sections.push(
          '\n**Note**: This tool relies on C array size declarations and explicit size annotations.',
        );
        sections.push(
          'For complete coverage, annotate C wrapper functions with /* writes N bytes */ comments.',
        );
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
