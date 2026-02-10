import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join, dirname, relative } from 'node:path';
import { scanSchemeFiles } from './parse-utils.js';

interface NullDeref {
  accessor: string;
  field: string;
  line: number;
  file: string;
}

interface WrapperCall {
  accessor: string;
  line: number;
  file: string;
  guarded: boolean;
  guardReason?: string;
}

/**
 * Extract c-lambda accessors that dereference pointer arguments without null checks.
 * Looks for patterns like: ___arg1->field or ___arg1.field in c-lambda bodies.
 */
function extractPointerDerefs(content: string, filePath: string): NullDeref[] {
  const derefs: NullDeref[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Match c-lambda or define-c-lambda with inline C code that dereferences args
    // Pattern: ___arg1->field or ___arg1.field (also ___arg2, etc.)
    const derefPattern = /___arg(\d+)->(\w+)/g;
    let match;
    while ((match = derefPattern.exec(line)) !== null) {
      // Check if there's a null check for this arg in nearby lines
      const context = lines.slice(Math.max(0, i - 5), i + 5).join('\n');
      const argName = `___arg${match[1]}`;
      const hasNullCheck =
        context.includes(`${argName} != NULL`) ||
        context.includes(`${argName} == NULL`) ||
        context.includes(`!${argName}`) ||
        context.includes(`if (${argName})`) ||
        context.includes(`if(${argName})`);

      if (!hasNullCheck) {
        // Find the accessor name — scan backwards for the define-c-lambda or def name
        const accessorName = findAccessorName(lines, i);
        if (accessorName) {
          derefs.push({
            accessor: accessorName,
            field: match[2],
            line: i + 1,
            file: filePath,
          });
        }
      }
    }

    // Also match cast-then-deref patterns: ((TypeName*)___arg1)->field
    const castDerefPattern = /\(\([A-Za-z_]\w*\s*\*\)\s*___arg(\d+)\)->(\w+)/g;
    while ((match = castDerefPattern.exec(line)) !== null) {
      const context = lines.slice(Math.max(0, i - 5), i + 5).join('\n');
      const argName = `___arg${match[1]}`;
      const hasNullCheck =
        context.includes(`${argName} != NULL`) ||
        context.includes(`${argName} == NULL`) ||
        context.includes(`!${argName}`) ||
        context.includes(`if (${argName})`) ||
        context.includes(`if(${argName})`);

      if (!hasNullCheck) {
        const accessorName = findAccessorName(lines, i);
        if (accessorName) {
          derefs.push({
            accessor: accessorName,
            field: match[2],
            line: i + 1,
            file: filePath,
          });
        }
      }
    }
  }

  return derefs;
}

/**
 * Scan backwards from a line to find the enclosing define-c-lambda or def name.
 */
function findAccessorName(lines: string[], lineIdx: number): string | null {
  for (let i = lineIdx; i >= Math.max(0, lineIdx - 20); i--) {
    const line = lines[i];
    // define-c-lambda name
    const dcl = line.match(/\(\s*define-c-lambda\s+([a-zA-Z_!?<>=+\-*/][a-zA-Z0-9_!?<>=+\-*/.]*)/);
    if (dcl) return dcl[1];
    // (def name (c-lambda ...))
    const defcl = line.match(/\(\s*(?:def|define)\s+([a-zA-Z_!?<>=+\-*/][a-zA-Z0-9_!?<>=+\-*/.]*)\s+\(\s*c-lambda/);
    if (defcl) return defcl[1];
  }
  return null;
}

/**
 * Check if a call to an accessor is guarded by a null/truthy check in a .ss wrapper file.
 */
function findWrapperCalls(
  content: string,
  filePath: string,
  accessorNames: Set<string>,
): WrapperCall[] {
  const calls: WrapperCall[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    for (const accessor of accessorNames) {
      const escaped = accessor.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
      const pattern = new RegExp(`\\(${escaped}\\s`, 'g');
      if (pattern.test(line)) {
        // Check context for guards
        const context = lines.slice(Math.max(0, i - 5), i + 1).join('\n');
        const guarded = isCallGuarded(context, accessor, line);
        calls.push({
          accessor,
          line: i + 1,
          file: filePath,
          guarded: guarded.is,
          guardReason: guarded.reason,
        });
      }
    }
  }

  return calls;
}

function isCallGuarded(
  context: string,
  _accessor: string,
  _line: string,
): { is: boolean; reason?: string } {
  // Check for common guard patterns
  if (context.match(/\(if\s+\S+/)) {
    return { is: true, reason: 'guarded by if' };
  }
  if (context.match(/\(when\s+\S+/)) {
    return { is: true, reason: 'guarded by when' };
  }
  if (context.match(/\(and\s+\S+/)) {
    return { is: true, reason: 'guarded by and (short-circuit)' };
  }
  if (context.match(/\(cond\b/)) {
    return { is: true, reason: 'inside cond branch' };
  }
  if (context.match(/\(match\b/)) {
    return { is: true, reason: 'inside match' };
  }
  return { is: false };
}

export function registerFfiNullSafetyTool(server: McpServer): void {
  server.registerTool(
    'gerbil_ffi_null_safety',
    {
      title: 'FFI Null-Pointer Safety Audit',
      description:
        'Statically analyze .scm FFI files for c-lambda declarations that dereference ' +
        'pointer arguments (___arg1->field) without null checks. Cross-references with .ss ' +
        'wrapper files to check whether calls are guarded. Reports each accessor that could ' +
        'segfault on NULL input.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Single .scm file to audit'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory to audit all .scm and .ss files'),
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

      const scmFiles: string[] = [];
      const ssFiles: string[] = [];

      if (file_path) {
        if (file_path.endsWith('.scm')) {
          scmFiles.push(file_path);
          // Also look for companion .ss file in same directory
          const ssFile = file_path.replace(/\.scm$/, '.ss');
          ssFiles.push(ssFile);
          // Also look for other .ss files in the directory
          const dir = dirname(file_path);
          try {
            const allFiles = await scanSchemeFiles(dir);
            for (const f of allFiles) {
              if (f.endsWith('.ss') && !ssFiles.includes(f)) ssFiles.push(f);
            }
          } catch { /* ignore */ }
        } else {
          return {
            content: [
              {
                type: 'text' as const,
                text: 'file_path should be a .scm file containing FFI declarations.',
              },
            ],
            isError: true,
          };
        }
      } else if (project_path) {
        const allFiles = await scanSchemeFiles(project_path);
        for (const f of allFiles) {
          if (f.endsWith('.scm')) scmFiles.push(f);
          else if (f.endsWith('.ss')) ssFiles.push(f);
        }
      }

      if (scmFiles.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'No .scm files found to audit.',
            },
          ],
        };
      }

      // Phase 1: Find unguarded pointer dereferences in .scm files
      const allDerefs: NullDeref[] = [];
      for (const f of scmFiles) {
        try {
          const content = await readFile(f, 'utf-8');
          allDerefs.push(...extractPointerDerefs(content, f));
        } catch { /* skip */ }
      }

      if (allDerefs.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No unguarded pointer dereferences found in ${scmFiles.length} .scm file(s).`,
            },
          ],
        };
      }

      // Phase 2: Check wrapper .ss files for guarded calls
      const accessorNames = new Set(allDerefs.map((d) => d.accessor));
      const allWrapperCalls: WrapperCall[] = [];
      for (const f of ssFiles) {
        try {
          const content = await readFile(f, 'utf-8');
          allWrapperCalls.push(...findWrapperCalls(content, f, accessorNames));
        } catch { /* skip */ }
      }

      // Build call-site map
      const callsByAccessor = new Map<string, WrapperCall[]>();
      for (const call of allWrapperCalls) {
        if (!callsByAccessor.has(call.accessor)) {
          callsByAccessor.set(call.accessor, []);
        }
        callsByAccessor.get(call.accessor)!.push(call);
      }

      // Format output
      const basePath = project_path || dirname(file_path!);
      const sections: string[] = [
        '## FFI Null-Pointer Safety Audit',
        '',
        `Scanned: ${scmFiles.length} .scm file(s), ${ssFiles.length} .ss file(s)`,
        `Unguarded dereferences found: ${allDerefs.length}`,
        '',
      ];

      let unguardedCount = 0;
      for (const deref of allDerefs) {
        const relFile = relative(basePath, deref.file);
        const calls = callsByAccessor.get(deref.accessor) || [];
        const unguardedCalls = calls.filter((c) => !c.guarded);
        const guardedCalls = calls.filter((c) => c.guarded);

        if (unguardedCalls.length > 0 || calls.length === 0) {
          unguardedCount++;
          sections.push(
            `### ${deref.accessor} (${relFile}:${deref.line})`,
          );
          sections.push(
            `  Dereferences: ___arg->**${deref.field}** without null check`,
          );

          if (calls.length === 0) {
            sections.push('  Call sites: none found in .ss files');
          } else {
            if (guardedCalls.length > 0) {
              for (const c of guardedCalls) {
                sections.push(
                  `  GUARDED at ${relative(basePath, c.file)}:${c.line} (${c.guardReason})`,
                );
              }
            }
            if (unguardedCalls.length > 0) {
              for (const c of unguardedCalls) {
                sections.push(
                  `  **UNGUARDED** at ${relative(basePath, c.file)}:${c.line}`,
                );
              }
            }
          }
          sections.push('');
        }
      }

      if (unguardedCount === 0) {
        sections.push(
          'All pointer dereferences are guarded at their call sites.',
        );
      } else {
        sections.push(
          `\n**${unguardedCount} accessor(s) may segfault on NULL input.**`,
        );
        sections.push(
          'Remediation: Add null checks in the C code or guard calls in Scheme wrappers.',
        );
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
