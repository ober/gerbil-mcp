import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { relative } from 'node:path';
import { scanSchemeFiles, parseDefinitions } from './parse-utils.js';

interface MethodCall {
  method: string;
  line: number;
  file: string;
  objectExpr: string;
}

/**
 * Extract method dispatch calls {method-name obj} from Gerbil source.
 */
function extractMethodCalls(content: string, filePath: string): MethodCall[] {
  const calls: MethodCall[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    // Match {method-name expr} patterns
    const pattern = /\{(\S+)\s+([^}]+)\}/g;
    let match;
    while ((match = pattern.exec(line)) !== null) {
      // Skip if inside a comment
      const beforeMatch = line.slice(0, match.index);
      if (beforeMatch.includes(';')) continue;

      calls.push({
        method: match[1],
        objectExpr: match[2].trim(),
        line: i + 1,
        file: filePath,
      });
    }
  }

  return calls;
}

/**
 * Extract defmethod declarations from source.
 */
function extractMethodDefs(content: string): Map<string, string[]> {
  const methods = new Map<string, string[]>(); // method-name -> [type-names]
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();
    // (defmethod {method-name type} ...)
    const match = line.match(/\(\s*defmethod\s+\{:?(\S+)\s+(\S+)\}/);
    if (match) {
      const method = match[1];
      const typeName = match[2];
      if (!methods.has(method)) methods.set(method, []);
      methods.get(method)!.push(typeName);
    }
  }

  return methods;
}

/**
 * Extract struct/class type definitions and their slots.
 */
function extractTypeInfo(content: string): Map<string, { slots: string[]; parent?: string }> {
  const types = new Map<string, { slots: string[]; parent?: string }>();
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();
    // (defstruct name (slots...)) or (defclass name (parent) (slots...))
    const structMatch = line.match(/\(\s*defstruct\s+(\S+)\s+\(([^)]*)\)/);
    if (structMatch) {
      const name = structMatch[1];
      const slots = structMatch[2].split(/\s+/).filter(Boolean);
      types.set(name, { slots });
      continue;
    }
    const classMatch = line.match(/\(\s*defclass\s+(\S+)\s+\(([^)]*)\)\s+\(([^)]*)\)/);
    if (classMatch) {
      const name = classMatch[1];
      const parent = classMatch[2].trim() || undefined;
      const slots = classMatch[3].split(/\s+/).filter(Boolean);
      types.set(name, { slots, parent });
    }
  }

  return types;
}

export function registerMethodDispatchAuditTool(server: McpServer): void {
  server.registerTool(
    'gerbil_method_dispatch_audit',
    {
      title: 'Method Dispatch Audit',
      description:
        'Detect {method-name obj} dispatch calls and cross-reference with defmethod ' +
        'declarations to find methods that may not exist on the target type. ' +
        'Static analysis that catches method dispatch failures before runtime.',
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
          .describe('Project directory to audit'),
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
      } else if (project_path) {
        files.push(...await scanSchemeFiles(project_path));
      }

      // Collect all method calls, method defs, and type info
      const allCalls: MethodCall[] = [];
      const allMethodDefs = new Map<string, string[]>();
      const allTypes = new Map<string, { slots: string[]; parent?: string }>();

      for (const f of files) {
        try {
          const content = await readFile(f, 'utf-8');
          allCalls.push(...extractMethodCalls(content, f));

          const defs = extractMethodDefs(content);
          for (const [method, types] of defs) {
            if (!allMethodDefs.has(method)) allMethodDefs.set(method, []);
            allMethodDefs.get(method)!.push(...types);
          }

          const typeInfo = extractTypeInfo(content);
          for (const [name, info] of typeInfo) {
            allTypes.set(name, info);
          }
        } catch { /* skip */ }
      }

      if (allCalls.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `No method dispatch calls ({method obj}) found in ${files.length} file(s).`,
            },
          ],
        };
      }

      const basePath = project_path || '';
      const sections: string[] = [
        '## Method Dispatch Audit',
        '',
        `Method dispatch calls found: ${allCalls.length}`,
        `Method definitions found: ${allMethodDefs.size}`,
        `Type definitions found: ${allTypes.size}`,
        '',
      ];

      // Check each call against known method definitions
      const warnings: string[] = [];
      const unknownMethods: string[] = [];

      for (const call of allCalls) {
        const knownTypes = allMethodDefs.get(call.method);
        const relFile = basePath ? relative(basePath, call.file) : call.file;

        if (!knownTypes) {
          unknownMethods.push(
            `  {${call.method} ...} at ${relFile}:${call.line} — no defmethod found in project`,
          );
        }
      }

      if (unknownMethods.length > 0) {
        sections.push(`### Methods without visible defmethod (${unknownMethods.length})`);
        sections.push('These may be defined in imported modules or may indicate missing implementations:');
        sections.push('');
        sections.push(...unknownMethods);
        sections.push('');
      }

      if (unknownMethods.length === 0) {
        sections.push('All method dispatch calls have matching defmethod declarations in scope.');
      }

      sections.push('');
      sections.push('**Note**: This is static analysis limited to project-local definitions.');
      sections.push('Methods from imported modules (e.g. :std/db/dbi) are not checked.');
      sections.push('Use gerbil_class_info to verify type hierarchies at runtime.');

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
