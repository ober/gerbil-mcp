import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { relative } from 'node:path';
import { scanSchemeFiles, parseDefinitions } from './parse-utils.js';

interface InterfaceDef {
  name: string;
  methods: string[];
  file: string;
  line: number;
}

interface MethodImpl {
  method: string;
  type: string;
  file: string;
  line: number;
}

/**
 * Extract definterface declarations from source.
 */
function extractInterfaces(content: string, filePath: string): InterfaceDef[] {
  const interfaces: InterfaceDef[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();
    // (definterface Name (method1 method2 ...))
    const match = line.match(/\(\s*definterface\s+(\S+)\s+\(([^)]*)\)/);
    if (match) {
      const methods = match[2].split(/\s+/).filter(Boolean);
      interfaces.push({
        name: match[1],
        methods,
        file: filePath,
        line: i + 1,
      });
    }
  }

  return interfaces;
}

/**
 * Extract defmethod declarations.
 */
function extractMethodImpls(content: string, filePath: string): MethodImpl[] {
  const impls: MethodImpl[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();
    // (defmethod {method-name type} ...) or (defmethod {:method-name type} ...)
    const match = line.match(/\(\s*defmethod\s+\{:?(\S+)\s+(\S+)\}/);
    if (match) {
      impls.push({
        method: match[1],
        type: match[2],
        file: filePath,
        line: i + 1,
      });
    }
  }

  return impls;
}

/**
 * Extract defstruct/defclass type hierarchy.
 */
function extractTypes(content: string): Map<string, string[]> {
  const types = new Map<string, string[]>(); // type -> [parents]
  const lines = content.split('\n');

  for (const line of lines) {
    const trimmed = line.trim();
    // (defstruct (child parent) ...)
    const structMatch = trimmed.match(/\(\s*defstruct\s+\((\S+)\s+(\S+)\)/);
    if (structMatch) {
      types.set(structMatch[1], [structMatch[2]]);
      continue;
    }
    // (defstruct name ...) — no parent
    const simpleMatch = trimmed.match(/\(\s*defstruct\s+(\S+)\s/);
    if (simpleMatch) {
      types.set(simpleMatch[1], []);
      continue;
    }
    // (defclass name (parent1 parent2) ...)
    const classMatch = trimmed.match(/\(\s*defclass\s+(\S+)\s+\(([^)]*)\)/);
    if (classMatch) {
      const parents = classMatch[2].split(/\s+/).filter(Boolean);
      types.set(classMatch[1], parents);
    }
  }

  return types;
}

export function registerInterfaceComplianceCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_interface_compliance_check',
    {
      title: 'Interface Compliance Check',
      description:
        'Verify that struct/class types implement all methods required by an interface. ' +
        'Cross-references definterface declarations with defmethod implementations to find ' +
        'missing method implementations. Reports which types are missing which methods.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Single .ss file to check'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory to check'),
        interface_name: z
          .string()
          .optional()
          .describe('Specific interface to check (default: check all)'),
      },
    },
    async ({ file_path, project_path, interface_name }) => {
      if (!file_path && !project_path) {
        return {
          content: [{
            type: 'text' as const,
            text: 'Either file_path or project_path is required.',
          }],
          isError: true,
        };
      }

      const files: string[] = [];
      if (file_path) files.push(file_path);
      if (project_path) files.push(...await scanSchemeFiles(project_path));

      const allInterfaces: InterfaceDef[] = [];
      const allImpls: MethodImpl[] = [];
      const allTypes = new Map<string, string[]>();

      for (const f of files) {
        try {
          const content = await readFile(f, 'utf-8');
          allInterfaces.push(...extractInterfaces(content, f));
          allImpls.push(...extractMethodImpls(content, f));
          const types = extractTypes(content);
          for (const [name, parents] of types) {
            allTypes.set(name, parents);
          }
        } catch { /* skip */ }
      }

      if (allInterfaces.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: 'No definterface declarations found.',
          }],
        };
      }

      // Filter to specific interface if requested
      const interfaces = interface_name
        ? allInterfaces.filter((i) => i.name === interface_name)
        : allInterfaces;

      if (interfaces.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: `Interface '${interface_name}' not found.`,
          }],
          isError: true,
        };
      }

      // Build method impl map: type -> Set<methods>
      const implsByType = new Map<string, Set<string>>();
      for (const impl of allImpls) {
        if (!implsByType.has(impl.type)) implsByType.set(impl.type, new Set());
        implsByType.get(impl.type)!.add(impl.method);
      }

      // Get all concrete types that should implement interfaces
      const concreteTypes = [...allTypes.keys()];

      const basePath = project_path || '';
      const sections: string[] = [
        '## Interface Compliance Report',
        '',
        `Interfaces: ${interfaces.length}`,
        `Types: ${concreteTypes.length}`,
        `Method implementations: ${allImpls.length}`,
        '',
      ];

      let totalMissing = 0;

      for (const iface of interfaces) {
        sections.push(`### Interface: ${iface.name}`);
        sections.push(`Required methods: ${iface.methods.join(', ')}`);
        sections.push('');

        // Check each type
        for (const typeName of concreteTypes) {
          const typeMethods = implsByType.get(typeName) || new Set();

          // Check parent hierarchy too
          const allMethods = new Set(typeMethods);
          const visited = new Set<string>();
          const queue = allTypes.get(typeName) || [];
          const toVisit = [...queue];
          while (toVisit.length > 0) {
            const parent = toVisit.pop()!;
            if (visited.has(parent)) continue;
            visited.add(parent);
            const parentMethods = implsByType.get(parent);
            if (parentMethods) {
              for (const m of parentMethods) allMethods.add(m);
            }
            const grandparents = allTypes.get(parent) || [];
            toVisit.push(...grandparents);
          }

          const missing = iface.methods.filter((m) => !allMethods.has(m));
          if (missing.length > 0) {
            totalMissing += missing.length;
            sections.push(
              `  **${typeName}**: missing ${missing.join(', ')}`,
            );
          }
        }

        if (totalMissing === 0) {
          sections.push('  All types implement all required methods.');
        }
        sections.push('');
      }

      if (totalMissing > 0) {
        sections.push(
          `\n**${totalMissing} missing method implementation(s) found.**`,
        );
        sections.push(
          'Use gerbil_class_info to verify type hierarchies at runtime.',
        );
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
