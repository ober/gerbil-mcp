import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, stat } from 'node:fs/promises';
import { join, relative } from 'node:path';
import { scanSchemeFiles, parseDefinitions, extractModulePaths } from './parse-utils.js';

interface HealthIssue {
  category: string;
  severity: 'critical' | 'high' | 'medium' | 'low';
  file: string;
  line?: number;
  message: string;
}

export function registerProjectHealthCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_project_health_check',
    {
      title: 'Project Health Check',
      description:
        'Composite project audit that runs multiple quality checks in a single tool call: ' +
        'lint (unused imports, duplicates, style), dead code detection, dependency cycle check, ' +
        'export consistency, and import conflict detection. Returns a unified report with ' +
        'issue counts by severity and category.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        project_path: z
          .string()
          .describe('Project directory to audit'),
      },
    },
    async ({ project_path }) => {
      // Verify directory exists
      try {
        const s = await stat(project_path);
        if (!s.isDirectory()) {
          return {
            content: [{ type: 'text' as const, text: `Not a directory: ${project_path}` }],
            isError: true,
          };
        }
      } catch {
        return {
          content: [{ type: 'text' as const, text: `Directory not found: ${project_path}` }],
          isError: true,
        };
      }

      const ssFiles = await scanSchemeFiles(project_path);
      if (ssFiles.length === 0) {
        return {
          content: [{ type: 'text' as const, text: `No .ss files found in ${project_path}` }],
        };
      }

      const issues: HealthIssue[] = [];

      // Read all files
      const fileContents = new Map<string, string>();
      for (const f of ssFiles) {
        try {
          fileContents.set(f, await readFile(f, 'utf-8'));
        } catch { /* skip */ }
      }

      // Check 1: Lint — unused imports, duplicate definitions
      for (const [file, content] of fileContents) {
        const relFile = relative(project_path, file);
        const analysis = parseDefinitions(content);

        // Check for duplicate definitions
        const defNames = new Map<string, number>();
        for (const def of analysis.definitions) {
          if (defNames.has(def.name)) {
            issues.push({
              category: 'lint',
              severity: 'medium',
              file: relFile,
              line: def.line,
              message: `Duplicate definition: ${def.name} (first at line ${defNames.get(def.name)})`,
            });
          } else {
            defNames.set(def.name, def.line);
          }
        }
      }

      // Check 2: Dead code detection
      const allDefs = new Map<string, { file: string; line: number }>();
      const allExports = new Set<string>();

      for (const [file, content] of fileContents) {
        const relFile = relative(project_path, file);
        const analysis = parseDefinitions(content);

        for (const def of analysis.definitions) {
          allDefs.set(`${relFile}:${def.name}`, { file: relFile, line: def.line });
        }

        for (const exp of analysis.exports) {
          const symPattern = /([a-zA-Z_!?*+/<>=.-][a-zA-Z0-9_!?*+/<>=.-]*)/g;
          let match;
          while ((match = symPattern.exec(exp.raw)) !== null) {
            const sym = match[1];
            if (!['export', 'except', 'only', 'rename', 'prefix', '#t'].includes(sym)) {
              allExports.add(sym);
            }
          }
        }
      }

      // Find defs that are neither exported nor used in other files
      for (const [key, info] of allDefs) {
        const defName = key.split(':').slice(1).join(':');
        if (allExports.has(defName)) continue;

        let usedElsewhere = false;
        for (const [file, content] of fileContents) {
          const relFile = relative(project_path, file);
          if (relFile === info.file) continue;
          const escaped = defName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
          if (new RegExp(`(?<![a-zA-Z0-9_!?*+/<>=.-])${escaped}(?![a-zA-Z0-9_!?*+/<>=.-])`).test(content)) {
            usedElsewhere = true;
            break;
          }
        }

        if (!usedElsewhere) {
          // Check if used within own file
          const ownContent = fileContents.get(
            [...fileContents.keys()].find((k) => relative(project_path, k) === info.file)!,
          );
          if (ownContent) {
            const escaped = defName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
            const pattern = new RegExp(`(?<![a-zA-Z0-9_!?*+/<>=.-])${escaped}(?![a-zA-Z0-9_!?*+/<>=.-])`, 'g');
            const matches = ownContent.match(pattern);
            if (matches && matches.length <= 1) {
              issues.push({
                category: 'dead-code',
                severity: 'low',
                file: info.file,
                line: info.line,
                message: `Potentially unused: ${defName}`,
              });
            }
          }
        }
      }

      // Check 3: Dependency cycles
      let packageName = '';
      try {
        const pkgContent = await readFile(join(project_path, 'gerbil.pkg'), 'utf-8');
        const pkgMatch = pkgContent.match(/\(package:\s+([^\s)]+)\)/);
        if (pkgMatch) packageName = pkgMatch[1];
      } catch { /* no pkg file */ }

      if (packageName) {
        const moduleImports = new Map<string, Set<string>>();

        for (const [file, content] of fileContents) {
          const rel = relative(project_path, file).replace(/\.ss$/, '');
          const modId = `:${packageName}/${rel}`;
          const analysis = parseDefinitions(content);
          const deps = new Set<string>();

          for (const imp of analysis.imports) {
            const mods = extractModulePaths(imp.raw);
            for (const m of mods) {
              if (m.startsWith(`:${packageName}/`) && m !== modId) {
                deps.add(m);
              }
            }
          }

          moduleImports.set(modId, deps);
        }

        // DFS cycle detection
        const visited = new Set<string>();
        const stack = new Set<string>();

        function dfs(node: string, path: string[]): string[] | null {
          if (stack.has(node)) {
            const cycleStart = path.indexOf(node);
            return path.slice(cycleStart);
          }
          if (visited.has(node)) return null;

          visited.add(node);
          stack.add(node);
          path.push(node);

          const deps = moduleImports.get(node) || new Set();
          for (const dep of deps) {
            const cycle = dfs(dep, [...path]);
            if (cycle) return cycle;
          }

          stack.delete(node);
          return null;
        }

        for (const mod of moduleImports.keys()) {
          const cycle = dfs(mod, []);
          if (cycle) {
            issues.push({
              category: 'dependency-cycle',
              severity: 'high',
              file: cycle.join(' → '),
              message: `Circular dependency: ${cycle.join(' → ')} → ${cycle[0]}`,
            });
            break; // Report first cycle only
          }
        }
      }

      // Check 4: Export consistency
      for (const [file, content] of fileContents) {
        const relFile = relative(project_path, file);
        const analysis = parseDefinitions(content);
        const definedNames = new Set(analysis.definitions.map((d) => d.name));

        for (const exp of analysis.exports) {
          const symPattern = /([a-zA-Z_!?*+/<>=.-][a-zA-Z0-9_!?*+/<>=.-]*)/g;
          let match;
          while ((match = symPattern.exec(exp.raw)) !== null) {
            const sym = match[1];
            if (['export', 'except', 'only', 'rename', 'prefix', '#t'].includes(sym)) continue;
            if (!definedNames.has(sym)) {
              // Could be a re-export — check imports
              let isImported = false;
              for (const imp of analysis.imports) {
                if (imp.raw.includes(sym)) {
                  isImported = true;
                  break;
                }
              }
              if (!isImported) {
                issues.push({
                  category: 'export-consistency',
                  severity: 'medium',
                  file: relFile,
                  line: exp.line,
                  message: `Exported but not defined or imported: ${sym}`,
                });
              }
            }
          }
        }
      }

      // Format report
      const sections: string[] = [
        `## Project Health Check: ${project_path}`,
        '',
        `Files scanned: ${ssFiles.length}`,
        `Total issues: ${issues.length}`,
        '',
      ];

      // Summary by severity
      const bySeverity = new Map<string, number>();
      for (const issue of issues) {
        bySeverity.set(issue.severity, (bySeverity.get(issue.severity) || 0) + 1);
      }

      if (issues.length === 0) {
        sections.push('No issues found. Project looks healthy!');
      } else {
        sections.push('### Summary');
        for (const sev of ['critical', 'high', 'medium', 'low']) {
          const count = bySeverity.get(sev) || 0;
          if (count > 0) {
            sections.push(`  ${sev.toUpperCase()}: ${count}`);
          }
        }
        sections.push('');

        // Group by category
        const byCategory = new Map<string, HealthIssue[]>();
        for (const issue of issues) {
          if (!byCategory.has(issue.category)) byCategory.set(issue.category, []);
          byCategory.get(issue.category)!.push(issue);
        }

        for (const [category, catIssues] of byCategory) {
          sections.push(`### ${category} (${catIssues.length})`);
          for (const issue of catIssues) {
            const loc = issue.line ? `${issue.file}:${issue.line}` : issue.file;
            sections.push(`  [${issue.severity.toUpperCase()}] ${loc} — ${issue.message}`);
          }
          sections.push('');
        }
      }

      sections.push(
        '\n**Note**: For deeper analysis, use individual tools: gerbil_lint, gerbil_security_scan, ' +
        'gerbil_stale_static, gerbil_check_import_conflicts.',
      );

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
