import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';

interface PlanStep {
  name: string;
  deps: string[];
}

export function registerConcurrentPlanValidateTool(server: McpServer): void {
  server.registerTool(
    'gerbil_concurrent_plan_validate',
    {
      title: 'Concurrent Plan Validator',
      description:
        'Validate a :std/misc/concurrent-plan DAG execution plan. Checks that all ' +
        'dependencies reference existing steps, detects circular dependencies, verifies ' +
        'leaf steps have no unresolved dependencies, and estimates parallelism degree.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        steps: z
          .array(
            z.object({
              name: z.string().describe('Step name'),
              deps: z.array(z.string()).optional().describe('Dependencies (step names)'),
            }),
          )
          .describe('Plan steps with dependencies'),
      },
    },
    async ({ steps }) => {
      const planSteps: PlanStep[] = steps.map((s) => ({
        name: s.name,
        deps: s.deps || [],
      }));

      const stepNames = new Set(planSteps.map((s) => s.name));
      const issues: string[] = [];

      // Check 1: All deps reference existing steps
      for (const step of planSteps) {
        for (const dep of step.deps) {
          if (!stepNames.has(dep)) {
            issues.push(
              `Step '${step.name}' depends on '${dep}' which does not exist`,
            );
          }
        }
      }

      // Check 2: Duplicate step names
      const seen = new Set<string>();
      for (const step of planSteps) {
        if (seen.has(step.name)) {
          issues.push(`Duplicate step name: '${step.name}'`);
        }
        seen.add(step.name);
      }

      // Check 3: Self-dependency
      for (const step of planSteps) {
        if (step.deps.includes(step.name)) {
          issues.push(`Step '${step.name}' depends on itself`);
        }
      }

      // Check 4: Circular dependencies
      const visited = new Set<string>();
      const inStack = new Set<string>();
      let hasCycle = false;
      let cycleInfo = '';

      const depMap = new Map<string, string[]>();
      for (const step of planSteps) {
        depMap.set(step.name, step.deps);
      }

      function dfs(node: string, path: string[]): boolean {
        if (inStack.has(node)) {
          const cycleStart = path.indexOf(node);
          cycleInfo = path.slice(cycleStart).concat(node).join(' → ');
          return true;
        }
        if (visited.has(node)) return false;

        visited.add(node);
        inStack.add(node);
        path.push(node);

        for (const dep of depMap.get(node) || []) {
          if (stepNames.has(dep) && dfs(dep, [...path])) return true;
        }

        inStack.delete(node);
        return false;
      }

      for (const step of planSteps) {
        if (!visited.has(step.name)) {
          if (dfs(step.name, [])) {
            hasCycle = true;
            issues.push(`Circular dependency: ${cycleInfo}`);
            break;
          }
        }
      }

      // Calculate parallelism
      const levels = new Map<string, number>();

      function getLevel(name: string, visiting: Set<string> = new Set()): number {
        if (levels.has(name)) return levels.get(name)!;
        if (visiting.has(name)) return 0; // cycle guard
        visiting.add(name);

        const deps = depMap.get(name) || [];
        const validDeps = deps.filter((d) => stepNames.has(d));

        if (validDeps.length === 0) {
          levels.set(name, 0);
          return 0;
        }

        const maxDepLevel = Math.max(
          ...validDeps.map((d) => getLevel(d, new Set(visiting))),
        );
        const level = maxDepLevel + 1;
        levels.set(name, level);
        return level;
      }

      if (!hasCycle) {
        for (const step of planSteps) {
          getLevel(step.name);
        }
      }

      // Group steps by level for parallelism analysis
      const byLevel = new Map<number, string[]>();
      for (const [name, level] of levels) {
        if (!byLevel.has(level)) byLevel.set(level, []);
        byLevel.get(level)!.push(name);
      }

      const maxParallel = byLevel.size > 0
        ? Math.max(...[...byLevel.values()].map((l) => l.length))
        : 0;
      const totalLevels = byLevel.size;
      const leafSteps = planSteps.filter((s) => s.deps.length === 0);
      const rootSteps = planSteps.filter(
        (s) => !planSteps.some((other) => other.deps.includes(s.name)),
      );

      // Format output
      const sections: string[] = [
        '## Concurrent Plan Validation',
        '',
        `Steps: ${planSteps.length}`,
        `Leaf steps (no deps): ${leafSteps.length}`,
        `Terminal steps (no dependents): ${rootSteps.length}`,
        '',
      ];

      if (issues.length > 0) {
        sections.push(`### Issues (${issues.length})`);
        for (const issue of issues) {
          sections.push(`  - ${issue}`);
        }
        sections.push('');
      } else {
        sections.push('**No issues found.** Plan is valid.');
        sections.push('');
      }

      if (!hasCycle && totalLevels > 0) {
        sections.push('### Execution Schedule');
        sections.push(`Total phases: ${totalLevels}`);
        sections.push(`Max parallel degree: ${maxParallel}`);
        sections.push('');
        for (let level = 0; level < totalLevels; level++) {
          const stepsAtLevel = byLevel.get(level) || [];
          sections.push(`  Phase ${level}: ${stepsAtLevel.join(', ')} (${stepsAtLevel.length} parallel)`);
        }
        sections.push('');
      }

      // Generate Gerbil code snippet
      sections.push('### Gerbil Code Template');
      sections.push('```scheme');
      sections.push('(import :std/misc/concurrent-plan)');
      sections.push('');
      sections.push(`(def ${planSteps[0]?.name || 'my'}-plan`);
      sections.push('  (make-concurrent-plan');
      for (const step of planSteps) {
        if (step.deps.length > 0) {
          sections.push(`    (${step.name} (depends: (${step.deps.join(' ')})) (lambda () ;; TODO))`);
        } else {
          sections.push(`    (${step.name} (lambda () ;; TODO))`);
        }
      }
      sections.push('  ))');
      sections.push('```');
      sections.push('');
      sections.push('**Note**: Verify the concurrent-plan API with `gerbil_module_exports :std/misc/concurrent-plan`.');

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
