import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { execSync } from 'node:child_process';
import { join, dirname } from 'node:path';

interface BenchmarkEntry {
  name: string;
  value: number;
  unit: string;
}

interface BenchmarkResult {
  timestamp: string;
  command: string;
  label: string;
  entries: BenchmarkEntry[];
  rawOutput: string;
}

/**
 * Parse benchmark output to extract timing/performance numbers.
 * Supports common formats:
 * - "name: 1.234s" / "name: 1234ms"
 * - "name 1.234" (tab or space separated)
 * - shellbench format: "name  1234.5  ops/sec"
 * - Gerbil benchmark format: "wall: 1.234s cpu: 0.987s"
 */
function parseBenchmarkOutput(output: string): BenchmarkEntry[] {
  const entries: BenchmarkEntry[] = [];
  const lines = output.split('\n');

  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith('#') || trimmed.startsWith('-')) continue;

    // Format: "name: 1.234s" or "name: 1234ms"
    const colonMatch = trimmed.match(
      /^(.+?):\s+([\d.]+)\s*(s|ms|us|ns|μs|ops\/sec|op\/s|MB|KB|GB)/i,
    );
    if (colonMatch) {
      entries.push({
        name: colonMatch[1].trim(),
        value: parseFloat(colonMatch[2]),
        unit: colonMatch[3],
      });
      continue;
    }

    // Format: "name\t1234.5\tops/sec" (tab separated, shellbench-style)
    const tabMatch = trimmed.match(
      /^(.+?)\t+([\d.]+)\s*(s|ms|us|ns|μs|ops\/sec|op\/s|MB|KB|GB)?/i,
    );
    if (tabMatch) {
      entries.push({
        name: tabMatch[1].trim(),
        value: parseFloat(tabMatch[2]),
        unit: tabMatch[3] || 'units',
      });
      continue;
    }

    // Format: "wall: 1.234s" (Gerbil benchmark tool output)
    const gerbilMatch = trimmed.match(
      /\b(wall|cpu|gc|alloc|real|user|sys)\b[:\s]+([\d.]+)\s*(s|ms|us|ns|μs|MB|KB|bytes)?/i,
    );
    if (gerbilMatch) {
      entries.push({
        name: gerbilMatch[1],
        value: parseFloat(gerbilMatch[2]),
        unit: gerbilMatch[3] || 's',
      });
    }
  }

  return entries;
}

/**
 * Compare two benchmark results and format a comparison table.
 */
function formatComparison(
  baseline: BenchmarkResult,
  current: BenchmarkResult,
): string {
  const sections: string[] = [];

  sections.push('Benchmark Comparison');
  sections.push(`  Baseline: ${baseline.label} (${baseline.timestamp})`);
  sections.push(`  Current:  ${current.label} (${current.timestamp})`);
  sections.push(`  Command:  ${current.command}`);
  sections.push('');

  // Match entries by name
  const baseMap = new Map(
    baseline.entries.map((e) => [e.name, e]),
  );

  const rows: Array<{
    name: string;
    baseVal: number;
    currVal: number;
    unit: string;
    pctChange: number;
  }> = [];

  for (const curr of current.entries) {
    const base = baseMap.get(curr.name);
    if (base) {
      const pctChange =
        base.value !== 0
          ? ((curr.value - base.value) / base.value) * 100
          : 0;
      rows.push({
        name: curr.name,
        baseVal: base.value,
        currVal: curr.value,
        unit: curr.unit,
        pctChange,
      });
    }
  }

  if (rows.length === 0) {
    sections.push('No matching entries found for comparison.');
    sections.push('');
    sections.push('Baseline entries:');
    for (const e of baseline.entries) {
      sections.push(`  ${e.name}: ${e.value} ${e.unit}`);
    }
    sections.push('Current entries:');
    for (const e of current.entries) {
      sections.push(`  ${e.name}: ${e.value} ${e.unit}`);
    }
  } else {
    // Format table
    const nameWidth = Math.max(
      8,
      ...rows.map((r) => r.name.length),
    );

    sections.push(
      `${'Metric'.padEnd(nameWidth)}  ${'Baseline'.padStart(12)}  ${'Current'.padStart(12)}  ${'Change'.padStart(10)}`,
    );
    sections.push('-'.repeat(nameWidth + 40));

    for (const row of rows) {
      const baseStr = `${row.baseVal.toFixed(3)} ${row.unit}`;
      const currStr = `${row.currVal.toFixed(3)} ${row.unit}`;
      const sign = row.pctChange >= 0 ? '+' : '';
      const pctStr = `${sign}${row.pctChange.toFixed(1)}%`;
      // For time metrics, negative is better (faster)
      const isTime = /^(s|ms|us|ns|μs)$/i.test(row.unit);
      const indicator = isTime
        ? row.pctChange < -1
          ? ' (faster)'
          : row.pctChange > 1
            ? ' (slower)'
            : ' (same)'
        : row.pctChange > 1
          ? ' (higher)'
          : row.pctChange < -1
            ? ' (lower)'
            : ' (same)';

      sections.push(
        `${row.name.padEnd(nameWidth)}  ${baseStr.padStart(12)}  ${currStr.padStart(12)}  ${(pctStr + indicator).padStart(10)}`,
      );
    }
  }

  return sections.join('\n');
}

export function registerBenchmarkCompareTool(server: McpServer): void {
  server.registerTool(
    'gerbil_benchmark_compare',
    {
      title: 'Benchmark Compare with Baseline',
      description:
        'Run a benchmark command, save results as a baseline, and compare against ' +
        'previous baselines. Shows side-by-side comparison with percentage changes. ' +
        'Use save_as to save a baseline, compare_with to compare against a saved baseline. ' +
        'Supports common output formats (time, ops/sec, memory) and shellbench output.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        command: z
          .string()
          .optional()
          .describe(
            'Shell command to run as benchmark (e.g., "shellbench -s gsh -t 3"). ' +
            'If omitted, uses benchmark_output instead.',
          ),
        benchmark_output: z
          .string()
          .optional()
          .describe(
            'Benchmark output text to parse directly (alternative to running a command)',
          ),
        save_as: z
          .string()
          .optional()
          .describe(
            'Save the results as a named baseline (e.g., "before-optimization"). ' +
            'Saved to .gerbil-benchmarks/ in the current directory.',
          ),
        compare_with: z
          .string()
          .optional()
          .describe(
            'Name of a saved baseline to compare against (e.g., "before-optimization")',
          ),
        baseline_dir: z
          .string()
          .optional()
          .describe(
            'Directory to store baseline files (default: .gerbil-benchmarks/)',
          ),
        label: z
          .string()
          .optional()
          .describe(
            'Label for this benchmark run (e.g., "after pcre integration")',
          ),
        timeout: z
          .number()
          .optional()
          .describe('Timeout in milliseconds for the command (default: 60000)'),
      },
    },
    async ({
      command,
      benchmark_output,
      save_as,
      compare_with,
      baseline_dir,
      label,
      timeout,
    }) => {
      if (!command && !benchmark_output) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Either command or benchmark_output is required.',
            },
          ],
          isError: true,
        };
      }

      const effectiveTimeout = timeout ?? 60_000;
      const benchDir = baseline_dir ?? '.gerbil-benchmarks';
      let output: string;
      const cmdStr = command ?? '(direct output)';

      if (command) {
        try {
          output = execSync(command, {
            encoding: 'utf-8',
            timeout: effectiveTimeout,
            stdio: ['pipe', 'pipe', 'pipe'],
          });
        } catch (err: unknown) {
          const msg = err instanceof Error ? err.message : String(err);
          // execSync might have stdout even on non-zero exit
          if (
            typeof err === 'object' &&
            err !== null &&
            'stdout' in err
          ) {
            output = String((err as { stdout: unknown }).stdout);
          } else {
            return {
              content: [
                {
                  type: 'text' as const,
                  text: `Benchmark command failed: ${msg}`,
                },
              ],
              isError: true,
            };
          }
        }
      } else {
        output = benchmark_output!;
      }

      const entries = parseBenchmarkOutput(output);
      const now = new Date().toISOString();
      const resultLabel = label ?? cmdStr;

      const currentResult: BenchmarkResult = {
        timestamp: now,
        command: cmdStr,
        label: resultLabel,
        entries,
        rawOutput: output,
      };

      const sections: string[] = [];

      // Save baseline if requested
      if (save_as) {
        try {
          await mkdir(benchDir, { recursive: true });
          const filePath = join(benchDir, `${save_as}.json`);
          await writeFile(
            filePath,
            JSON.stringify(currentResult, null, 2),
            'utf-8',
          );
          sections.push(`Baseline saved: ${filePath}`);
          sections.push('');
        } catch (err) {
          const msg =
            err instanceof Error ? err.message : String(err);
          sections.push(`Warning: failed to save baseline: ${msg}`);
          sections.push('');
        }
      }

      // Compare with baseline if requested
      if (compare_with) {
        const baselinePath = join(benchDir, `${compare_with}.json`);
        try {
          const baselineJson = await readFile(baselinePath, 'utf-8');
          const baselineResult: BenchmarkResult =
            JSON.parse(baselineJson);
          const comparison = formatComparison(
            baselineResult,
            currentResult,
          );
          sections.push(comparison);
        } catch {
          sections.push(
            `Cannot load baseline "${compare_with}" from ${baselinePath}`,
          );
        }
        sections.push('');
      }

      // Show current results
      if (!compare_with) {
        sections.push(`Benchmark: ${resultLabel}`);
        sections.push(`Timestamp: ${now}`);
        sections.push('');

        if (entries.length > 0) {
          sections.push(`Results (${entries.length} entries):`);
          for (const e of entries) {
            sections.push(`  ${e.name}: ${e.value} ${e.unit}`);
          }
        } else {
          sections.push(
            'No benchmark entries parsed from output.',
          );
          sections.push('Ensure output contains timing/metric values.');
        }
        sections.push('');
        sections.push('--- Raw output ---');
        sections.push(output.trim());
      }

      return {
        content: [
          { type: 'text' as const, text: sections.join('\n') },
        ],
      };
    },
  );
}
