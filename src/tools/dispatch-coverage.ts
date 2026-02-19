import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { scanSchemeFiles } from './parse-utils.js';

interface CommandCall {
  command: string;
  line: number;
  testName?: string;
}

interface SequenceGap {
  commands: string[];
  individualCoverage: { command: string; testCount: number }[];
  suggestion: string;
}

/**
 * Extract execute-command! calls from a test file.
 */
function extractCommandCalls(content: string, filePath: string): CommandCall[] {
  const calls: CommandCall[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Match (execute-command! 'command-name) or (execute-command! "command-name")
    // Note: 'symbol is a quoted symbol, so we need to match the quote and capture what follows
    const match = line.match(/\(execute-command!\s+['"]([a-zA-Z0-9_!?<>=+\-*/:~#]+)/);
    if (match) {
      calls.push({
        command: match[1],
        line: i + 1,
      });
    }
  }

  return calls;
}

/**
 * Group command calls into sequences (consecutive calls within a test).
 */
function extractSequences(calls: CommandCall[]): string[][] {
  const sequences: string[][] = [];
  let currentSequence: string[] = [];

  for (let i = 0; i < calls.length; i++) {
    currentSequence.push(calls[i].command);

    // If the next call is more than 10 lines away, consider it a new sequence
    if (i + 1 >= calls.length || calls[i + 1].line - calls[i].line > 10) {
      if (currentSequence.length > 0) {
        sequences.push([...currentSequence]);
      }
      currentSequence = [];
    }
  }

  return sequences;
}

/**
 * Analyze command coverage and find gaps.
 */
function analyzeCommandCoverage(
  sequences: string[][],
  allCommands: Set<string>,
): SequenceGap[] {
  const gaps: SequenceGap[] = [];

  // Count individual command occurrences
  const commandCounts = new Map<string, number>();
  for (const seq of sequences) {
    for (const cmd of seq) {
      commandCounts.set(cmd, (commandCounts.get(cmd) || 0) + 1);
    }
  }

  // Count pair occurrences (command A followed by command B)
  const pairCounts = new Map<string, number>();
  for (const seq of sequences) {
    for (let i = 0; i < seq.length - 1; i++) {
      const pair = `${seq[i]} → ${seq[i + 1]}`;
      pairCounts.set(pair, (pairCounts.get(pair) || 0) + 1);
    }
  }

  // Find commands that are tested individually but never in combination
  const commands = [...allCommands].sort();
  for (let i = 0; i < commands.length; i++) {
    for (let j = i + 1; j < commands.length; j++) {
      const cmdA = commands[i];
      const cmdB = commands[j];

      const countA = commandCounts.get(cmdA) || 0;
      const countB = commandCounts.get(cmdB) || 0;

      // Both commands are tested individually
      if (countA > 0 && countB > 0) {
        const forwardPair = `${cmdA} → ${cmdB}`;
        const reversePair = `${cmdB} → ${cmdA}`;

        // But never tested in sequence
        if (!pairCounts.has(forwardPair) && !pairCounts.has(reversePair)) {
          gaps.push({
            commands: [cmdA, cmdB],
            individualCoverage: [
              { command: cmdA, testCount: countA },
              { command: cmdB, testCount: countB },
            ],
            suggestion: `Consider testing ${cmdA} followed by ${cmdB}, or vice versa`,
          });
        }
      }
    }
  }

  return gaps;
}

export function registerDispatchCoverageTool(server: McpServer): void {
  server.registerTool(
    'gerbil_dispatch_coverage_analysis',
    {
      title: 'Command Dispatch Sequence Coverage Analysis',
      description:
        'Analyze functional test files for command interaction coverage gaps. ' +
        'Identifies commands tested in isolation but never in combination with other commands. ' +
        'Helps detect missing test scenarios for multi-command sequences that could reveal ' +
        'state management bugs. Scans for execute-command! calls and suggests missing interaction patterns.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        test_directory: z
          .string()
          .describe('Directory containing functional test files (*.ss)'),
        min_individual_coverage: z
          .number()
          .optional()
          .describe(
            'Minimum number of times a command must appear individually to suggest sequences (default: 2)',
          ),
      },
    },
    async ({ test_directory, min_individual_coverage }) => {
      const minCoverage = min_individual_coverage ?? 2;

      // Scan for test files
      const testFiles = await scanSchemeFiles(test_directory);
      if (testFiles.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'No .ss files found in test directory.',
            },
          ],
        };
      }

      // Extract command calls from all test files
      const allCalls: CommandCall[] = [];
      const allCommands = new Set<string>();

      for (const file of testFiles) {
        try {
          const content = await readFile(file, 'utf-8');
          const calls = extractCommandCalls(content, file);
          allCalls.push(...calls);
          calls.forEach((c) => allCommands.add(c.command));
        } catch (err) {
          // Skip files that can't be read
          continue;
        }
      }

      if (allCalls.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'No execute-command! calls found in test files.',
            },
          ],
        };
      }

      // Extract sequences
      const sequences = extractSequences(allCalls);

      // Analyze coverage
      const gaps = analyzeCommandCoverage(sequences, allCommands);

      // Filter gaps by minimum coverage threshold
      const filteredGaps = gaps.filter((gap) =>
        gap.individualCoverage.every((c) => c.testCount >= minCoverage),
      );

      // Format output
      const sections: string[] = [
        `Command Dispatch Sequence Coverage Analysis`,
        ``,
        `Test files analyzed: ${testFiles.length}`,
        `Total commands found: ${allCommands.size}`,
        `Total command calls: ${allCalls.length}`,
        `Sequences detected: ${sequences.length}`,
        ``,
      ];

      if (filteredGaps.length === 0) {
        sections.push(
          `✓ No significant coverage gaps detected for commonly-used commands.`,
        );
        sections.push(
          `  All commands with ${minCoverage}+ individual tests have some sequence coverage.`,
        );
      } else {
        sections.push(
          `⚠ Found ${filteredGaps.length} command pair(s) tested individually but never sequenced:`,
        );
        sections.push(``);

        for (const gap of filteredGaps.slice(0, 20)) {
          // Limit to top 20
          const coverageStr = gap.individualCoverage
            .map((c) => `${c.command} (${c.testCount} tests)`)
            .join(', ');
          sections.push(`  ${gap.commands.join(' + ')}:`);
          sections.push(`    Individual coverage: ${coverageStr}`);
          sections.push(`    Suggestion: ${gap.suggestion}`);
          sections.push(``);
        }

        if (filteredGaps.length > 20) {
          sections.push(
            `  ... and ${filteredGaps.length - 20} more pairs. Increase min_individual_coverage to focus on most-tested commands.`,
          );
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
