import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, readdir, stat } from 'node:fs/promises';
import { join } from 'node:path';

interface SigchldFinding {
  file: string;
  line: number;
  symbol: string;
  context: string;
}

interface SigchldReport {
  signalHandlers: SigchldFinding[];
  processStatusCalls: SigchldFinding[];
  openProcessCalls: SigchldFinding[];
  hasConflict: boolean;
}

/**
 * Scan a file for process-status and signal handler usage.
 */
function scanFile(
  content: string,
  filePath: string,
): {
  signalHandlers: SigchldFinding[];
  processStatusCalls: SigchldFinding[];
  openProcessCalls: SigchldFinding[];
} {
  const signalHandlers: SigchldFinding[] = [];
  const processStatusCalls: SigchldFinding[] = [];
  const openProcessCalls: SigchldFinding[] = [];

  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const trimmed = line.trim();
    const lineNum = i + 1;

    // Skip comments
    if (trimmed.startsWith(';')) continue;

    // Detect add-signal-handler! / ##add-signal-handler!
    if (/\b(?:##)?add-signal-handler!/.test(trimmed)) {
      signalHandlers.push({
        file: filePath,
        line: lineNum,
        symbol: 'add-signal-handler!',
        context: trimmed,
      });
    }

    // Detect signal-handler-set! (alternative API)
    if (/\bsignal-handler-set!/.test(trimmed)) {
      signalHandlers.push({
        file: filePath,
        line: lineNum,
        symbol: 'signal-handler-set!',
        context: trimmed,
      });
    }

    // Detect process-status usage
    if (/\b(?:##)?process-status\b/.test(trimmed)) {
      processStatusCalls.push({
        file: filePath,
        line: lineNum,
        symbol: 'process-status',
        context: trimmed,
      });
    }

    // Detect open-process (which implicitly uses process-status when waiting)
    if (
      /\b(?:##)?open-process\b/.test(trimmed) ||
      /\b(?:##)?process-pid\b/.test(trimmed)
    ) {
      openProcessCalls.push({
        file: filePath,
        line: lineNum,
        symbol: trimmed.includes('open-process')
          ? 'open-process'
          : 'process-pid',
        context: trimmed,
      });
    }
  }

  return { signalHandlers, processStatusCalls, openProcessCalls };
}

/**
 * Scan all .ss files in a directory recursively.
 */
async function scanDirectory(dirPath: string): Promise<SigchldReport> {
  const report: SigchldReport = {
    signalHandlers: [],
    processStatusCalls: [],
    openProcessCalls: [],
    hasConflict: false,
  };

  const entries = await readdir(dirPath, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = join(dirPath, entry.name);
    if (entry.isDirectory() && !entry.name.startsWith('.')) {
      const sub = await scanDirectory(fullPath);
      report.signalHandlers.push(...sub.signalHandlers);
      report.processStatusCalls.push(...sub.processStatusCalls);
      report.openProcessCalls.push(...sub.openProcessCalls);
    } else if (entry.isFile() && entry.name.endsWith('.ss')) {
      const content = await readFile(fullPath, 'utf-8');
      const found = scanFile(content, fullPath);
      report.signalHandlers.push(...found.signalHandlers);
      report.processStatusCalls.push(...found.processStatusCalls);
      report.openProcessCalls.push(...found.openProcessCalls);
    }
  }

  report.hasConflict =
    report.signalHandlers.length > 0 &&
    (report.processStatusCalls.length > 0 ||
      report.openProcessCalls.length > 0);

  return report;
}

export function registerSigchldCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_sigchld_check',
    {
      title: 'Detect SIGCHLD/process-status Incompatibility',
      description:
        'Detect uses of Gambit process-status in projects that also use ' +
        'add-signal-handler! (which blocks SIGCHLD via signalfd on Linux). ' +
        'When both are present, process-status will hang because Gambit\'s ' +
        'internal SIGCHLD handler never fires. Scans for: (1) add-signal-handler! ' +
        'and signal-handler-set! calls, (2) process-status calls, ' +
        '(3) open-process calls that implicitly rely on process-status. ' +
        'Reports the conflict with explanation and suggests ffi-waitpid as replacement.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Single .ss file to scan'),
        project_path: z
          .string()
          .optional()
          .describe(
            'Project directory to scan recursively (all .ss files)',
          ),
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

      let report: SigchldReport;

      if (file_path) {
        let content: string;
        try {
          content = await readFile(file_path, 'utf-8');
        } catch {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Cannot read file: ${file_path}`,
              },
            ],
            isError: true,
          };
        }
        const found = scanFile(content, file_path);
        report = {
          ...found,
          hasConflict:
            found.signalHandlers.length > 0 &&
            (found.processStatusCalls.length > 0 ||
              found.openProcessCalls.length > 0),
        };
      } else {
        try {
          await stat(project_path!);
        } catch {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Directory not found: ${project_path}`,
              },
            ],
            isError: true,
          };
        }
        report = await scanDirectory(project_path!);
      }

      const sections: string[] = [];

      if (report.hasConflict) {
        sections.push(
          'WARNING: SIGCHLD/process-status conflict detected!',
        );
        sections.push('');
        sections.push(
          'On Linux, add-signal-handler! uses signalfd which blocks SIGCHLD ' +
          'via sigprocmask. This prevents Gambit\'s internal SIGCHLD handler ' +
          'from firing, causing process-status to hang indefinitely.',
        );
        sections.push('');

        sections.push(
          `Signal handlers (${report.signalHandlers.length}):`,
        );
        for (const f of report.signalHandlers) {
          sections.push(`  ${f.file}:${f.line}  ${f.context}`);
        }
        sections.push('');

        if (report.processStatusCalls.length > 0) {
          sections.push(
            `process-status calls (${report.processStatusCalls.length}):`,
          );
          for (const f of report.processStatusCalls) {
            sections.push(`  ${f.file}:${f.line}  ${f.context}`);
          }
          sections.push('');
        }

        if (report.openProcessCalls.length > 0) {
          sections.push(
            `open-process calls (${report.openProcessCalls.length}):`,
          );
          for (const f of report.openProcessCalls) {
            sections.push(`  ${f.file}:${f.line}  ${f.context}`);
          }
          sections.push('');
        }

        sections.push('Suggested fix:');
        sections.push(
          '  Replace process-status with an FFI-based waitpid polling approach.',
        );
        sections.push(
          '  Use (c-lambda (int int) int "waitpid") to check child exit status',
        );
        sections.push(
          '  without relying on SIGCHLD delivery.',
        );
        sections.push('');
        sections.push(
          'See cookbook recipe "waitpid-ffi" for a working implementation.',
        );
      } else if (
        report.signalHandlers.length === 0 &&
        report.processStatusCalls.length === 0 &&
        report.openProcessCalls.length === 0
      ) {
        sections.push(
          'No signal handlers or process-status calls found. No SIGCHLD conflict.',
        );
      } else {
        sections.push('No SIGCHLD conflict detected.');
        sections.push('');

        if (report.signalHandlers.length > 0) {
          sections.push(
            `Signal handlers found (${report.signalHandlers.length}) but no process-status usage:`,
          );
          for (const f of report.signalHandlers) {
            sections.push(`  ${f.file}:${f.line}  ${f.context}`);
          }
        }

        if (
          report.processStatusCalls.length > 0 ||
          report.openProcessCalls.length > 0
        ) {
          const total =
            report.processStatusCalls.length +
            report.openProcessCalls.length;
          sections.push(
            `Process calls found (${total}) but no signal handler usage:`,
          );
          for (const f of [
            ...report.processStatusCalls,
            ...report.openProcessCalls,
          ]) {
            sections.push(`  ${f.file}:${f.line}  ${f.context}`);
          }
        }
      }

      return {
        content: [
          { type: 'text' as const, text: sections.join('\n') },
        ],
        isError: report.hasConflict,
      };
    },
  );
}
