import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, writeFile, unlink } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { randomUUID } from 'node:crypto';
import { runGxi, runGxiFile, escapeSchemeString, buildLoadpathEnv } from '../gxi.js';

const FORM_MARKER = 'GERBIL-MCP-BISECT-FORM:';

// Forms that are always kept (preamble)
const PREAMBLE_HEADS = new Set([
  'import', 'export', 'require', 'cond-expand', 'declare',
  'begin-syntax', 'begin-foreign', 'extern',
]);

interface FormInfo {
  index: number;
  startLine: number; // 1-based, where reader started scanning (may include whitespace before form)
  endLine: number;   // 1-based, line after form ends (reader position after read)
  summary: string;
  // Computed: actual source line range (1-based, inclusive)
  fromLine: number;
  toLine: number;
}

interface CrashSignature {
  exitCode: number;
  stderrSnippet: string;
}

interface TrialResult {
  crashed: boolean;
  exitCode: number;
  stderr: string;
  timedOut: boolean;
}

function buildReadFormsExpr(escaped: string): string {
  return [
    '(let* ((src "' + escaped + '")',
    '       (port (open-input-string src))',
    '       (idx 0))',
    '  (let loop ()',
    '    (let ((start-line (input-port-line port)))',
    '      (with-catch',
    '        (lambda (e)',
    '          (display "' + FORM_MARKER + 'ERROR\\t")',
    '          (display-exception e (current-output-port))',
    '          (newline))',
    '        (lambda ()',
    '          (let ((form (read port)))',
    '            (if (eof-object? form)',
    '              (void)',
    '              (let ((end-line (input-port-line port))',
    '                    (summary (cond',
    '                               ((pair? form)',
    '                                (let ((hd (car form)))',
    '                                  (if (symbol? hd)',
    '                                    (symbol->string hd)',
    '                                    "(...)")))',
    '                               ((symbol? form) (symbol->string form))',
    '                               ((string? form) "\\"...\\"" )',
    '                               ((number? form) (number->string form))',
    '                               ((boolean? form) (if form "#t" "#f"))',
    '                               (else (object->string form)))))',
    '                (display "' + FORM_MARKER + '")',
    '                (display idx) (display "\\t")',
    '                (display start-line) (display "\\t")',
    '                (display end-line) (display "\\t")',
    '                (displayln summary)',
    '                (set! idx (+ idx 1))',
    '                (loop)))))))))',
  ].join(' ');
}

function parseFormLines(output: string, totalLines: number): FormInfo[] {
  const raw: Array<{ index: number; startLine: number; endLine: number; summary: string }> = [];
  for (const line of output.split('\n')) {
    if (line.startsWith(FORM_MARKER)) {
      const rest = line.slice(FORM_MARKER.length);
      if (rest.startsWith('ERROR')) continue;
      const parts = rest.split('\t');
      if (parts.length >= 4) {
        raw.push({
          index: parseInt(parts[0], 10),
          startLine: parseInt(parts[1], 10),
          endLine: parseInt(parts[2], 10),
          summary: parts[3],
        });
      }
    }
  }

  // Compute actual line ranges for each form.
  // The reader's endLine is the line where the reader sits after consuming the form.
  // For form i: content spans from (prevEndLine + 1) to (thisEndLine), 1-based inclusive.
  // For the first form: content spans from line 1 to endLine.
  const forms: FormInfo[] = [];
  for (let i = 0; i < raw.length; i++) {
    const r = raw[i];
    const fromLine = i > 0 ? raw[i - 1].endLine + 1 : 1;
    const toLine = r.endLine;
    forms.push({
      ...r,
      fromLine: Math.min(fromLine, toLine),
      toLine,
    });
  }

  return forms;
}

function isPreamble(form: FormInfo): boolean {
  return PREAMBLE_HEADS.has(form.summary);
}

function extractFormText(sourceLines: string[], form: FormInfo): string {
  // fromLine and toLine are 1-based inclusive
  return sourceLines.slice(form.fromLine - 1, form.toLine).join('\n');
}

function buildTrialSource(sourceLines: string[], preamble: FormInfo[], body: FormInfo[]): string {
  const parts: string[] = [];
  for (const form of preamble) {
    parts.push(extractFormText(sourceLines, form));
  }
  for (const form of body) {
    parts.push(extractFormText(sourceLines, form));
  }
  return parts.join('\n') + '\n';
}

function crashMatches(trial: TrialResult, original: CrashSignature): boolean {
  // Same crash = same non-zero exit code
  return trial.crashed && trial.exitCode === original.exitCode;
}

function stderrSnippet(stderr: string): string {
  const lines = stderr.trim().split('\n');
  if (lines.length <= 3) return stderr.trim();
  return lines.slice(0, 3).join('\n') + '\n...';
}

function formatLineRange(forms: FormInfo[]): string {
  if (forms.length === 0) return '';
  const first = forms[0];
  const last = forms[forms.length - 1];
  if (first.fromLine === last.toLine) {
    return `L${first.fromLine}`;
  }
  return `L${first.fromLine}-${last.toLine}`;
}

export function registerBisectCrashTool(server: McpServer): void {
  server.registerTool(
    'gerbil_bisect_crash',
    {
      title: 'Bisect Crash',
      description:
        'Binary-search a crashing Gerbil file to find the minimal set of top-level forms ' +
        'that reproduce the crash. Keeps preamble forms (import, export, require, declare, ' +
        'begin-syntax, begin-foreign, extern) and bisects body forms. Useful for isolating ' +
        'segfaults, uncaught exceptions, and other crashes — especially in FFI code.',
      inputSchema: {
        file_path: z
          .string()
          .describe('Path to the crashing Gerbil file'),
        timeout: z
          .number()
          .optional()
          .describe('Per-trial timeout in ms (default 10000)'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Directories to add to GERBIL_LOADPATH'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib'),
      },
    },
    async ({ file_path, timeout, loadpath, project_path }) => {
      const trialTimeout = timeout ?? 10_000;
      const MAX_ITERATIONS = 20;

      // Build loadpath env
      const effectiveLoadpath = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.unshift(join(project_path, '.gerbil', 'lib'));
      }
      const env = buildLoadpathEnv(effectiveLoadpath);

      // Read the source file
      let source: string;
      try {
        source = await readFile(file_path, 'utf-8');
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [{ type: 'text' as const, text: `Failed to read file: ${msg}` }],
          isError: true,
        };
      }

      const sourceLines = source.split('\n');

      // Parse forms using the Gerbil reader
      const escaped = escapeSchemeString(source);
      const expr = buildReadFormsExpr(escaped);
      const readResult = await runGxi([expr]);

      if (readResult.timedOut) {
        return {
          content: [{ type: 'text' as const, text: 'Reader timed out while parsing forms.' }],
          isError: true,
        };
      }

      const forms = parseFormLines(readResult.stdout, sourceLines.length);

      if (forms.length === 0) {
        return {
          content: [{ type: 'text' as const, text: 'No top-level forms found in file.' }],
          isError: true,
        };
      }

      // Partition into preamble and body
      const preamble = forms.filter(isPreamble);
      const body = forms.filter(f => !isPreamble(f));

      if (body.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: 'File contains only preamble forms (import/export/etc.) — nothing to bisect.',
          }],
        };
      }

      // Temp file management
      const tempFiles: string[] = [];
      async function writeTempFile(content: string): Promise<string> {
        const name = `gerbil-bisect-${randomUUID().slice(0, 8)}.ss`;
        const path = join(tmpdir(), name);
        await writeFile(path, content, 'utf-8');
        tempFiles.push(path);
        return path;
      }

      async function cleanupTempFiles(): Promise<void> {
        for (const f of tempFiles) {
          try { await unlink(f); } catch { /* ignore */ }
        }
      }

      async function runTrial(bodyForms: FormInfo[]): Promise<TrialResult> {
        const src = buildTrialSource(sourceLines, preamble, bodyForms);
        const path = await writeTempFile(src);
        const result = await runGxiFile(path, { timeout: trialTimeout, env });
        return {
          crashed: result.exitCode !== 0 && !result.timedOut,
          exitCode: result.exitCode,
          stderr: result.stderr,
          timedOut: result.timedOut,
        };
      }

      try {
        // Step 1: Confirm the crash with the original file
        const originalResult = await runGxiFile(file_path, { timeout: trialTimeout, env });

        if (originalResult.exitCode === 0) {
          return {
            content: [{
              type: 'text' as const,
              text: `File: ${file_path}\n\nNo crash detected — file exited successfully (exit code 0).`,
            }],
          };
        }

        if (originalResult.timedOut) {
          return {
            content: [{
              type: 'text' as const,
              text: `File: ${file_path}\n\nFile timed out after ${trialTimeout}ms — cannot bisect timeouts.`,
            }],
          };
        }

        const crashSig: CrashSignature = {
          exitCode: originalResult.exitCode,
          stderrSnippet: stderrSnippet(originalResult.stderr),
        };

        const log: string[] = [];
        let iteration = 0;

        // Step 2: Binary search through body forms
        let currentBody = [...body];

        while (currentBody.length > 1 && iteration < MAX_ITERATIONS) {
          const mid = Math.ceil(currentBody.length / 2);
          const firstHalf = currentBody.slice(0, mid);
          const secondHalf = currentBody.slice(mid);

          // Try first half
          iteration++;
          const firstDesc = `forms ${firstHalf[0].index}-${firstHalf[firstHalf.length - 1].index}`;
          log.push(`  [${iteration}] Trying ${firstDesc} (${firstHalf.length} of ${currentBody.length})...`);

          const firstResult = await runTrial(firstHalf);
          if (crashMatches(firstResult, crashSig)) {
            log[log.length - 1] += ' CRASH';
            currentBody = firstHalf;
            continue;
          }
          log[log.length - 1] += ' ok';

          // Try second half
          iteration++;
          const secondDesc = `forms ${secondHalf[0].index}-${secondHalf[secondHalf.length - 1].index}`;
          log.push(`  [${iteration}] Trying ${secondDesc} (${secondHalf.length} of ${currentBody.length})...`);

          const secondResult = await runTrial(secondHalf);
          if (crashMatches(secondResult, crashSig)) {
            log[log.length - 1] += ' CRASH';
            currentBody = secondHalf;
            continue;
          }
          log[log.length - 1] += ' ok';

          // Neither half alone crashes — forms interact
          log.push('  [!] Neither half crashes alone — forms interact.');
          break;
        }

        // Build output
        const sections: string[] = [];
        sections.push(`File: ${file_path}`);
        sections.push(`Original crash: exit code ${crashSig.exitCode}`);
        if (crashSig.stderrSnippet) {
          sections.push(`Stderr: ${crashSig.stderrSnippet}`);
        }
        sections.push('');

        if (log.length > 0) {
          sections.push('Bisection log:');
          sections.push(...log);
          sections.push('');
        }

        const lineRange = formatLineRange(currentBody);
        sections.push(
          `Minimal crashing code (${currentBody.length} form${currentBody.length !== 1 ? 's' : ''}, ${lineRange}):`,
        );
        for (const form of currentBody) {
          const text = extractFormText(sourceLines, form);
          for (const line of text.split('\n')) {
            sections.push(`  ${line}`);
          }
        }

        if (preamble.length > 0) {
          sections.push('');
          sections.push('Preamble (always included):');
          for (const form of preamble) {
            const fRange =
              form.fromLine === form.toLine
                ? `L${form.fromLine}`
                : `L${form.fromLine}-${form.toLine}`;
            const text = extractFormText(sourceLines, form).trim();
            sections.push(`  ${fRange}: ${text}`);
          }
        }

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      } finally {
        await cleanupTempFiles();
      }
    },
  );
}
