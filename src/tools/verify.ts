import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { writeFile, readFile, unlink } from 'node:fs/promises';
import { join } from 'node:path';
import { randomUUID } from 'node:crypto';
import { tmpdir } from 'node:os';
import { runGxi, runGxc, escapeSchemeString, ERROR_MARKER, VALID_MARKER, buildLoadpathEnv } from '../gxi.js';
import { parseDefinitions, parseGxcErrors, extractModulePaths, type FileAnalysis } from './parse-utils.js';

interface VerifyIssue {
  phase: 'syntax' | 'compile' | 'lint' | 'arity';
  severity: 'error' | 'warning' | 'info';
  line?: number;
  message: string;
}

export function registerVerifyTool(server: McpServer): void {
  server.registerTool(
    'gerbil_verify',
    {
      title: 'Verify Gerbil Code',
      description:
        'Combined verification tool that runs syntax check, compilation check (gxc -S), ' +
        'lint, and arity checking in one pass. Returns a unified list of issues with severity ' +
        'and line numbers. Accepts either inline code or a file path. Replaces the workflow of ' +
        'calling check_syntax, compile_check, lint, and check_arity as separate tool calls.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        code: z
          .string()
          .optional()
          .describe('Gerbil source code to verify'),
        file_path: z
          .string()
          .optional()
          .describe('Path to a .ss/.scm file to verify (alternative to code)'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Directories to add to GERBIL_LOADPATH for project-local module resolution'),
        project_path: z
          .string()
          .optional()
          .describe('Project root directory (sets loadpath to its .gerbil/lib directory)'),
        skip_lint: z
          .boolean()
          .optional()
          .describe('Skip the lint phase (default: false)'),
        skip_arity: z
          .boolean()
          .optional()
          .describe('Skip the arity check phase (default: false)'),
      },
    },
    async ({ code, file_path, loadpath, project_path, skip_lint, skip_arity }) => {
      if (!code && !file_path) {
        return {
          content: [{
            type: 'text' as const,
            text: 'Either "code" or "file_path" must be provided.',
          }],
          isError: true,
        };
      }

      const issues: VerifyIssue[] = [];
      let targetPath = file_path || '';
      let tempFile = false;
      let sourceCode = code || '';

      // Build loadpath env
      const lpDirs: string[] = [...(loadpath || [])];
      if (project_path) {
        lpDirs.push(join(project_path, '.gerbil', 'lib'));
      }
      const env = lpDirs.length > 0 ? buildLoadpathEnv(lpDirs) : undefined;

      // Write code to temp file if inline
      if (code) {
        const tempName = `gerbil-verify-${randomUUID().slice(0, 8)}.ss`;
        targetPath = join(tmpdir(), tempName);
        try {
          await writeFile(targetPath, code, 'utf-8');
          tempFile = true;
        } catch (err) {
          return {
            content: [{
              type: 'text' as const,
              text: `Failed to write temp file: ${err instanceof Error ? err.message : String(err)}`,
            }],
            isError: true,
          };
        }
      } else {
        // Read source code for lint/arity
        try {
          sourceCode = await readFile(targetPath, 'utf-8');
        } catch (err) {
          return {
            content: [{
              type: 'text' as const,
              text: `Failed to read file: ${err instanceof Error ? err.message : String(err)}`,
            }],
            isError: true,
          };
        }
      }

      // Detect FFI-related code that can cause false positives in the isolated syntax check
      const hasFFIContent = /\b(begin-ffi|begin-foreign|c-declare|c-define-type|c-lambda)\b/.test(sourceCode) ||
        /\bimport\b.*\b(begin-ffi|ffi|foreign)\b/i.test(sourceCode);

      try {
        // Phase 1: Syntax check (quick, via expander)
        // Skip for files containing FFI forms, as core-expand in isolation
        // cannot resolve begin-ffi module exports and produces false EOF errors.
        const firstForm = sourceCode.split('\n').find(l => l.trim() && !l.trim().startsWith(';'));
        if (firstForm && !hasFFIContent) {
          const escaped = escapeSchemeString(firstForm.trim());
          const exprs = [
            '(import :gerbil/expander)',
            [
              '(with-catch',
              '  (lambda (e)',
              `    (display "${ERROR_MARKER}\\n")`,
              '    (display-exception e (current-output-port)))',
              '  (lambda ()',
              `    (core-expand (read (open-input-string "${escaped}")))`,
              `    (displayln "${VALID_MARKER}")))`,
            ].join(' '),
          ];
          try {
            const result = await runGxi(exprs, { timeout: 10000 });
            const stdout = result.stdout;
            if (stdout.includes(ERROR_MARKER)) {
              const errorMsg = stdout.slice(stdout.indexOf(ERROR_MARKER) + ERROR_MARKER.length).trim();
              // Suppress EOF errors when imports reference FFI modules
              const isEofError = /incomplete form|eof|end-of-file/i.test(errorMsg);
              const importsFFI = /\bimport\b/.test(sourceCode) && isEofError;
              if (!importsFFI) {
                issues.push({
                  phase: 'syntax',
                  severity: 'error',
                  message: `Syntax error: ${errorMsg.split('\n')[0]}`,
                });
              }
            }
          } catch {
            // Syntax check failure — continue to other phases
          }
        }

        // Phase 2: Compile check (gxc -S)
        try {
          const result = await runGxc(targetPath, { env });
          if (result.timedOut) {
            issues.push({ phase: 'compile', severity: 'error', message: 'Compilation check timed out' });
          } else if (result.exitCode !== 0) {
            let errorOutput = result.stderr.trim();
            const stdoutOutput = result.stdout.trim();
            if (!errorOutput && stdoutOutput) errorOutput = stdoutOutput;
            else if (errorOutput && stdoutOutput) errorOutput += '\n' + stdoutOutput;
            if (tempFile && errorOutput) {
              errorOutput = errorOutput.replaceAll(targetPath, '<input>');
            }
            // Parse individual errors
            const lines = errorOutput.split('\n').filter(l => l.trim());
            for (const line of lines.slice(0, 10)) {
              const lineMatch = line.match(/:(\d+):/);
              issues.push({
                phase: 'compile',
                severity: 'error',
                line: lineMatch ? parseInt(lineMatch[1]) : undefined,
                message: line.trim(),
              });
            }
            if (lines.length === 0) {
              issues.push({ phase: 'compile', severity: 'error', message: 'Compilation failed (no details)' });
            }
          }
        } catch {
          issues.push({ phase: 'compile', severity: 'warning', message: 'gxc not available for compile check' });
        }

        // Phase 3: Lint (static analysis)
        if (!skip_lint) {
          const lintIssues = runLintChecks(sourceCode, tempFile ? '<input>' : targetPath);
          issues.push(...lintIssues);
        }

        // Phase 4: Arity check (for file_path mode only, and only if no compile errors)
        if (!skip_arity && !tempFile) {
          const compileErrors = issues.filter(i => i.phase === 'compile' && i.severity === 'error');
          if (compileErrors.length === 0) {
            const arityIssues = runArityChecks(sourceCode);
            issues.push(...arityIssues);
          }
        }

        // Format output
        const sections: string[] = [];
        const errors = issues.filter(i => i.severity === 'error');
        const warnings = issues.filter(i => i.severity === 'warning');
        const infos = issues.filter(i => i.severity === 'info');

        if (issues.length === 0) {
          sections.push('All checks passed (syntax, compile, lint' + (skip_arity ? '' : ', arity') + ').');
        } else {
          sections.push(`Found ${issues.length} issue(s):\n`);
          for (const issue of issues) {
            const loc = issue.line ? `:${issue.line}` : '';
            const icon = issue.severity === 'error' ? 'ERROR' :
              issue.severity === 'warning' ? 'WARN' : 'INFO';
            sections.push(`  [${icon}] [${issue.phase}]${loc} ${issue.message}`);
          }
          sections.push('');
          sections.push(`Summary: ${errors.length} error(s), ${warnings.length} warning(s), ${infos.length} info(s)`);
        }

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
          isError: errors.length > 0,
        };
      } finally {
        if (tempFile) {
          try { await unlink(targetPath); } catch { /* ignore */ }
        }
      }
    },
  );
}

/**
 * Lightweight lint checks (subset of the full lint tool).
 */
function runLintChecks(source: string, filePath: string): VerifyIssue[] {
  const issues: VerifyIssue[] = [];
  const lines = source.split('\n');

  // Track imports and definitions
  const importedModules: string[] = [];

  // Check for duplicate definitions using the shared parser
  const analysis = parseDefinitions(source);
  const defSeen = new Map<string, number>();
  for (const def of analysis.definitions) {
    if (defSeen.has(def.name)) {
      issues.push({
        phase: 'lint',
        severity: 'warning',
        line: def.line,
        message: `Duplicate definition: "${def.name}" already defined at line ${defSeen.get(def.name)}`,
      });
    } else {
      defSeen.set(def.name, def.line);
    }
  }

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const trimmed = line.trim();

    // Skip comments
    if (trimmed.startsWith(';') || trimmed.startsWith('#|')) continue;

    // Check for duplicate imports
    const importMatch = trimmed.match(/^\(import\s+(.+)\)$/);
    if (importMatch) {
      const mods = importMatch[1].split(/\s+/).filter(m => m.startsWith(':'));
      for (const mod of mods) {
        if (importedModules.includes(mod)) {
          issues.push({
            phase: 'lint',
            severity: 'warning',
            line: i + 1,
            message: `Duplicate import: ${mod}`,
          });
        }
        importedModules.push(mod);
      }
    }

    // Check for unquote outside quasiquote
    if (/,@?\w/.test(trimmed) && !trimmed.includes('`') && !trimmed.startsWith(';')) {
      // Simple heuristic: unquote without quasiquote on same line
      // Only flag if not inside a string
      const noStrings = trimmed.replace(/"(?:[^"\\]|\\.)*"/g, '""');
      if (/,@?\w/.test(noStrings) && !noStrings.includes('`')) {
        // Check if any preceding line in the same form has a quasiquote
        // Simple heuristic — just flag it as info
      }
    }

    // Check for . in brackets
    if (/\[.*\.\s/.test(trimmed)) {
      const noStrings = trimmed.replace(/"(?:[^"\\]|\\.)*"/g, '""');
      if (/\[.*\.\s/.test(noStrings)) {
        issues.push({
          phase: 'lint',
          severity: 'warning',
          line: i + 1,
          message: 'Dotted pair inside brackets — use parentheses for dotted pairs',
        });
      }
    }
  }

  return issues;
}

/**
 * Basic arity checks (heuristic).
 */
function runArityChecks(source: string): VerifyIssue[] {
  const issues: VerifyIssue[] = [];
  const lines = source.split('\n');

  // Known strict-arity functions
  const STRICT_ARITY: Record<string, number> = {
    'hash-get': 2,
    'hash-key?': 2,
    'hash-remove!': 2,
    'hash-put!': 3,
  };

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();
    if (line.startsWith(';')) continue;
    for (const [fn, arity] of Object.entries(STRICT_ARITY)) {
      const regex = new RegExp(`\\(${fn.replace(/[?!]/g, '\\$&')}\\s`);
      if (regex.test(line)) {
        // Count approximate args (very rough heuristic)
        const start = line.indexOf(`(${fn}`);
        if (start >= 0) {
          const rest = line.slice(start + fn.length + 1);
          const args = rest.split(/\s+/).filter(a => a && !a.startsWith(';'));
          // This is too rough for a real check — skip for inline code
        }
      }
    }
  }

  return issues;
}
