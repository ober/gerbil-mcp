/**
 * Shared verification utilities for cookbook recipe validation.
 *
 * These functions accept explicit gxi/gxc paths so they can be used
 * both by the MCP howto-verify tool (which uses the resolved global paths)
 * and by the standalone cross-version test script (which uses user-supplied paths).
 */

import { execFile } from 'node:child_process';
import { writeFile, unlink, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { escapeSchemeString } from '../gxi.js';
import type { Recipe } from './howto.js';

export const PASS_MARKER = 'GERBIL-MCP-VERIFY-PASS:';
export const FAIL_MARKER = 'GERBIL-MCP-VERIFY-FAIL:';
export const DEFAULT_BATCH_SIZE = 5;

export interface VerifyResult {
  id: string;
  passed: boolean;
  error?: string;
}

export interface CompileResult {
  id: string;
  passed: boolean;
  error?: string;
}

interface ExecResult {
  stdout: string;
  stderr: string;
  exitCode: number;
  timedOut: boolean;
}

/**
 * Run an executable and return structured result (no global caching).
 */
function execBinary(
  binPath: string,
  args: string[],
  options?: { timeout?: number; env?: Record<string, string> },
): Promise<ExecResult> {
  const timeout = options?.timeout ?? 30_000;
  return new Promise((resolve) => {
    execFile(
      binPath,
      args,
      {
        timeout,
        maxBuffer: 1024 * 1024,
        env: { ...process.env, ...options?.env },
      },
      (error, stdout, stderr) => {
        if (error) {
          const timedOut = error.killed === true;
          const code = (error as NodeJS.ErrnoException).code;
          const exitCode =
            typeof error.code === 'number'
              ? error.code
              : code === 'ENOENT'
                ? 127
                : 1;
          resolve({
            stdout: stdout ?? '',
            stderr: stderr ?? '',
            exitCode,
            timedOut,
          });
        } else {
          resolve({
            stdout: stdout ?? '',
            stderr: stderr ?? '',
            exitCode: 0,
            timedOut: false,
          });
        }
      },
    );
  });
}

/**
 * Build the Scheme expressions needed to syntax-verify a batch of recipes.
 * Returns { importExpr, checksExpr } suitable as separate -e arguments to gxi.
 */
export function buildVerifyExpressions(recipes: Recipe[]): {
  importExpr: string;
  checksExpr: string;
} {
  const allImports = new Set<string>();
  for (const recipe of recipes) {
    for (const imp of recipe.imports) {
      allImports.add(imp);
    }
  }
  const topLevelImports = [...allImports].map((imp) => `(import ${imp})`).join(' ');

  const checks = recipes.map((recipe) => {
    const escapedId = escapeSchemeString(recipe.id);
    const escapedCode = escapeSchemeString(recipe.code);

    return [
      '(with-catch',
      '  (lambda (e)',
      `    (display "${FAIL_MARKER}${escapedId}\\t")`,
      '    (display-exception e (current-output-port)))',
      '  (lambda ()',
      `    (let ((p (open-input-string "${escapedCode}")))`,
      '      (let loop ((form (read p)))',
      '        (unless (eof-object? form)',
      '          (unless (and (pair? form) (memq (car form) (quote (import export))))',
      '            (core-expand form))',
      '          (loop (read p)))))',
      `    (display "${PASS_MARKER}${escapedId}\\n")))`,
    ].join(' ');
  });

  const importExpr = `(import :gerbil/expander) ${topLevelImports}`;
  const checksExpr = checks.join(' ');
  return { importExpr, checksExpr };
}

/**
 * Parse PASS/FAIL markers from gxi stdout.
 */
export function parseVerifyOutput(
  output: string,
): Map<string, { passed: boolean; error?: string }> {
  const resultMap = new Map<string, { passed: boolean; error?: string }>();

  for (const line of output.split('\n')) {
    if (line.startsWith(PASS_MARKER)) {
      const id = line.slice(PASS_MARKER.length).trim();
      resultMap.set(id, { passed: true });
    } else if (line.startsWith(FAIL_MARKER)) {
      const rest = line.slice(FAIL_MARKER.length);
      const tabIdx = rest.indexOf('\t');
      if (tabIdx !== -1) {
        const id = rest.slice(0, tabIdx);
        const error = rest.slice(tabIdx + 1).trim();
        resultMap.set(id, { passed: false, error });
      } else {
        const id = rest.trim();
        resultMap.set(id, { passed: false, error: 'verification failed' });
      }
    }
  }

  return resultMap;
}

/**
 * Build the content for a temp .ss file to compile-check a recipe.
 */
export function buildCompileCheckFile(recipe: Recipe): string {
  const importLines = recipe.imports
    .map((imp) => `(import ${imp})`)
    .join('\n');
  return `${importLines}\n${recipe.code}\n`;
}

/**
 * Extract a concise error message from gxc output.
 */
export function extractCompileError(output: string): string {
  for (const line of output.split('\n')) {
    if (/unbound identifier/i.test(line)) {
      return line.trim();
    }
    if (/Syntax Error/i.test(line)) {
      return line.trim();
    }
    if (/error/i.test(line) && line.trim().length > 5) {
      return line.trim();
    }
  }
  const firstLine = output.split('\n').find((l) => l.trim().length > 0);
  return firstLine?.trim() || output.slice(0, 200);
}

/**
 * Run syntax verification on a batch of recipes using a specified gxi binary.
 */
export async function runVerifyBatch(
  recipes: Recipe[],
  gxiPath: string,
  options?: { timeout?: number; env?: Record<string, string> },
): Promise<VerifyResult[]> {
  const { importExpr, checksExpr } = buildVerifyExpressions(recipes);
  const timeout = options?.timeout ?? 60_000;

  const result = await execBinary(gxiPath, ['-e', importExpr, '-e', checksExpr], {
    timeout,
    env: options?.env,
  });

  const resultMap = parseVerifyOutput(result.stdout);

  return recipes.map((recipe) => {
    const r = resultMap.get(recipe.id);
    if (r) {
      return { id: recipe.id, ...r };
    }
    if (result.timedOut) {
      return { id: recipe.id, passed: false, error: 'batch timed out' };
    }
    if (result.exitCode !== 0) {
      return {
        id: recipe.id,
        passed: false,
        error: `batch failed (exit ${result.exitCode})`,
      };
    }
    return { id: recipe.id, passed: false, error: 'no result (batch crash?)' };
  });
}

/**
 * Compile-check a single recipe using a specified gxc binary.
 */
export async function runCompileCheck(
  recipe: Recipe,
  gxcPath: string,
  tempDir: string,
  options?: { timeout?: number; env?: Record<string, string> },
): Promise<CompileResult> {
  const content = buildCompileCheckFile(recipe);
  const filePath = join(tempDir, `verify-${recipe.id}.ss`);
  const timeout = options?.timeout ?? 30_000;

  try {
    await writeFile(filePath, content, 'utf-8');
    const result = await execBinary(gxcPath, ['-S', filePath], {
      timeout,
      env: options?.env,
    });

    if (result.exitCode === 0) {
      return { id: recipe.id, passed: true };
    } else {
      const combined = [result.stdout, result.stderr]
        .filter(Boolean)
        .join('\n')
        .trim();
      const errorLine = extractCompileError(combined);
      return {
        id: recipe.id,
        passed: false,
        error: errorLine || 'compilation failed',
      };
    }
  } catch (e) {
    return {
      id: recipe.id,
      passed: false,
      error: `exception: ${e instanceof Error ? e.message : String(e)}`,
    };
  } finally {
    try {
      await unlink(filePath);
    } catch {
      // ignore cleanup errors
    }
  }
}

/**
 * Compile-check multiple recipes sequentially.
 */
export async function runCompileCheckBatch(
  recipes: Recipe[],
  gxcPath: string,
  options?: { timeout?: number; env?: Record<string, string> },
): Promise<CompileResult[]> {
  const tempDir = join(tmpdir(), 'gerbil-mcp-verify');
  await mkdir(tempDir, { recursive: true });

  const results: CompileResult[] = [];
  for (const recipe of recipes) {
    const result = await runCompileCheck(recipe, gxcPath, tempDir, options);
    results.push(result);
  }
  return results;
}
