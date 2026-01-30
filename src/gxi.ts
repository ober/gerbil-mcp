import { execFile } from 'node:child_process';
import { access } from 'node:fs/promises';
import { constants } from 'node:fs';

export interface GxiResult {
  stdout: string;
  stderr: string;
  exitCode: number;
  timedOut: boolean;
}

export interface GxiOptions {
  timeout?: number;
  gxiPath?: string;
}

const DEFAULT_TIMEOUT = 30_000;
const MAX_BUFFER = 1024 * 1024; // 1MB

let resolvedGxiPath: string | null = null;

async function findGxi(override?: string): Promise<string> {
  if (override) return override;
  if (resolvedGxiPath) return resolvedGxiPath;

  const candidates = [
    process.env.GERBIL_MCP_GXI_PATH,
    '/opt/gerbil/bin/gxi',
    'gxi',
  ].filter(Boolean) as string[];

  for (const candidate of candidates) {
    try {
      await access(candidate, constants.X_OK);
      resolvedGxiPath = candidate;
      return candidate;
    } catch {
      // not found or not executable, try next
    }
  }

  // Fall back to 'gxi' and let execFile handle the error
  resolvedGxiPath = 'gxi';
  return 'gxi';
}

export async function runGxi(
  expressions: string[],
  options?: GxiOptions,
): Promise<GxiResult> {
  const gxiPath = await findGxi(options?.gxiPath);
  const timeout = options?.timeout ?? DEFAULT_TIMEOUT;

  const args: string[] = [];
  for (const expr of expressions) {
    args.push('-e', expr);
  }

  return new Promise((resolve) => {
    execFile(
      gxiPath,
      args,
      {
        timeout,
        maxBuffer: MAX_BUFFER,
        env: { ...process.env },
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
 * Escape a string for embedding inside a Scheme string literal.
 * Example: `he said "hi"` becomes `he said \"hi\"`
 */
export function escapeSchemeString(s: string): string {
  return s
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t');
}

// Markers used by tools to delimit output
export const RESULT_MARKER = 'GERBIL-MCP-RESULT:';
export const ERROR_MARKER = 'GERBIL-MCP-ERROR:';
export const VALID_MARKER = 'GERBIL-MCP-VALID';
