import { execFile, spawn, type ChildProcess } from 'node:child_process';
import { access } from 'node:fs/promises';
import { constants } from 'node:fs';
import { randomUUID } from 'node:crypto';

export interface GxiResult {
  stdout: string;
  stderr: string;
  exitCode: number;
  timedOut: boolean;
}

export interface GxiOptions {
  timeout?: number;
  gxiPath?: string;
  env?: Record<string, string>;
}

const DEFAULT_TIMEOUT = 30_000;
const MAX_BUFFER = 1024 * 1024; // 1MB

// ── gxi binary resolution ──────────────────────────────────────────

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

// ── gxc binary resolution & runner ─────────────────────────────────

let resolvedGxcPath: string | null = null;

async function findGxc(): Promise<string> {
  if (resolvedGxcPath) return resolvedGxcPath;

  const candidates = [
    process.env.GERBIL_MCP_GXC_PATH,
    '/opt/gerbil/bin/gxc',
    'gxc',
  ].filter(Boolean) as string[];

  for (const candidate of candidates) {
    try {
      await access(candidate, constants.X_OK);
      resolvedGxcPath = candidate;
      return candidate;
    } catch {
      // not found or not executable, try next
    }
  }

  resolvedGxcPath = 'gxc';
  return 'gxc';
}

export async function runGxc(
  filePath: string,
  options?: GxiOptions,
): Promise<GxiResult> {
  const gxcPath = await findGxc();
  const timeout = options?.timeout ?? DEFAULT_TIMEOUT;

  return new Promise((resolve) => {
    execFile(
      gxcPath,
      ['-S', filePath],
      {
        timeout,
        maxBuffer: MAX_BUFFER,
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

// ── gxpkg binary resolution & runner ────────────────────────────────

let resolvedGxpkgPath: string | null = null;

async function findGxpkg(): Promise<string> {
  if (resolvedGxpkgPath) return resolvedGxpkgPath;

  const candidates = [
    process.env.GERBIL_MCP_GXPKG_PATH,
    '/opt/gerbil/bin/gxpkg',
    'gxpkg',
  ].filter(Boolean) as string[];

  for (const candidate of candidates) {
    try {
      await access(candidate, constants.X_OK);
      resolvedGxpkgPath = candidate;
      return candidate;
    } catch {
      // not found or not executable, try next
    }
  }

  resolvedGxpkgPath = 'gxpkg';
  return 'gxpkg';
}

export async function runGxpkg(
  args: string[],
  options?: { timeout?: number; cwd?: string },
): Promise<GxiResult> {
  const gxpkgPath = await findGxpkg();
  const timeout = options?.timeout ?? DEFAULT_TIMEOUT;

  return new Promise((resolve) => {
    execFile(
      gxpkgPath,
      args,
      {
        timeout,
        maxBuffer: MAX_BUFFER,
        env: { ...process.env },
        cwd: options?.cwd,
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

// ── gerbil binary resolution & runner ────────────────────────────

let resolvedGerbilPath: string | null = null;

async function findGerbil(): Promise<string> {
  if (resolvedGerbilPath) return resolvedGerbilPath;

  const candidates = [
    process.env.GERBIL_MCP_GERBIL_PATH,
    '/opt/gerbil/bin/gerbil',
    'gerbil',
  ].filter(Boolean) as string[];

  for (const candidate of candidates) {
    try {
      await access(candidate, constants.X_OK);
      resolvedGerbilPath = candidate;
      return candidate;
    } catch {
      // not found or not executable, try next
    }
  }

  resolvedGerbilPath = 'gerbil';
  return 'gerbil';
}

export async function runGerbilCmd(
  args: string[],
  options?: { timeout?: number; cwd?: string; env?: Record<string, string> },
): Promise<GxiResult> {
  const gerbilPath = await findGerbil();
  const timeout = options?.timeout ?? DEFAULT_TIMEOUT;

  return new Promise((resolve) => {
    execFile(
      gerbilPath,
      args,
      {
        timeout,
        maxBuffer: MAX_BUFFER,
        env: { ...process.env, ...options?.env },
        cwd: options?.cwd,
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

// ── String escaping ────────────────────────────────────────────────

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

/**
 * Build environment overlay for GERBIL_LOADPATH.
 * Merges with any existing GERBIL_LOADPATH from process.env.
 */
export function buildLoadpathEnv(
  loadpath: string[],
): Record<string, string> {
  if (loadpath.length === 0) return {};
  const existing = process.env.GERBIL_LOADPATH ?? '';
  const parts = [...loadpath, ...(existing ? [existing] : [])];
  return { GERBIL_LOADPATH: parts.join(':') };
}

// ── Run gxi with a file argument ──────────────────────────────────

export async function runGxiFile(
  filePath: string,
  options?: GxiOptions,
): Promise<GxiResult> {
  const gxiPath = await findGxi(options?.gxiPath);
  const timeout = options?.timeout ?? DEFAULT_TIMEOUT;

  return new Promise((resolve) => {
    execFile(
      gxiPath,
      [filePath],
      {
        timeout,
        maxBuffer: MAX_BUFFER,
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

// ── Markers used by tools to delimit output ────────────────────────

export const RESULT_MARKER = 'GERBIL-MCP-RESULT:';
export const ERROR_MARKER = 'GERBIL-MCP-ERROR:';
export const VALID_MARKER = 'GERBIL-MCP-VALID';

// ── REPL Session Management ────────────────────────────────────────

const REPL_SENTINEL = 'GERBIL-MCP-REPL-DONE';
const MAX_SESSIONS = 5;
const SESSION_IDLE_TIMEOUT = 10 * 60 * 1000; // 10 minutes
const EVAL_TIMEOUT = 30_000; // 30 seconds per expression

export interface ReplSessionInfo {
  id: string;
  createdAt: number;
  lastUsedAt: number;
}

interface ReplSession {
  id: string;
  process: ChildProcess;
  createdAt: number;
  lastUsedAt: number;
  stdoutBuffer: string;
  stderrBuffer: string;
}

const sessions = new Map<string, ReplSession>();

function cleanupIdleSessions(): void {
  const now = Date.now();
  for (const [id, session] of sessions) {
    if (now - session.lastUsedAt > SESSION_IDLE_TIMEOUT) {
      session.process.kill();
      sessions.delete(id);
    }
  }
}

export async function createReplSession(options?: {
  env?: Record<string, string>;
}): Promise<{
  id: string;
  error?: string;
}> {
  cleanupIdleSessions();

  if (sessions.size >= MAX_SESSIONS) {
    return {
      id: '',
      error: `Maximum ${MAX_SESSIONS} concurrent sessions reached. Destroy an existing session first.`,
    };
  }

  const gxiPath = await findGxi();
  const id = randomUUID().slice(0, 8);

  const proc = spawn(gxiPath, [], {
    stdio: ['pipe', 'pipe', 'pipe'],
    env: { ...process.env, ...options?.env },
  });

  const session: ReplSession = {
    id,
    process: proc,
    createdAt: Date.now(),
    lastUsedAt: Date.now(),
    stdoutBuffer: '',
    stderrBuffer: '',
  };

  proc.stdout!.on('data', (chunk: Buffer) => {
    session.stdoutBuffer += chunk.toString();
  });

  proc.stderr!.on('data', (chunk: Buffer) => {
    session.stderrBuffer += chunk.toString();
  });

  proc.on('exit', () => {
    sessions.delete(id);
  });

  sessions.set(id, session);

  // Send sentinel to confirm process is ready
  proc.stdin!.write(`(display "${REPL_SENTINEL}\\n")\n`);

  const ready = await waitForSentinel(session, 5000);
  if (!ready.ok) {
    session.process.kill();
    sessions.delete(id);
    return { id: '', error: 'Failed to start gxi session.' };
  }

  return { id };
}

export async function evalInSession(
  sessionId: string,
  expression: string,
): Promise<{ output: string; error?: string }> {
  cleanupIdleSessions();

  const session = sessions.get(sessionId);
  if (!session) {
    return { output: '', error: `Session "${sessionId}" not found.` };
  }

  if (!session.process.stdin!.writable) {
    sessions.delete(sessionId);
    return { output: '', error: `Session "${sessionId}" process has exited.` };
  }

  session.lastUsedAt = Date.now();
  session.stdoutBuffer = '';
  session.stderrBuffer = '';

  // Send expression followed by sentinel
  session.process.stdin!.write(expression + '\n');
  session.process.stdin!.write(`(display "${REPL_SENTINEL}\\n")\n`);

  const result = await waitForSentinel(session, EVAL_TIMEOUT);

  if (!result.ok) {
    return {
      output: '',
      error: 'Expression evaluation timed out after 30 seconds.',
    };
  }

  // Clean up output: remove prompts and sentinel
  let output = result.text;
  // Strip gxi prompts ("> " at line starts)
  output = output.replace(/^> /gm, '');
  output = output.trim();

  const stderrOutput = session.stderrBuffer.trim();
  if (stderrOutput) {
    return {
      output: output || '(void)',
      error: stderrOutput,
    };
  }

  return { output: output || '(void)' };
}

export function destroyReplSession(sessionId: string): boolean {
  const session = sessions.get(sessionId);
  if (!session) return false;

  session.process.kill();
  sessions.delete(sessionId);
  return true;
}

export function listReplSessions(): ReplSessionInfo[] {
  cleanupIdleSessions();
  const result: ReplSessionInfo[] = [];
  for (const session of sessions.values()) {
    result.push({
      id: session.id,
      createdAt: session.createdAt,
      lastUsedAt: session.lastUsedAt,
    });
  }
  return result;
}

function waitForSentinel(
  session: ReplSession,
  timeout: number,
): Promise<{ ok: boolean; text: string }> {
  return new Promise((resolve) => {
    const startLen = session.stdoutBuffer.length;
    const timer = setTimeout(() => {
      resolve({ ok: false, text: '' });
    }, timeout);

    const check = (): void => {
      const idx = session.stdoutBuffer.indexOf(REPL_SENTINEL);
      if (idx !== -1) {
        clearTimeout(timer);
        const text = session.stdoutBuffer.slice(0, idx);
        // Remove sentinel and trailing newline from buffer
        session.stdoutBuffer = session.stdoutBuffer.slice(
          idx + REPL_SENTINEL.length + 1,
        );
        resolve({ ok: true, text });
        return;
      }

      // Check if process has exited
      if (session.process.exitCode !== null) {
        clearTimeout(timer);
        resolve({ ok: false, text: session.stdoutBuffer });
        return;
      }

      setTimeout(check, 50);
    };

    // Start checking after a small delay to let data arrive
    setTimeout(check, 10);
  });
}
