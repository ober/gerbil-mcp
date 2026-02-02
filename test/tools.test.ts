/**
 * Functional tests for all Gerbil MCP tools.
 *
 * These tests verify that each tool works correctly by invoking it through
 * the MCP JSON-RPC protocol and checking the response.
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { spawn, ChildProcess } from 'node:child_process';
import { mkdirSync, writeFileSync, rmSync } from 'node:fs';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

interface McpResponse {
  jsonrpc: string;
  id: number;
  result?: {
    content?: Array<{ type: string; text: string }>;
    protocolVersion?: string;
  };
  error?: {
    code: number;
    message: string;
  };
}

class McpClient {
  private proc: ChildProcess | null = null;
  private buffer = '';
  private responseQueue: Array<{
    resolve: (value: McpResponse) => void;
    reject: (error: Error) => void;
  }> = [];
  private initialized = false;

  async start(): Promise<void> {
    this.proc = spawn('node', ['dist/index.js'], {
      stdio: ['pipe', 'pipe', 'pipe'],
      cwd: process.cwd(),
    });

    this.proc.stdout!.on('data', (chunk: Buffer) => {
      this.buffer += chunk.toString();
      this.processBuffer();
    });

    this.proc.stderr!.on('data', (chunk: Buffer) => {
      // Ignore stderr (startup message)
    });

    // Initialize the MCP connection
    const initResponse = await this.send({
      jsonrpc: '2.0',
      id: 0,
      method: 'initialize',
      params: {
        protocolVersion: '2024-11-05',
        capabilities: {},
        clientInfo: { name: 'test', version: '1.0' },
      },
    });

    if (!initResponse.result?.protocolVersion) {
      throw new Error('Failed to initialize MCP connection');
    }
    this.initialized = true;
  }

  private processBuffer(): void {
    const lines = this.buffer.split('\n');
    this.buffer = lines.pop() || '';

    for (const line of lines) {
      if (!line.trim()) continue;
      try {
        const response = JSON.parse(line) as McpResponse;
        const handler = this.responseQueue.shift();
        if (handler) {
          handler.resolve(response);
        }
      } catch {
        // Ignore non-JSON lines
      }
    }
  }

  async send(request: object): Promise<McpResponse> {
    if (!this.proc?.stdin?.writable) {
      throw new Error('MCP process not running');
    }

    return new Promise((resolve, reject) => {
      this.responseQueue.push({ resolve, reject });
      this.proc!.stdin!.write(JSON.stringify(request) + '\n');

      // Timeout after 30 seconds
      setTimeout(() => {
        const idx = this.responseQueue.findIndex((h) => h.resolve === resolve);
        if (idx !== -1) {
          this.responseQueue.splice(idx, 1);
          reject(new Error('Request timed out'));
        }
      }, 30000);
    });
  }

  async callTool(
    name: string,
    args: Record<string, unknown> = {},
  ): Promise<{ text: string; isError: boolean }> {
    const response = await this.send({
      jsonrpc: '2.0',
      id: Date.now(),
      method: 'tools/call',
      params: { name, arguments: args },
    });

    if (response.error) {
      return { text: response.error.message, isError: true };
    }

    const content = response.result?.content?.[0];
    if (!content || content.type !== 'text') {
      return { text: 'No text content in response', isError: true };
    }

    return {
      text: content.text,
      isError: (response.result as { isError?: boolean })?.isError === true,
    };
  }

  stop(): void {
    if (this.proc) {
      this.proc.kill();
      this.proc = null;
    }
    this.initialized = false;
  }
}

// Shared test fixtures
const TEST_DIR = join(tmpdir(), 'gerbil-mcp-test-' + Date.now());

describe('Gerbil MCP Tools', () => {
  let client: McpClient;

  beforeAll(async () => {
    // Create test directory and files
    mkdirSync(TEST_DIR, { recursive: true });

    writeFileSync(
      join(TEST_DIR, 'sample.ss'),
      `(import :std/text/json)
(export main helper)

(def (main args)
  (displayln "hello"))

(def (helper x)
  (+ x 1))

(defstruct point (x y))
`,
    );

    writeFileSync(
      join(TEST_DIR, 'with-error.ss'),
      `(import :std/text/json)
(define (f x)
  (+ x undefined-var))
`,
    );

    writeFileSync(
      join(TEST_DIR, 'lint-issues.ss'),
      `(import :std/text/json :std/text/json)
(define (f x) x)
`,
    );

    writeFileSync(
      join(TEST_DIR, 'uses-json.ss'),
      `(import :std/text/json)
(def (parse-data s)
  (read-json s))
`,
    );

    // Balance checking fixtures
    writeFileSync(
      join(TEST_DIR, 'balanced.ss'),
      `(import :std/text/json)
(def (f x) (+ x 1))
(def (g y) [y (+ y 2)])
`,
    );

    writeFileSync(
      join(TEST_DIR, 'unclosed.ss'),
      `(def (f x)
  (+ x 1)
`,
    );

    writeFileSync(
      join(TEST_DIR, 'extra-close.ss'),
      `(def (f x) (+ x 1)))
`,
    );

    writeFileSync(
      join(TEST_DIR, 'mismatch.ss'),
      `(def (f x) [+ x 1)
`,
    );

    writeFileSync(
      join(TEST_DIR, 'reader-error.ss'),
      `(def (f x) (+ x 1))
(def (g y "unterminated
`,
    );

    // Start MCP client
    client = new McpClient();
    await client.start();
  }, 60000);

  afterAll(() => {
    client.stop();
    rmSync(TEST_DIR, { recursive: true, force: true });
  });

  describe('Core evaluation tools', () => {
    it('gerbil_version returns version info', async () => {
      const result = await client.callTool('gerbil_version');
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Gerbil version');
      expect(result.text).toContain('Gambit version');
    });

    it('gerbil_eval evaluates simple expressions', async () => {
      const result = await client.callTool('gerbil_eval', {
        expression: '(+ 1 2 3)',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('6');
    });

    it('gerbil_eval handles imports', async () => {
      const result = await client.callTool('gerbil_eval', {
        expression: '(json-object->string (hash ("a" 1)))',
        imports: [':std/text/json'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('a');
    });

    it('gerbil_check_syntax validates correct code', async () => {
      const result = await client.callTool('gerbil_check_syntax', {
        code: '(define (f x) (+ x 1))',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('valid');
    });

    it('gerbil_check_syntax detects syntax errors', async () => {
      const result = await client.callTool('gerbil_check_syntax', {
        code: '(if)',
      });
      expect(result.isError).toBe(true);
      expect(result.text.toLowerCase()).toContain('syntax');
    });

    it('gerbil_compile_check validates compilable code', async () => {
      const result = await client.callTool('gerbil_compile_check', {
        code: '(import :std/text/json) (define (f x) (read-json x))',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('passed');
    });

    it('gerbil_format pretty-prints code', async () => {
      const result = await client.callTool('gerbil_format', {
        code: '(define (f x y) (cond ((> x y) (+ x 1)) ((< x y) (- y 1)) (else 0)))',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('define');
    });

    it('gerbil_benchmark measures execution time', async () => {
      const result = await client.callTool('gerbil_benchmark', {
        expression:
          '(let loop ((i 0) (s 0)) (if (< i 1000) (loop (+ i 1) (+ s i)) s))',
        iterations: 2,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Iterations');
      expect(result.text).toContain('499500');
    });
  });

  describe('Module inspection tools', () => {
    it('gerbil_module_exports lists exports', async () => {
      const result = await client.callTool('gerbil_module_exports', {
        module_path: ':std/text/json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('read-json');
      expect(result.text).toContain('write-json');
    });

    it('gerbil_list_std_modules lists modules by prefix', async () => {
      const result = await client.callTool('gerbil_list_std_modules', {
        prefix: 'std/text',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain(':std/text/json');
      expect(result.text).toContain(':std/text/csv');
    });

    it('gerbil_module_deps shows dependencies', async () => {
      const result = await client.callTool('gerbil_module_deps', {
        module_path: ':std/text/json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('dependencies');
    });

    it('gerbil_function_signature returns arity info', async () => {
      const result = await client.callTool('gerbil_function_signature', {
        module_path: ':std/text/json',
        symbol: 'read-json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('procedure');
      expect(result.text).toContain('arity');
    });

    it('gerbil_ffi_inspect shows FFI bindings', async () => {
      const result = await client.callTool('gerbil_ffi_inspect', {
        module_path: ':std/os/signal',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('SIGTERM');
      expect(result.text).toContain('SIGKILL');
    });
  });

  describe('Symbol lookup tools', () => {
    it('gerbil_apropos finds matching symbols', async () => {
      const result = await client.callTool('gerbil_apropos', {
        pattern: 'hash-ref',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('hash-ref');
    });

    it('gerbil_doc returns symbol documentation', async () => {
      const result = await client.callTool('gerbil_doc', {
        symbol: 'read-json',
        module_path: ':std/text/json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Kind: procedure');
      expect(result.text).toContain('Arity');
    });

    it('gerbil_find_definition locates symbol definition', async () => {
      const result = await client.callTool('gerbil_find_definition', {
        symbol: 'read-json',
        module_path: ':std/text/json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Kind: procedure');
      expect(result.text).toContain('Module file');
    });

    it('gerbil_suggest_imports finds module for symbol', async () => {
      const result = await client.callTool('gerbil_suggest_imports', {
        symbol: 'read-json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain(':std/text/json');
    });
  });

  describe('Macro tools', () => {
    it('gerbil_expand_macro expands macros', async () => {
      const result = await client.callTool('gerbil_expand_macro', {
        expression: '(when #t (displayln 42))',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('%#if');
    });

    it('gerbil_trace_macro shows expansion steps', async () => {
      const result = await client.callTool('gerbil_trace_macro', {
        expression: '(when #t (displayln 42))',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('[step');
      expect(result.text).toContain('if');
    });
  });

  describe('Type inspection tools', () => {
    it('gerbil_class_info shows class metadata', async () => {
      const result = await client.callTool('gerbil_class_info', {
        type_name: 'Error',
        module_path: ':std/error',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Slots');
      expect(result.text).toContain('Precedence');
    });

    it('gerbil_error_hierarchy shows error types', async () => {
      const result = await client.callTool('gerbil_error_hierarchy');
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Error');
      expect(result.text).toContain('IOError');
    });
  });

  describe('File analysis tools', () => {
    it('gerbil_load_file extracts definitions', async () => {
      const result = await client.callTool('gerbil_load_file', {
        file_path: join(TEST_DIR, 'sample.ss'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('main');
      expect(result.text).toContain('helper');
      expect(result.text).toContain('point');
    });

    it('gerbil_document_symbols lists symbols with lines', async () => {
      const result = await client.callTool('gerbil_document_symbols', {
        file_path: join(TEST_DIR, 'sample.ss'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('main');
      expect(result.text).toContain('L4'); // Line number
    });

    it('gerbil_diagnostics checks files for errors', async () => {
      const result = await client.callTool('gerbil_diagnostics', {
        file_path: join(TEST_DIR, 'sample.ss'),
      });
      expect(result.isError).toBe(false);
      // Valid file should compile cleanly
      expect(result.text).toContain('cleanly');
    });

    it('gerbil_lint detects code issues', async () => {
      const result = await client.callTool('gerbil_lint', {
        file_path: join(TEST_DIR, 'lint-issues.ss'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('warning');
    });
  });

  describe('Project tools', () => {
    it('gerbil_workspace_symbols finds symbols by query', async () => {
      const result = await client.callTool('gerbil_workspace_symbols', {
        query: 'main',
        directory: TEST_DIR,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('main');
    });

    it('gerbil_find_callers finds symbol references', async () => {
      const result = await client.callTool('gerbil_find_callers', {
        symbol: 'read-json',
        directory: TEST_DIR,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('uses-json.ss');
    });

    it('gerbil_rename_symbol previews renames (dry run)', async () => {
      const result = await client.callTool('gerbil_rename_symbol', {
        old_name: 'main',
        new_name: 'start',
        directory: TEST_DIR,
        dry_run: true,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Dry run');
      expect(result.text).toContain('main');
    });

    it('gerbil_project_info shows project metadata', async () => {
      // Create a minimal gerbil.pkg
      writeFileSync(join(TEST_DIR, 'gerbil.pkg'), '(package: test-pkg)');

      const result = await client.callTool('gerbil_project_info', {
        project_path: TEST_DIR,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Source Files');
    });

    it('gerbil_package_info lists installed packages', async () => {
      const result = await client.callTool('gerbil_package_info', {
        action: 'list',
      });
      expect(result.isError).toBe(false);
      // May have no packages installed, which is fine
      expect(result.text).toBeDefined();
    });
  });

  describe('Balance checking tools', () => {
    it('gerbil_check_balance reports balanced file', async () => {
      const result = await client.callTool('gerbil_check_balance', {
        file_path: join(TEST_DIR, 'balanced.ss'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Balance OK');
      expect(result.text).toContain('top-level form');
    });

    it('gerbil_check_balance detects unclosed delimiter', async () => {
      const result = await client.callTool('gerbil_check_balance', {
        file_path: join(TEST_DIR, 'unclosed.ss'),
      });
      expect(result.isError).toBe(true);
      expect(result.text.toLowerCase()).toContain('unclosed');
    });

    it('gerbil_check_balance detects extra closer', async () => {
      const result = await client.callTool('gerbil_check_balance', {
        file_path: join(TEST_DIR, 'extra-close.ss'),
      });
      expect(result.isError).toBe(true);
      expect(result.text.toLowerCase()).toContain('unexpected');
    });

    it('gerbil_check_balance detects mismatched delimiters', async () => {
      const result = await client.callTool('gerbil_check_balance', {
        file_path: join(TEST_DIR, 'mismatch.ss'),
      });
      expect(result.isError).toBe(true);
      expect(result.text.toLowerCase()).toContain('mismatch');
    });

    it('gerbil_check_balance ignores parens in strings', async () => {
      const result = await client.callTool('gerbil_check_balance', {
        code: '(def x "hello (world) [foo]")',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Balance OK');
    });

    it('gerbil_check_balance ignores parens in comments', async () => {
      const result = await client.callTool('gerbil_check_balance', {
        code: '(def x 1) ; unclosed ( in comment\n#| nested ([ |#\n(def y 2)',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Balance OK');
    });

    it('gerbil_check_balance works with inline code', async () => {
      const result = await client.callTool('gerbil_check_balance', {
        code: '(def (f x) (+ x 1)',
      });
      expect(result.isError).toBe(true);
      expect(result.text.toLowerCase()).toContain('unclosed');
    });
  });

  describe('Read forms tool', () => {
    it('gerbil_read_forms lists forms with line numbers', async () => {
      const result = await client.callTool('gerbil_read_forms', {
        file_path: join(TEST_DIR, 'sample.ss'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Forms');
      expect(result.text).toContain('import');
      expect(result.text).toContain('def');
    });

    it('gerbil_read_forms reports reader errors', async () => {
      const result = await client.callTool('gerbil_read_forms', {
        file_path: join(TEST_DIR, 'reader-error.ss'),
      });
      // Should report the error
      expect(result.text.toLowerCase()).toContain('error');
    });
  });

  describe('REPL session tools', () => {
    it('gerbil_repl_session creates and uses sessions', async () => {
      // Create session
      const createResult = await client.callTool('gerbil_repl_session', {
        action: 'create',
      });
      expect(createResult.isError).toBe(false);
      expect(createResult.text).toContain('Session created');

      // Extract session ID
      const match = createResult.text.match(/Session created: (\w+)/);
      expect(match).toBeTruthy();
      const sessionId = match![1];

      // Eval in session
      const evalResult = await client.callTool('gerbil_repl_session', {
        action: 'eval',
        session_id: sessionId,
        expression: '(define x 42)',
      });
      expect(evalResult.isError).toBe(false);

      // Use defined variable
      const useResult = await client.callTool('gerbil_repl_session', {
        action: 'eval',
        session_id: sessionId,
        expression: '(+ x 10)',
      });
      expect(useResult.isError).toBe(false);
      expect(useResult.text).toContain('52');

      // List sessions
      const listResult = await client.callTool('gerbil_repl_session', {
        action: 'list',
      });
      expect(listResult.isError).toBe(false);
      expect(listResult.text).toContain(sessionId);

      // Destroy session
      const destroyResult = await client.callTool('gerbil_repl_session', {
        action: 'destroy',
        session_id: sessionId,
      });
      expect(destroyResult.isError).toBe(false);
      expect(destroyResult.text).toContain('destroyed');
    });
  });
});
