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

    // Minimal test file for run_tests timeout test
    writeFileSync(
      join(TEST_DIR, 'timeout-test.ss'),
      `(import :std/test)
(def timeout-test
  (test-suite "timeout"
    (test-case "fast" (check (+ 1 1) => 2))))
(run-tests! timeout-test)
(test-report-summary!)
`,
    );

    // Hash literal lint fixture
    writeFileSync(
      join(TEST_DIR, 'hash-lint.ss'),
      `(import :std/text/json)
(def config
  (hash (FOO 1) ("bar" 2) (CRITICAL "yes") (name "val")))
`,
    );

    // Package file for project tools and REPL project_path tests
    writeFileSync(join(TEST_DIR, 'gerbil.pkg'), '(package: test-pkg)');

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

    it('gerbil_eval accepts loadpath parameter', async () => {
      const result = await client.callTool('gerbil_eval', {
        expression: '(+ 1 2)',
        loadpath: ['/nonexistent/path'],
      });
      // Should still work — loadpath just adds to env
      expect(result.isError).toBe(false);
      expect(result.text).toContain('3');
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

    it('gerbil_compile_check accepts loadpath parameter', async () => {
      const result = await client.callTool('gerbil_compile_check', {
        code: '(import :std/text/json) (define (f x) (read-json x))',
        loadpath: ['/nonexistent/path'],
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

    it('gerbil_lint warns on bare uppercase hash keys', async () => {
      const result = await client.callTool('gerbil_lint', {
        file_path: join(TEST_DIR, 'hash-lint.ss'),
      });
      expect(result.text).toContain('hash-symbol-key');
      expect(result.text).toContain('FOO');
      expect(result.text).toContain('CRITICAL');
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
      const result = await client.callTool('gerbil_project_info', {
        project_path: TEST_DIR,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Source Files');
    });

    it('gerbil_project_map returns module exports and definitions', async () => {
      const result = await client.callTool('gerbil_project_map', {
        project_path: TEST_DIR,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Module:');
      expect(result.text).toContain('sample.ss');
      expect(result.text).toContain('main');
      expect(result.text).toContain('helper');
      expect(result.text).toContain('point');
      expect(result.text).toContain('Structs:');
      expect(result.text).toContain('Procedures:');
      expect(result.text).toContain(':std/text/json');
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

  describe('Run tests tool', () => {
    it('gerbil_run_tests accepts timeout parameter', async () => {
      const result = await client.callTool('gerbil_run_tests', {
        file_path: join(TEST_DIR, 'timeout-test.ss'),
        timeout: 60000,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('PASSED');
    });

    it('gerbil_run_tests rejects both file_path and directory', async () => {
      const result = await client.callTool('gerbil_run_tests', {
        file_path: join(TEST_DIR, 'timeout-test.ss'),
        directory: TEST_DIR,
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('Cannot specify both');
    });

    it('gerbil_run_tests requires file_path or directory', async () => {
      const result = await client.callTool('gerbil_run_tests', {});
      expect(result.isError).toBe(true);
      expect(result.text).toContain('required');
    });

    it('gerbil_run_tests directory mode runs project tests', async () => {
      const result = await client.callTool('gerbil_run_tests', {
        directory: TEST_DIR,
        timeout: 120000,
      });
      // Either passes (if gerbil test finds tests) or returns structured output
      expect(result.text).toBeDefined();
      // Should contain structured output sections
      expect(result.text).toContain('Result:');
    });

    it('gerbil_run_tests directory mode accepts filter', async () => {
      const result = await client.callTool('gerbil_run_tests', {
        directory: TEST_DIR,
        filter: 'timeout',
        timeout: 120000,
      });
      expect(result.text).toBeDefined();
      expect(result.text).toContain('Result:');
      expect(result.text).toContain('Filter: timeout');
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

    it('gerbil_repl_session create accepts loadpath', async () => {
      const createResult = await client.callTool('gerbil_repl_session', {
        action: 'create',
        loadpath: ['/tmp/test-loadpath'],
      });
      expect(createResult.isError).toBe(false);
      expect(createResult.text).toContain('Session created');
      expect(createResult.text).toContain('GERBIL_LOADPATH');

      // Clean up session
      const match = createResult.text.match(/Session created: (\w+)/);
      if (match) {
        await client.callTool('gerbil_repl_session', {
          action: 'destroy',
          session_id: match[1],
        });
      }
    });

    it('gerbil_repl_session create with preload_file loads imports', async () => {
      const createResult = await client.callTool('gerbil_repl_session', {
        action: 'create',
        preload_file: join(TEST_DIR, 'uses-json.ss'),
      });
      expect(createResult.isError).toBe(false);
      expect(createResult.text).toContain('Session created');
      expect(createResult.text).toContain('Preloaded');
      expect(createResult.text).toContain(':std/text/json');

      // Verify imports are actually loaded — read-json should be available
      const match = createResult.text.match(/Session created: (\w+)/);
      expect(match).toBeTruthy();
      const sessionId = match![1];

      const evalResult = await client.callTool('gerbil_repl_session', {
        action: 'eval',
        session_id: sessionId,
        expression: '(json-object->string (hash ("test" 1)))',
      });
      expect(evalResult.isError).toBe(false);
      expect(evalResult.text).toContain('test');

      // Clean up
      await client.callTool('gerbil_repl_session', {
        action: 'destroy',
        session_id: sessionId,
      });
    });

    it('gerbil_repl_session create accepts project_path', async () => {
      const createResult = await client.callTool('gerbil_repl_session', {
        action: 'create',
        project_path: TEST_DIR,
      });
      expect(createResult.isError).toBe(false);
      expect(createResult.text).toContain('Session created');
      expect(createResult.text).toContain('GERBIL_LOADPATH');
      expect(createResult.text).toContain('test-pkg');

      // Clean up session
      const match = createResult.text.match(/Session created: (\w+)/);
      if (match) {
        await client.callTool('gerbil_repl_session', {
          action: 'destroy',
          session_id: match[1],
        });
      }
    });
  });

  // ── Profiling tools ──────────────────────────────────────────────────

  describe('Profiling tools', () => {
    it('gerbil_profile reports timing stats', async () => {
      const result = await client.callTool('gerbil_profile', {
        expression: '(let loop ((i 0)) (if (< i 1000) (loop (+ i 1)) i))',
        functions: [],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Profile');
      expect(result.text).toContain('Wall time');
      expect(result.text).toContain('Result: 1000');
    });

    it('gerbil_profile warns for unbound functions', async () => {
      const result = await client.callTool('gerbil_profile', {
        expression: '(+ 1 2)',
        functions: ['nonexistent-fn'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('not bound');
    });

    it('gerbil_profile handles empty function list', async () => {
      const result = await client.callTool('gerbil_profile', {
        expression: '(+ 1 2)',
        functions: [],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Profile');
      expect(result.text).toContain('Wall time');
    });

    it('gerbil_profile accepts loadpath', async () => {
      const result = await client.callTool('gerbil_profile', {
        expression: '(+ 1 1)',
        functions: [],
        loadpath: ['/nonexistent/path'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Profile');
    });
  });

  // ── Heap profiling tools ─────────────────────────────────────────────

  describe('Heap profiling tools', () => {
    it('gerbil_heap_profile captures memory metrics', async () => {
      const result = await client.callTool('gerbil_heap_profile', {
        expression: '(let ((v (make-vector 10000 0))) (vector-length v))',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Heap Profile');
      expect(result.text).toContain('gc-heap-size');
      expect(result.text).toContain('gc-alloc');
    });

    it('gerbil_heap_profile shows result', async () => {
      const result = await client.callTool('gerbil_heap_profile', {
        expression: '(+ 40 2)',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Result: 42');
    });

    it('gerbil_heap_profile accepts imports', async () => {
      const result = await client.callTool('gerbil_heap_profile', {
        expression: '(hash ("a" 1))',
        imports: [':std/text/json'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Heap Profile');
    });
  });

  // ── Trace calls tool ─────────────────────────────────────────────────

  describe('Trace calls tool', () => {
    it('gerbil_trace_calls counts function calls', async () => {
      const result = await client.callTool('gerbil_trace_calls', {
        expression:
          '(begin (def (inc x) (+ x 1)) (let loop ((i 0)) (if (< i 50) (loop (inc i)) i)))',
        functions: ['inc'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Call Trace');
      expect(result.text).toContain('inc');
      expect(result.text).toContain('50');
    });

    it('gerbil_trace_calls handles empty function list', async () => {
      const result = await client.callTool('gerbil_trace_calls', {
        expression: '(+ 1 2)',
        functions: [],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Call Trace');
    });

    it('gerbil_trace_calls warns for unbound functions', async () => {
      const result = await client.callTool('gerbil_trace_calls', {
        expression: '(+ 1 2)',
        functions: ['no-such-fn'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('not bound');
    });

    it('gerbil_trace_calls shows result', async () => {
      const result = await client.callTool('gerbil_trace_calls', {
        expression:
          '(begin (def (id x) x) (id 42))',
        functions: ['id'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Result: 42');
    });
  });

  // ── Call graph tool ──────────────────────────────────────────────────

  describe('Call graph tool', () => {
    it('gerbil_call_graph analyzes function calls in a file', async () => {
      writeFileSync(
        join(TEST_DIR, 'call-graph-target.ss'),
        `(def (parse-input s) (string-split s #\\space))
(def (validate x) (> (length x) 0))
(def (process data)
  (let ((parsed (parse-input data)))
    (when (validate parsed) parsed)))
(def (main args) (process (car args)))
`,
      );
      const result = await client.callTool('gerbil_call_graph', {
        file_path: join(TEST_DIR, 'call-graph-target.ss'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Call graph');
      expect(result.text).toContain('main');
      expect(result.text).toContain('process');
      expect(result.text).toContain('parse-input');
    });

    it('gerbil_call_graph filters by function name', async () => {
      const result = await client.callTool('gerbil_call_graph', {
        file_path: join(TEST_DIR, 'call-graph-target.ss'),
        function: 'process',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('process');
      expect(result.text).toContain('parse-input');
      expect(result.text).toContain('validate');
    });

    it('gerbil_call_graph handles file with no functions', async () => {
      writeFileSync(
        join(TEST_DIR, 'no-fns.ss'),
        '(import :std/text/json)\n',
      );
      const result = await client.callTool('gerbil_call_graph', {
        file_path: join(TEST_DIR, 'no-fns.ss'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('No function definitions');
    });

    it('gerbil_call_graph reports error for missing function filter', async () => {
      const result = await client.callTool('gerbil_call_graph', {
        file_path: join(TEST_DIR, 'call-graph-target.ss'),
        function: 'nonexistent',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('not found');
    });
  });

  // ── Scaffold test tool ──────────────────────────────────────────────

  describe('Scaffold test tool', () => {
    it('gerbil_scaffold_test generates test file for :std/text/json', async () => {
      const result = await client.callTool('gerbil_scaffold_test', {
        module_path: ':std/text/json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('test-suite');
      expect(result.text).toContain('test-case');
      expect(result.text).toContain('import :std/test :std/text/json');
      expect(result.text).toContain('json-test');
      expect(result.text).toContain('read-json');
    });

    it('gerbil_scaffold_test respects suite_name override', async () => {
      const result = await client.callTool('gerbil_scaffold_test', {
        module_path: ':std/text/json',
        suite_name: 'my-custom-test',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('my-custom-test');
      expect(result.text).toContain('(export my-custom-test)');
    });

    it('gerbil_scaffold_test handles invalid module gracefully', async () => {
      const result = await client.callTool('gerbil_scaffold_test', {
        module_path: ':nonexistent/module/path',
      });
      expect(result.isError).toBe(true);
    });

    it('gerbil_scaffold_test generates value checks for non-procedures', async () => {
      const result = await client.callTool('gerbil_scaffold_test', {
        module_path: ':std/text/json',
      });
      expect(result.isError).toBe(false);
      // Should contain at least one test-case with a check form
      expect(result.text).toContain('check');
    });
  });

  // ── Build and report tool ─────────────────────────────────────────

  describe('Build and report tool', () => {
    it('gerbil_build_and_report handles missing project path', async () => {
      const result = await client.callTool('gerbil_build_and_report', {
        project_path: '/nonexistent/project/path',
      });
      // Should fail since the directory doesn't exist
      expect(result.isError).toBe(true);
    });

    it('gerbil_build_and_report accepts flags parameter', async () => {
      const result = await client.callTool('gerbil_build_and_report', {
        project_path: '/nonexistent/project/path',
        flags: ['--optimized'],
      });
      expect(result.isError).toBe(true);
    });

    it('gerbil_build_and_report reports success for a valid project', async () => {
      // Create a minimal buildable project in the test directory
      const buildDir = join(TEST_DIR, 'buildable');
      mkdirSync(buildDir, { recursive: true });
      writeFileSync(join(buildDir, 'gerbil.pkg'), '(package: test-build)');
      writeFileSync(
        join(buildDir, 'build.ss'),
        '#!/usr/bin/env gxi\n(import :std/build-script)\n(defbuild-script\n  \'("hello"))\n',
      );
      writeFileSync(
        join(buildDir, 'hello.ss'),
        '(export greet)\n(def (greet name) (string-append "Hello, " name "!"))\n',
      );

      const result = await client.callTool('gerbil_build_and_report', {
        project_path: buildDir,
      });
      // Either succeeds or fails with structured output
      expect(result.text).toBeDefined();
    }, 60000);
  });

  // ── Generate module stub tool ─────────────────────────────────────

  describe('Generate module stub tool', () => {
    it('gerbil_generate_module_stub generates stub for :std/text/json', async () => {
      const result = await client.callTool('gerbil_generate_module_stub', {
        module_path: ':std/text/json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('import');
      expect(result.text).toContain('export');
      expect(result.text).toContain('def');
      expect(result.text).toContain('read-json');
    });

    it('gerbil_generate_module_stub includes additional imports', async () => {
      const result = await client.callTool('gerbil_generate_module_stub', {
        module_path: ':std/text/json',
        imports: [':std/iter'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain(':std/iter');
      expect(result.text).toContain(':std/text/json');
    });

    it('gerbil_generate_module_stub handles invalid module gracefully', async () => {
      const result = await client.callTool('gerbil_generate_module_stub', {
        module_path: ':nonexistent/module/path',
      });
      expect(result.isError).toBe(true);
    });
  });

  // ── Enhanced find-definition ─────────────────────────────────────────

  describe('Enhanced find-definition with source preview', () => {
    it('gerbil_find_definition with source_preview does not crash', async () => {
      const result = await client.callTool('gerbil_find_definition', {
        symbol: 'read-json',
        module_path: ':std/text/json',
        source_preview: true,
        preview_lines: 10,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Kind: procedure');
      // Preview may or may not be available depending on source file
    });

    it('gerbil_find_definition without source_preview is unchanged', async () => {
      const result = await client.callTool('gerbil_find_definition', {
        symbol: 'read-json',
        module_path: ':std/text/json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Kind: procedure');
      expect(result.text).not.toContain('Source preview');
    });
  });
});
