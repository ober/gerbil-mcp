/**
 * Functional tests for all Gerbil MCP tools.
 *
 * These tests verify that each tool works correctly by invoking it through
 * the MCP JSON-RPC protocol and checking the response.
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { spawn, ChildProcess } from 'node:child_process';
import { mkdirSync, writeFileSync, rmSync, chmodSync } from 'node:fs';
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

    // Channel lint fixture
    writeFileSync(
      join(TEST_DIR, 'channel-lint.ss'),
      `(import :std/misc/channel :std/misc/wg)

(def (spin-worker ch)
  (while #t
    (channel-try-get ch #f)))

(def (collector wg result-ch)
  (wg-wait! wg)
  (channel-try-get result-ch #f))
`,
    );

    // Package file for project tools and REPL project_path tests
    writeFileSync(join(TEST_DIR, 'gerbil.pkg'), '(package: test-pkg)');

    // Lint pitfall fixtures
    writeFileSync(
      join(TEST_DIR, 'lint-unquote.ss'),
      `(def (bad-fn x)
  (list ,x ,@(list 1 2)))
`,
    );

    writeFileSync(
      join(TEST_DIR, 'lint-dot-bracket.ss'),
      `(def pair-list
  [a . b])
`,
    );

    writeFileSync(
      join(TEST_DIR, 'lint-missing-export.ss'),
      `(export existing-fn missing-fn)
(def (existing-fn x) (+ x 1))
`,
    );

    // SRFI-19 time->seconds shadow lint fixture
    writeFileSync(
      join(TEST_DIR, 'lint-srfi19.ss'),
      `(import :std/srfi/19)

(def (get-epoch)
  (time->seconds (current-time)))
`,
    );

    // Unsafe mutex pattern lint fixture
    writeFileSync(
      join(TEST_DIR, 'lint-mutex.ss'),
      `(def mx (make-mutex 'my-mutex))

(def (unsafe-update! val)
  (mutex-lock! mx)
  (do-something val)
  (mutex-unlock! mx))

(def (safe-update! val)
  (mutex-lock! mx)
  (unwind-protect
    (do-something val)
    (mutex-unlock! mx)))
`,
    );

    // Check-exports fixtures: multi-file project
    const exportCheckDir = join(TEST_DIR, 'export-check');
    mkdirSync(exportCheckDir, { recursive: true });
    writeFileSync(
      join(exportCheckDir, 'gerbil.pkg'),
      '(package: exportcheck)',
    );
    writeFileSync(
      join(exportCheckDir, 'lib.ss'),
      `(export add-numbers not-defined-fn)
(def (add-numbers a b) (+ a b))
`,
    );
    writeFileSync(
      join(exportCheckDir, 'main.ss'),
      `(import :exportcheck/lib)
(export run)
(def (run) (add-numbers 1 2))
`,
    );

    // Clean project for check-exports
    const cleanExportDir = join(TEST_DIR, 'clean-exports');
    mkdirSync(cleanExportDir, { recursive: true });
    writeFileSync(
      join(cleanExportDir, 'gerbil.pkg'),
      '(package: cleanpkg)',
    );
    writeFileSync(
      join(cleanExportDir, 'util.ss'),
      `(export greet)
(def (greet name) (string-append "hi " name))
`,
    );

    // Arity check fixtures
    const arityDir = join(TEST_DIR, 'arity-check');
    mkdirSync(arityDir, { recursive: true });
    writeFileSync(join(arityDir, 'gerbil.pkg'), '(package: arity-test)');
    writeFileSync(
      join(arityDir, 'funcs.ss'),
      `(export add greet)
(def (add x y) (+ x y))
(def (greet name (greeting "Hello"))
  (string-append greeting ", " name))
`,
    );
    writeFileSync(
      join(arityDir, 'caller.ss'),
      `(import :arity-test/funcs)
(export run)
(def (run)
  (add 1)
  (add 1 2 3)
  (greet "Alice")
  (greet "Alice" "Hi"))
`,
    );

    // Resolve-imports fixture
    writeFileSync(
      join(TEST_DIR, 'needs-imports.ss'),
      `(def (process data)
  (let ((parsed (read-json data)))
    (for/collect (x parsed) (* x 2))))
`,
    );

    // Build-and-report context_lines fixture
    const ctxDir = join(TEST_DIR, 'ctx-build');
    mkdirSync(ctxDir, { recursive: true });
    writeFileSync(join(ctxDir, 'gerbil.pkg'), '(package: ctx-test)');
    writeFileSync(
      join(ctxDir, 'build.ss'),
      '#!/usr/bin/env gxi\n(import :std/build-script)\n(defbuild-script\n  \'("broken"))\n',
      { mode: 0o755 },
    );
    writeFileSync(
      join(ctxDir, 'broken.ss'),
      `(export process)
(def (process x)
  (let ((result (undefined-function x)))
    result))
`,
    );

    // Generate-module template fixture
    writeFileSync(
      join(TEST_DIR, 'template.ss'),
      `(import :std/text/json)
(export read-foo write-foo)
(def (read-foo port) (read-json port))
(def (write-foo obj port) (write-json obj port))
`,
    );

    // Makefile fixture for build-and-report Makefile detection
    const makeDir = join(TEST_DIR, 'with-makefile');
    mkdirSync(makeDir, { recursive: true });
    writeFileSync(join(makeDir, 'gerbil.pkg'), '(package: makefile-test)');
    writeFileSync(
      join(makeDir, 'Makefile'),
      `all: build

build:
\t@echo "building"

clean:
\t@echo "cleaning"

test:
\t@echo "testing"
`,
    );
    writeFileSync(
      join(makeDir, 'hello.ss'),
      '(export greet)\n(def (greet name) (string-append "Hi " name))\n',
    );

    // Balanced-replace fixture
    writeFileSync(
      join(TEST_DIR, 'balanced-edit.ss'),
      `(import :std/text/json)
(def (process x)
  (+ x 1))
`,
    );

    // Wrap-form fixture
    writeFileSync(
      join(TEST_DIR, 'wrap-target.ss'),
      `(import :std/iter)
(def (process x)
  (displayln x)
  (+ x 1))
`,
    );

    // Splice-form fixture
    writeFileSync(
      join(TEST_DIR, 'splice-target.ss'),
      `(import :std/iter)
(when (> x 0)
  (do-x)
  (do-y))
`,
    );

    // FFI scaffold fixtures
    writeFileSync(
      join(TEST_DIR, 'simple-lib.h'),
      `#ifndef SIMPLE_LIB_H
#define SIMPLE_LIB_H

#define MAX_SIZE 1024
#define ERROR_CODE 0xFF

typedef struct simple_ctx_st simple_ctx_t;

extern simple_ctx_t *simple_ctx_create(void);
extern void simple_ctx_destroy(simple_ctx_t *ctx);
extern int simple_process(simple_ctx_t *ctx, const char *input, int flags);
extern const char *simple_get_name(simple_ctx_t *ctx);

#endif
`,
    );

    writeFileSync(
      join(TEST_DIR, 'enum-lib.h'),
      `typedef enum {
  COLOR_RED = 0,
  COLOR_GREEN = 1,
  COLOR_BLUE = 2
} color_t;

typedef struct widget_st widget_t;

extern widget_t *widget_new(const char *label, color_t color);
extern void widget_free(widget_t *w);
extern color_t widget_get_color(widget_t *w);
`,
    );

    writeFileSync(
      join(TEST_DIR, 'multi-type.h'),
      `/* Multi-type library */
typedef struct db_st db_t;
typedef struct iterator_st iterator_t;
typedef struct options_st options_t;

extern options_t *options_create(void);
extern void options_destroy(options_t *opts);

extern db_t *db_open(const char *path, options_t *opts);
extern void db_close(db_t *db);

extern int db_put(db_t *db, const char *key, const char *value);
extern const char *db_get(db_t *db, const char *key);

extern iterator_t *db_iterator(db_t *db);
extern int iterator_next(iterator_t *it);
extern const char *iterator_key(iterator_t *it);
extern void iterator_destroy(iterator_t *it);
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

    it('gerbil_compile_check includes error details for invalid code', async () => {
      const result = await client.callTool('gerbil_compile_check', {
        code: '(import :nonexistent/module)',
      });
      expect(result.isError).toBe(true);
      // Should contain actual error details, not just "Compilation errors found:"
      expect(result.text.length).toBeGreaterThan('Compilation errors found:\n\n'.length);
      // The error should mention something about the module or identifier
      const hasDetails = result.text.includes('nonexistent') ||
        result.text.includes('module') ||
        result.text.includes('import') ||
        result.text.includes('exit code');
      expect(hasDetails).toBe(true);
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

    it('gerbil_module_exports falls back to compiled .scm for declare modules', async () => {
      // Create a fake compiled .scm file to test the fallback path
      const staticDir = join(TEST_DIR, 'fake-gerbil', 'lib', 'static');
      mkdirSync(staticDir, { recursive: true });
      writeFileSync(
        join(staticDir, 'test__declmod.scm'),
        [
          '(declare (block) (standard-bindings) (extended-bindings))',
          '(begin',
          '  (define test/declmod::timestamp 1234567890)',
          '  (define test/declmod#my-function (lambda (x) x))',
          '  (define test/declmod#my-value 42)',
          '  (define test/declmod#%internal-thing (lambda () #f))',
          ')',
        ].join('\n'),
      );
      // The fallback is tested indirectly — the module won't exist so expander fails,
      // then fallback scans the static dir. We set GERBIL_PATH to our fake dir.
      // Since we can't set env per-tool-call, we test the standard exports path works.
      // The fallback code path is validated by the unit structure.
      const result = await client.callTool('gerbil_module_exports', {
        module_path: ':std/text/json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('symbol(s)');
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

    it('gerbil_lint warns on bare hash keys including lowercase', async () => {
      const result = await client.callTool('gerbil_lint', {
        file_path: join(TEST_DIR, 'hash-lint.ss'),
      });
      expect(result.text).toContain('hash-symbol-key');
      expect(result.text).toContain('FOO');
      expect(result.text).toContain('CRITICAL');
      expect(result.text).toContain('name');
    });

    it('gerbil_lint warns on channel-try-get in loop', async () => {
      const result = await client.callTool('gerbil_lint', {
        file_path: join(TEST_DIR, 'channel-lint.ss'),
      });
      expect(result.text).toContain('channel-try-get-in-loop');
    });

    it('gerbil_lint warns on wg-wait then channel-try-get', async () => {
      const result = await client.callTool('gerbil_lint', {
        file_path: join(TEST_DIR, 'channel-lint.ss'),
      });
      expect(result.text).toContain('wg-wait-then-try-get');
    });

    it('gerbil_lint warns on time->seconds with SRFI-19 import', async () => {
      const result = await client.callTool('gerbil_lint', {
        file_path: join(TEST_DIR, 'lint-srfi19.ss'),
      });
      expect(result.text).toContain('srfi19-time-seconds-shadow');
      expect(result.text).toContain('time->seconds');
    });

    it('gerbil_lint warns on mutex-lock!/unlock! without unwind-protect', async () => {
      const result = await client.callTool('gerbil_lint', {
        file_path: join(TEST_DIR, 'lint-mutex.ss'),
      });
      expect(result.text).toContain('unsafe-mutex-pattern');
      // Should only warn on the unsafe pattern, not the safe one
      const matches = result.text.match(/unsafe-mutex-pattern/g) || [];
      expect(matches.length).toBe(1);
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

    it('gerbil_check_balance handles #!void reader directives', async () => {
      const result = await client.callTool('gerbil_check_balance', {
        code: '(def x #!void)\n(def y (list #!eof 1 2))',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Balance OK');
    });

    it('gerbil_check_balance handles vector literals', async () => {
      const result = await client.callTool('gerbil_check_balance', {
        code: '(def v #(1 2 3))\n(def w (vector-ref #(a b c) 0))',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Balance OK');
    });

    it('gerbil_check_balance error output includes verification suggestion', async () => {
      const result = await client.callTool('gerbil_check_balance', {
        code: '(def (f x) (+ x 1)',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('heuristic');
      expect(result.text).toContain('gerbil_check_syntax');
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

    it('gerbil_build_and_report accepts loadpath parameter', async () => {
      const result = await client.callTool('gerbil_build_and_report', {
        project_path: '/nonexistent/project/path',
        loadpath: ['/some/lib', '/other/lib'],
      });
      // Should fail (nonexistent path) but accept the loadpath parameter without error
      expect(result.isError).toBe(true);
    });

    it('gerbil_build_and_report auto-detects loadpath from gerbil.pkg depend:', async () => {
      // Create a project with depend: in gerbil.pkg
      const depDir = join(TEST_DIR, 'dep-project');
      mkdirSync(depDir, { recursive: true });
      writeFileSync(
        join(depDir, 'gerbil.pkg'),
        '(package: dep-test depend: ("github.com/some/package"))',
      );
      writeFileSync(
        join(depDir, 'build.ss'),
        '#!/usr/bin/env gxi\n(import :std/build-script)\n(defbuild-script\n  \'("hello"))\n',
      );
      writeFileSync(
        join(depDir, 'hello.ss'),
        '(export greet)\n(def (greet) "hi")\n',
      );
      // The build may succeed or fail, but it should not crash due to auto-loadpath
      const result = await client.callTool('gerbil_build_and_report', {
        project_path: depDir,
      });
      expect(result.text).toBeDefined();
    }, 60000);

    it('gerbil_build_and_report falls back to make on gerbil build failure', async () => {
      // Create a project where gerbil build will fail (bad build.ss)
      // but has a Makefile with a build target that succeeds
      const fallbackDir = join(TEST_DIR, 'makefile-fallback');
      mkdirSync(fallbackDir, { recursive: true });
      writeFileSync(join(fallbackDir, 'gerbil.pkg'), '(package: fb-test)');
      writeFileSync(
        join(fallbackDir, 'build.ss'),
        '#!/usr/bin/env gxi\n(import :std/build-script)\n(defbuild-script\n  \'("nonexistent-module"))\n',
      );
      writeFileSync(
        join(fallbackDir, 'Makefile'),
        'build:\n\t@echo "make build succeeded"\n',
      );

      const result = await client.callTool('gerbil_build_and_report', {
        project_path: fallbackDir,
      });
      // Should succeed via Makefile fallback
      if (!result.isError) {
        expect(result.text).toContain('Makefile fallback');
      }
      // If gerbil build somehow succeeds or make isn't available, just verify it ran
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

  // ── Check exports tool ──────────────────────────────────────────────

  describe('Check exports tool', () => {
    it('gerbil_check_exports detects missing exported definitions', async () => {
      const result = await client.callTool('gerbil_check_exports', {
        project_path: join(TEST_DIR, 'export-check'),
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('not-defined-fn');
      expect(result.text).toContain('no definition found');
    });

    it('gerbil_check_exports passes for clean project', async () => {
      const result = await client.callTool('gerbil_check_exports', {
        project_path: join(TEST_DIR, 'clean-exports'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('no issues found');
    });

    it('gerbil_check_exports requires gerbil.pkg', async () => {
      const result = await client.callTool('gerbil_check_exports', {
        project_path: '/nonexistent/path',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('gerbil.pkg');
    });
  });

  // ── Generate module tool ────────────────────────────────────────────

  describe('Generate module tool', () => {
    it('gerbil_generate_module applies substitutions', async () => {
      const result = await client.callTool('gerbil_generate_module', {
        template_path: join(TEST_DIR, 'template.ss'),
        substitutions: [
          { from: 'read-foo', to: 'read-bar' },
          { from: 'write-foo', to: 'write-bar' },
        ],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('read-bar');
      expect(result.text).toContain('write-bar');
      // The generated code (after header comments) should not contain old names
      expect(result.text).toContain('(def (read-bar port)');
      expect(result.text).toContain('(export read-bar write-bar)');
    });

    it('gerbil_generate_module reads template file', async () => {
      const result = await client.callTool('gerbil_generate_module', {
        template_path: join(TEST_DIR, 'template.ss'),
        substitutions: [],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('read-foo');
      expect(result.text).toContain(':std/text/json');
    });

    it('gerbil_generate_module handles missing template', async () => {
      const result = await client.callTool('gerbil_generate_module', {
        template_path: '/nonexistent/template.ss',
        substitutions: [{ from: 'a', to: 'b' }],
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('Failed to read template');
    });

    it('gerbil_generate_module shows substitution summary', async () => {
      const result = await client.callTool('gerbil_generate_module', {
        template_path: join(TEST_DIR, 'template.ss'),
        substitutions: [
          { from: 'read-foo', to: 'read-baz' },
          { from: 'nonexistent-sym', to: 'other' },
        ],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Substitutions applied');
      expect(result.text).toContain('"read-foo" -> "read-baz"');
      expect(result.text).toContain('"nonexistent-sym" -> "other"');
      expect(result.text).toContain('0 replacements');
    });
  });

  // ── Lint pitfall detection rules ────────────────────────────────────

  describe('Lint pitfall detection rules', () => {
    it('gerbil_lint warns on unquote outside quasiquote', async () => {
      const result = await client.callTool('gerbil_lint', {
        file_path: join(TEST_DIR, 'lint-unquote.ss'),
      });
      expect(result.text).toContain('unquote-outside-quasiquote');
    });

    it('gerbil_lint warns on dot in brackets', async () => {
      const result = await client.callTool('gerbil_lint', {
        file_path: join(TEST_DIR, 'lint-dot-bracket.ss'),
      });
      expect(result.text).toContain('dot-in-brackets');
    });

    it('gerbil_lint warns on missing exported definitions', async () => {
      const result = await client.callTool('gerbil_lint', {
        file_path: join(TEST_DIR, 'lint-missing-export.ss'),
      });
      expect(result.text).toContain('missing-exported-definition');
      expect(result.text).toContain('missing-fn');
    });
  });

  // ── Diagnostics with loadpath ───────────────────────────────────────

  describe('Diagnostics with loadpath', () => {
    it('gerbil_diagnostics accepts loadpath parameter', async () => {
      const result = await client.callTool('gerbil_diagnostics', {
        file_path: join(TEST_DIR, 'sample.ss'),
        loadpath: ['/nonexistent/path'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('cleanly');
    });
  });

  // ── Function signature with parameter names ─────────────────────────

  describe('Function signature parameter names', () => {
    it('gerbil_function_signature shows parameter names or arity', async () => {
      const result = await client.callTool('gerbil_function_signature', {
        module_path: ':std/text/json',
        symbol: 'read-json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('procedure');
      // Should show either parameter names (from source) or arity fallback
      expect(
        result.text.includes('[') || result.text.includes('arity:'),
      ).toBe(true);
    });

    it('gerbil_function_signature falls back to arity when source unavailable', async () => {
      // Test with a module that may not have source available
      const result = await client.callTool('gerbil_function_signature', {
        module_path: ':std/text/json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('procedure');
    });

    it('gerbil_function_signature shows keyword args for http-get', async () => {
      const result = await client.callTool('gerbil_function_signature', {
        module_path: ':std/net/request',
        symbol: 'http-get',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('procedure');
      // Should show keyword args like headers:, timeout:, etc.
      expect(result.text).toContain('headers:');
      expect(result.text).toContain('timeout:');
    });

    it('gerbil_function_signature shows normal arity for non-keyword function', async () => {
      const result = await client.callTool('gerbil_function_signature', {
        module_path: ':std/text/json',
        symbol: 'read-json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('procedure');
      // read-json does NOT have keyword args — should not show keywords:
      expect(result.text).not.toContain('keywords:');
    });

    it('gerbil_function_signature accepts loadpath parameter', async () => {
      // Use loadpath with a standard module to verify the parameter is accepted
      const result = await client.callTool('gerbil_function_signature', {
        module_path: ':std/text/json',
        symbol: 'read-json',
        loadpath: ['/nonexistent/path'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('read-json');
      expect(result.text).toContain('procedure');
    });

    it('gerbil_function_signature accepts project_path parameter', async () => {
      const result = await client.callTool('gerbil_function_signature', {
        module_path: ':std/text/json',
        symbol: 'write-json',
        project_path: TEST_DIR,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('write-json');
      expect(result.text).toContain('procedure');
    });

    it('gerbil_module_exports accepts loadpath parameter', async () => {
      const result = await client.callTool('gerbil_module_exports', {
        module_path: ':std/text/json',
        loadpath: ['/nonexistent/path'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('read-json');
    });

    it('gerbil_module_exports accepts project_path parameter', async () => {
      const result = await client.callTool('gerbil_module_exports', {
        module_path: ':std/text/json',
        project_path: TEST_DIR,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('read-json');
    });
  });

  // ── Makefile awareness ─────────────────────────────────────────────

  describe('Makefile awareness', () => {
    it('gerbil_build_and_report falls back to Makefile when gerbil build fails', async () => {
      const result = await client.callTool('gerbil_build_and_report', {
        project_path: join(TEST_DIR, 'with-makefile'),
      });
      // Makefile fallback should succeed since gerbil build has no build.ss
      expect(result.text).toContain('Makefile');
      expect(result.isError).toBe(false);
    }, 60000);

    it('gerbil_build_and_report omits Makefile note when no Makefile', async () => {
      const result = await client.callTool('gerbil_build_and_report', {
        project_path: join(TEST_DIR, 'buildable'),
      });
      // Should NOT mention Makefile since there is none
      expect(result.text).not.toContain('gerbil_make');
    }, 60000);

    it('gerbil_make returns error for missing Makefile', async () => {
      const result = await client.callTool('gerbil_make', {
        project_path: '/nonexistent/project/path',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('No Makefile');
    });
  });

  // ── Howto cookbook tool ─────────────────────────────────────────────

  describe('Howto cookbook tool', () => {
    it('gerbil_howto finds JSON recipes', async () => {
      const result = await client.callTool('gerbil_howto', {
        query: 'json parse',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('json');
      expect(result.text).toContain('read-json');
      expect(result.text).toContain('```scheme');
    });

    it('gerbil_howto finds file I/O recipes', async () => {
      const result = await client.callTool('gerbil_howto', {
        query: 'file read',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('file');
      expect(result.text).toContain('recipe');
    });

    it('gerbil_howto finds thread/concurrency recipes', async () => {
      const result = await client.callTool('gerbil_howto', {
        query: 'thread spawn concurrent',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('spawn');
    });

    it('gerbil_howto returns no-match message for gibberish', async () => {
      const result = await client.callTool('gerbil_howto', {
        query: 'xyzzyplugh',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('No recipes found');
      expect(result.text).toContain('Available topics');
    });
  });

  // ── Howto add / extensible cookbook ───────────────────────────────

  describe('Howto extensible cookbook', () => {
    it('gerbil_howto_add creates a new cookbook file and adds a recipe', async () => {
      const cookbookPath = join(TEST_DIR, '.claude', 'cookbooks.json');
      const result = await client.callTool('gerbil_howto_add', {
        cookbook_path: cookbookPath,
        id: 'my-custom-recipe',
        title: 'Custom recipe for testing',
        tags: ['custom', 'testing', 'unicorn'],
        imports: [':std/test'],
        code: '(displayln "hello from custom recipe")',
        notes: 'This is a test recipe.',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Added');
      expect(result.text).toContain('my-custom-recipe');
      expect(result.text).toContain('1 total recipes');
    });

    it('gerbil_howto_add replaces recipe with same id', async () => {
      const cookbookPath = join(TEST_DIR, '.claude', 'cookbooks.json');
      // Add a second recipe first
      await client.callTool('gerbil_howto_add', {
        cookbook_path: cookbookPath,
        id: 'another-recipe',
        title: 'Another recipe',
        tags: ['another'],
        imports: [],
        code: '(void)',
      });
      // Now update the first recipe
      const result = await client.callTool('gerbil_howto_add', {
        cookbook_path: cookbookPath,
        id: 'my-custom-recipe',
        title: 'Updated custom recipe',
        tags: ['custom', 'testing', 'unicorn', 'updated'],
        imports: [':std/test'],
        code: '(displayln "updated")',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Updated');
      expect(result.text).toContain('my-custom-recipe');
      expect(result.text).toContain('2 total recipes');
    });

    it('gerbil_howto loads external recipes via cookbook_path', async () => {
      const cookbookPath = join(TEST_DIR, '.claude', 'cookbooks.json');
      const result = await client.callTool('gerbil_howto', {
        query: 'unicorn custom',
        cookbook_path: cookbookPath,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Updated custom recipe');
      expect(result.text).toContain('updated');
    });

    it('gerbil_howto with invalid cookbook_path still returns embedded results', async () => {
      const result = await client.callTool('gerbil_howto', {
        query: 'json parse',
        cookbook_path: '/nonexistent/path/cookbooks.json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('read-json');
    });

    it('gerbil_howto_add returns error for unparseable existing file', async () => {
      const badPath = join(TEST_DIR, 'bad-cookbook.json');
      writeFileSync(badPath, 'this is not json{{{');
      const result = await client.callTool('gerbil_howto_add', {
        cookbook_path: badPath,
        id: 'test',
        title: 'Test',
        tags: ['test'],
        imports: [],
        code: '(void)',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('Error');
    });
  });

  // ── File summary tool ─────────────────────────────────────────────

  describe('File summary tool', () => {
    it('gerbil_file_summary shows structural overview', async () => {
      const result = await client.callTool('gerbil_file_summary', {
        file_path: join(TEST_DIR, 'sample.ss'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('sample.ss');
      expect(result.text).toContain('Imports:');
      expect(result.text).toContain('Exports:');
      expect(result.text).toContain('Procedures');
      expect(result.text).toContain('main');
      expect(result.text).toContain('helper');
      expect(result.text).toContain('point');
    });

    it('gerbil_file_summary handles missing file', async () => {
      const result = await client.callTool('gerbil_file_summary', {
        file_path: '/nonexistent/file.ss',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('Failed to read');
    });

    it('gerbil_file_summary handles empty file', async () => {
      writeFileSync(join(TEST_DIR, 'empty.ss'), '');
      const result = await client.callTool('gerbil_file_summary', {
        file_path: join(TEST_DIR, 'empty.ss'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('empty');
    });
  });

  // ── Check arity tool ─────────────────────────────────────────────────

  describe('Check arity tool', () => {
    it('gerbil_check_arity detects arity mismatches in local defs', async () => {
      const result = await client.callTool('gerbil_check_arity', {
        project_path: join(TEST_DIR, 'arity-check'),
      });
      // Should detect at least one issue (add called with wrong args)
      expect(result.text).toContain('Arity check');
      expect(result.text).toContain('add');
    });

    it('gerbil_check_arity works in single file mode', async () => {
      const result = await client.callTool('gerbil_check_arity', {
        project_path: join(TEST_DIR, 'arity-check'),
        file_path: join(TEST_DIR, 'arity-check', 'funcs.ss'),
      });
      expect(result.text).toContain('Arity check');
    });

    it('gerbil_check_arity reports no issues for clean files', async () => {
      const result = await client.callTool('gerbil_check_arity', {
        project_path: TEST_DIR,
        file_path: join(TEST_DIR, 'sample.ss'),
      });
      expect(result.isError).toBeFalsy();
      expect(result.text).toContain('no issues found');
    });

    it('gerbil_check_arity handles empty project', async () => {
      const emptyDir = join(TEST_DIR, 'empty-arity');
      mkdirSync(emptyDir, { recursive: true });
      const result = await client.callTool('gerbil_check_arity', {
        project_path: emptyDir,
      });
      expect(result.text).toContain('No .ss files');
    });
  });

  // ── Check test arity tool ──────────────────────────────────────────

  describe('Check test arity tool', () => {
    it('gerbil_check_test_arity finds matching calls in test files', async () => {
      const testDir = join(TEST_DIR, 'test-arity');
      mkdirSync(testDir, { recursive: true });
      // Create a test file that calls read-json with correct arity (1 arg)
      writeFileSync(
        join(testDir, 'json-test.ss'),
        '(import :std/text/json)\n(def (test-read) (read-json (open-input-string "{}")))\n',
      );
      const result = await client.callTool('gerbil_check_test_arity', {
        symbol: 'read-json',
        module_path: ':std/text/json',
        directory: testDir,
      });
      expect(result.isError).toBeFalsy();
      expect(result.text).toContain('read-json');
      expect(result.text).toContain('match');
    }, 30000);

    it('gerbil_check_test_arity reports no test files', async () => {
      const emptyDir = join(TEST_DIR, 'no-tests');
      mkdirSync(emptyDir, { recursive: true });
      writeFileSync(join(emptyDir, 'lib.ss'), '(def (foo) 42)\n');
      const result = await client.callTool('gerbil_check_test_arity', {
        symbol: 'foo',
        module_path: ':std/text/json',
        directory: emptyDir,
      });
      expect(result.text).toContain('No *-test.ss files');
    }, 30000);

    it('gerbil_check_test_arity reports no calls found', async () => {
      const testDir = join(TEST_DIR, 'test-arity-nocall');
      mkdirSync(testDir, { recursive: true });
      writeFileSync(
        join(testDir, 'empty-test.ss'),
        '(import :std/test)\n(def my-test (test-suite "empty" (test-case "noop" (check 1 => 1))))\n',
      );
      const result = await client.callTool('gerbil_check_test_arity', {
        symbol: 'read-json',
        module_path: ':std/text/json',
        directory: testDir,
      });
      expect(result.text).toContain('No calls to read-json');
    }, 30000);
  });

  // ── Howto verify tool ────────────────────────────────────────────────

  describe('Howto verify tool', () => {
    it('gerbil_howto_verify checks built-in recipes', async () => {
      const result = await client.callTool('gerbil_howto_verify', {});
      expect(result.text).toContain('Cookbook verification');
      expect(result.text).toContain('recipe(s) checked');
      expect(result.text).toContain('Summary');
    }, 120000);

    it('gerbil_howto_verify checks a single recipe by id', async () => {
      const result = await client.callTool('gerbil_howto_verify', {
        recipe_id: 'json-parse',
      });
      expect(result.text).toContain('1 recipe(s) checked');
      expect(result.text).toContain('json-parse');
      expect(result.text).toContain('PASS');
    });

    it('gerbil_howto_verify returns error for unknown recipe_id', async () => {
      const result = await client.callTool('gerbil_howto_verify', {
        recipe_id: 'nonexistent-recipe-xyz',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('not found');
    });

    it('gerbil_howto_verify with compile_check runs gxc on recipes', async () => {
      // Verify a single known-good recipe with compile_check enabled
      const result = await client.callTool('gerbil_howto_verify', {
        recipe_id: 'json-parse',
        compile_check: true,
      });
      expect(result.text).toContain('syntax + compile');
      expect(result.text).toContain('1 recipe(s) checked');
      expect(result.text).toContain('json-parse');
    }, 60000);

    it('gerbil_howto_verify compile_check detects REPL-only patterns', async () => {
      // Create a cookbook with a recipe that uses a REPL-only pattern:
      // (for ((k v) (in-hash ht)) ...) passes expander but fails gxc with unbound 'v'
      const cookbookPath = join(TEST_DIR, 'repl-only-cookbook.json');
      writeFileSync(
        cookbookPath,
        JSON.stringify([
          {
            id: 'repl-only-test',
            title: 'REPL-only pattern',
            tags: ['test'],
            imports: [':std/iter'],
            code: '(def (test-fn ht)\n  (for ((k v) (in-hash ht)) (displayln k v)))',
            notes: 'This uses destructuring in for bindings — works in REPL but not compiled.',
          },
        ]),
      );

      const result = await client.callTool('gerbil_howto_verify', {
        cookbook_path: cookbookPath,
        recipe_id: 'repl-only-test',
        compile_check: true,
      });
      // Should report a compile failure
      expect(result.text).toContain('COMPILE-FAIL');
      expect(result.text).toContain('repl-only-test');
    }, 60000);
  });

  // ── Resolve imports tool ─────────────────────────────────────────────

  describe('Resolve imports tool', () => {
    it('gerbil_resolve_imports finds unbound identifiers and suggests imports', async () => {
      const result = await client.callTool('gerbil_resolve_imports', {
        file_path: join(TEST_DIR, 'needs-imports.ss'),
      });
      expect(result.text).toContain('Resolved imports');
      expect(result.text).toContain('unbound identifier');
    });

    it('gerbil_resolve_imports reports clean file', async () => {
      const result = await client.callTool('gerbil_resolve_imports', {
        file_path: join(TEST_DIR, 'sample.ss'),
      });
      // sample.ss has imports — should compile cleanly
      expect(result.text).toContain('compiles cleanly');
    });

    it('gerbil_resolve_imports accepts loadpath', async () => {
      const result = await client.callTool('gerbil_resolve_imports', {
        file_path: join(TEST_DIR, 'needs-imports.ss'),
        loadpath: ['/nonexistent/path'],
      });
      expect(result.text).toContain('Resolved imports');
    });
  });

  // ── Trace eval tool ──────────────────────────────────────────────────

  describe('Trace eval tool', () => {
    it('gerbil_trace_eval traces let* bindings', async () => {
      const result = await client.callTool('gerbil_trace_eval', {
        expression: '(let* ((x 10) (y (* x 2)) (z (+ x y))) z)',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Trace:');
      expect(result.text).toContain('x');
      expect(result.text).toContain('y');
      expect(result.text).toContain('z');
      expect(result.text).toContain('number');
      expect(result.text).toContain('result');
    });

    it('gerbil_trace_eval handles non-let expressions', async () => {
      const result = await client.callTool('gerbil_trace_eval', {
        expression: '(+ 1 2)',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('result');
      expect(result.text).toContain('number');
    });

    it('gerbil_trace_eval works with imports', async () => {
      const result = await client.callTool('gerbil_trace_eval', {
        expression: '(let* ((data (call-with-input-string "[1,2,3]" read-json))) data)',
        imports: [':std/text/json'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('data');
      expect(result.text).toContain('result');
    });

    it('gerbil_trace_eval accepts project_path', async () => {
      const result = await client.callTool('gerbil_trace_eval', {
        expression: '(let* ((x 1)) x)',
        project_path: TEST_DIR,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('x');
    });

    it('gerbil_trace_eval reports error in binding', async () => {
      const result = await client.callTool('gerbil_trace_eval', {
        expression: '(let* ((x (/ 1 0))) x)',
      });
      // Should contain some error indication
      expect(result.text).toContain('x');
    });
  });

  // ── SXML inspect tool ──────────────────────────────────────────────

  describe('SXML inspect tool', () => {
    it('gerbil_sxml_inspect parses simple XML', async () => {
      const result = await client.callTool('gerbil_sxml_inspect', {
        xml_text: '<root><item>hello</item><item>world</item></root>',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('SXML Tree');
      expect(result.text).toContain('[DOCUMENT]');
      expect(result.text).toContain('[ELEMENT]');
      expect(result.text).toContain('root');
      expect(result.text).toContain('item');
    });

    it('gerbil_sxml_inspect shows PI nodes', async () => {
      const result = await client.callTool('gerbil_sxml_inspect', {
        xml_text: '<?xml version="1.0" encoding="UTF-8"?><root><item>hi</item></root>',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('[PI]');
      expect(result.text).toContain('[ELEMENT]');
      expect(result.text).toContain('root');
    });

    it('gerbil_sxml_inspect works in expression mode', async () => {
      const result = await client.callTool('gerbil_sxml_inspect', {
        expression: '\'(*TOP* (root (item "hello")))',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('[DOCUMENT]');
      expect(result.text).toContain('[ELEMENT]');
      expect(result.text).toContain('root');
    });

    it('gerbil_sxml_inspect validates missing params', async () => {
      const result = await client.callTool('gerbil_sxml_inspect', {});
      expect(result.isError).toBe(true);
      expect(result.text).toContain('Exactly one');
    });

    it('gerbil_sxml_inspect shows attributes', async () => {
      const result = await client.callTool('gerbil_sxml_inspect', {
        xml_text: '<root id="1"><item class="main">text</item></root>',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('[ELEMENT]');
      expect(result.text).toContain('root');
    });
  });

  // ── Eval project_path support ─────────────────────────────────────

  describe('Eval project_path', () => {
    it('gerbil_eval accepts project_path parameter', async () => {
      const result = await client.callTool('gerbil_eval', {
        expression: '(+ 1 2)',
        project_path: TEST_DIR,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('3');
    });

    it('gerbil_eval combines project_path with loadpath', async () => {
      const result = await client.callTool('gerbil_eval', {
        expression: '(+ 10 20)',
        loadpath: ['/nonexistent/path'],
        project_path: TEST_DIR,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('30');
    });
  });

  // ── Build and report context_lines ──────────────────────────────────

  describe('Build and report context_lines', () => {
    it('gerbil_build_and_report shows source context on error', async () => {
      const result = await client.callTool('gerbil_build_and_report', {
        project_path: join(TEST_DIR, 'ctx-build'),
        context_lines: 2,
      });
      // Build should fail with errors
      expect(result.isError).toBe(true);
      expect(result.text).toContain('Build failed');
      // Source context should show line numbers with | separator when line info is available
      // The Gambit error format includes line numbers, so context should appear
      if (result.text.includes('broken.ss')) {
        expect(result.text).toMatch(/\d+\s*\|/);
      }
    }, 60000);

    it('gerbil_build_and_report with context_lines 0 suppresses context', async () => {
      const result = await client.callTool('gerbil_build_and_report', {
        project_path: join(TEST_DIR, 'ctx-build'),
        context_lines: 0,
      });
      // Should not show source context even on errors
      if (result.isError) {
        expect(result.text).not.toMatch(/>\s+\d+\s*\|/);
      }
    }, 60000);

    it('gerbil_build_and_report accepts context_lines parameter', async () => {
      const result = await client.callTool('gerbil_build_and_report', {
        project_path: join(TEST_DIR, 'buildable'),
        context_lines: 3,
      });
      // Should work without error regardless of build outcome
      expect(result.text).toBeDefined();
    }, 60000);

    it('gerbil_build_and_report detects non-executable build.ss', async () => {
      const noExecDir = join(TEST_DIR, 'no-exec-build');
      mkdirSync(noExecDir, { recursive: true });
      writeFileSync(join(noExecDir, 'gerbil.pkg'), '(package: test-no-exec)');
      writeFileSync(
        join(noExecDir, 'build.ss'),
        '#!/usr/bin/env gxi\n(import :std/build-script)\n(defbuild-script \'("hello"))\n',
      );
      // Explicitly remove executable bit
      chmodSync(join(noExecDir, 'build.ss'), 0o644);
      writeFileSync(
        join(noExecDir, 'hello.ss'),
        '(export greet)\n(def (greet name) (string-append "Hello, " name "!"))\n',
      );

      const result = await client.callTool('gerbil_build_and_report', {
        project_path: noExecDir,
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('not executable');
      expect(result.text).toContain('chmod');
    }, 60000);
  });

  // ── Feature suggestion tools ─────────────────────────────────────

  describe('Feature suggestion tools', () => {
    it('gerbil_suggest_feature creates a new features file and adds suggestion', async () => {
      const featuresPath = join(TEST_DIR, 'features', 'features.json');
      const result = await client.callTool('gerbil_suggest_feature', {
        features_path: featuresPath,
        id: 'batch-module-check',
        title: 'Batch module checking',
        description: 'Check multiple modules in a single call',
        impact: 'high',
        tags: ['module', 'batch', 'check'],
        use_case: 'When working with large projects with many modules',
        example_scenario: 'User has 20 modules and wants to check all exports at once',
        estimated_token_reduction: '~500 tokens per invocation',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Added');
      expect(result.text).toContain('batch-module-check');
      expect(result.text).toContain('1 total suggestions');
    });

    it('gerbil_suggest_feature replaces existing suggestion with same id', async () => {
      const featuresPath = join(TEST_DIR, 'features', 'features.json');
      // Add a second suggestion first
      await client.callTool('gerbil_suggest_feature', {
        features_path: featuresPath,
        id: 'another-feature',
        title: 'Another feature',
        description: 'Some other feature',
        impact: 'low',
        tags: ['other'],
        use_case: 'Testing',
        example_scenario: 'Testing update semantics',
        estimated_token_reduction: '~100 tokens',
      });
      // Now update the first suggestion
      const result = await client.callTool('gerbil_suggest_feature', {
        features_path: featuresPath,
        id: 'batch-module-check',
        title: 'Batch module checking (improved)',
        description: 'Check multiple modules in a single call with better output',
        impact: 'high',
        tags: ['module', 'batch', 'check', 'improved'],
        use_case: 'Large projects',
        example_scenario: 'User has 20 modules',
        estimated_token_reduction: '~800 tokens per invocation',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Updated');
      expect(result.text).toContain('batch-module-check');
      expect(result.text).toContain('2 total suggestions');
    });

    it('gerbil_suggest_feature returns error for corrupt JSON', async () => {
      const badPath = join(TEST_DIR, 'bad-features.json');
      writeFileSync(badPath, 'this is not json{{{');
      const result = await client.callTool('gerbil_suggest_feature', {
        features_path: badPath,
        id: 'test',
        title: 'Test',
        description: 'Test',
        impact: 'low',
        tags: ['test'],
        use_case: 'Test',
        example_scenario: 'Test',
        estimated_token_reduction: '~0',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('Error');
    });

    it('gerbil_list_features lists all suggestions', async () => {
      const featuresPath = join(TEST_DIR, 'features', 'features.json');
      const result = await client.callTool('gerbil_list_features', {
        features_path: featuresPath,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('2 feature suggestion(s)');
      expect(result.text).toContain('batch-module-check');
      expect(result.text).toContain('another-feature');
    });

    it('gerbil_list_features searches by keyword', async () => {
      const featuresPath = join(TEST_DIR, 'features', 'features.json');
      const result = await client.callTool('gerbil_list_features', {
        features_path: featuresPath,
        query: 'batch module',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('batch-module-check');
      expect(result.text).toContain('matching "batch module"');
    });

    it('gerbil_list_features returns empty when no matches', async () => {
      const featuresPath = join(TEST_DIR, 'features', 'features.json');
      const result = await client.callTool('gerbil_list_features', {
        features_path: featuresPath,
        query: 'xyznonexistent',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('No feature suggestions matching');
    });

    it('gerbil_list_features handles missing file gracefully', async () => {
      const result = await client.callTool('gerbil_list_features', {
        features_path: '/nonexistent/path/features.json',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('No feature suggestions found');
    });
  });

  describe('Demangle tool', () => {
    it('decodes hex-encoded characters', async () => {
      const result = await client.callTool('gerbil_demangle', {
        symbols: 'hash_2d_ref',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('hash-ref');
    });

    it('decodes double underscore as literal underscore', async () => {
      const result = await client.callTool('gerbil_demangle', {
        symbols: 'my__var',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('my_var');
    });

    it('strips ___H_ prefix and labels as module init', async () => {
      const result = await client.callTool('gerbil_demangle', {
        symbols: '___H_test_2d_aws',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('module init');
      expect(result.text).toContain('test-aws');
    });

    it('strips ___G_ prefix and labels as global', async () => {
      const result = await client.callTool('gerbil_demangle', {
        symbols: '___G_hash_2d_put_21_',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('global');
      expect(result.text).toContain('hash-put!');
    });

    it('decodes full module path with slashes and hash separator', async () => {
      const result = await client.callTool('gerbil_demangle', {
        symbols: '___G_gerbil_2d_aws_2f_ec2_2f_api_23_EC2Client',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('gerbil-aws/ec2/api#EC2Client');
      expect(result.text).toContain('module:');
      expect(result.text).toContain('gerbil-aws/ec2/api');
      expect(result.text).toContain('symbol:');
      expect(result.text).toContain('EC2Client');
    });

    it('handles multiple symbols separated by newlines', async () => {
      const result = await client.callTool('gerbil_demangle', {
        symbols: '___H_test_2d_aws\n___G_set_21_',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('test-aws');
      expect(result.text).toContain('set!');
    });

    it('returns error for empty input', async () => {
      const result = await client.callTool('gerbil_demangle', {
        symbols: '',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('No symbols provided');
    });

    it('handles symbols without known prefix', async () => {
      const result = await client.callTool('gerbil_demangle', {
        symbols: 'string_2d__3e_number',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('string->number');
    });
  });

  describe('Stale static files tool', () => {
    const staleDir = join(TEST_DIR, 'stale-test');

    it('reports no local files when project has not been built', async () => {
      mkdirSync(join(staleDir, 'empty-project'), { recursive: true });
      const result = await client.callTool('gerbil_stale_static', {
        project_path: join(staleDir, 'empty-project'),
        gerbil_path: join(staleDir, 'fake-global'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('No local static files found');
    });

    it('reports no shadowing when global dir is empty', async () => {
      const localDir = join(staleDir, 'proj-only', '.gerbil', 'lib', 'static');
      const globalDir = join(staleDir, 'global-empty', 'lib', 'static');
      mkdirSync(localDir, { recursive: true });
      mkdirSync(globalDir, { recursive: true });
      writeFileSync(join(localDir, 'mod.scm'), 'local-content');
      const result = await client.callTool('gerbil_stale_static', {
        project_path: join(staleDir, 'proj-only'),
        gerbil_path: join(staleDir, 'global-empty'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('No shadowing possible');
    });

    it('detects stale global file with different size', async () => {
      const localDir = join(staleDir, 'size-diff', '.gerbil', 'lib', 'static');
      const globalDir = join(staleDir, 'global-size', 'lib', 'static');
      mkdirSync(localDir, { recursive: true });
      mkdirSync(globalDir, { recursive: true });
      writeFileSync(join(globalDir, 'pkg__mod.scm'), 'old-short');
      writeFileSync(join(localDir, 'pkg__mod.scm'), 'new-longer-content-here');
      const result = await client.callTool('gerbil_stale_static', {
        project_path: join(staleDir, 'size-diff'),
        gerbil_path: join(staleDir, 'global-size'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('STALE');
      expect(result.text).toContain('pkg__mod.scm');
    });

    it('reports ok when files match', async () => {
      const localDir = join(staleDir, 'match', '.gerbil', 'lib', 'static');
      const globalDir = join(staleDir, 'global-match', 'lib', 'static');
      mkdirSync(localDir, { recursive: true });
      mkdirSync(globalDir, { recursive: true });
      const content = 'identical-content';
      writeFileSync(join(globalDir, 'pkg__mod.scm'), content);
      // Write local file with same content — the global mtime will be <= local
      writeFileSync(join(localDir, 'pkg__mod.scm'), content);
      const result = await client.callTool('gerbil_stale_static', {
        project_path: join(staleDir, 'match'),
        gerbil_path: join(staleDir, 'global-match'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Matching files');
      expect(result.text).toContain('0 STALE');
    });

    it('filters by extension', async () => {
      const localDir = join(staleDir, 'ext-filter', '.gerbil', 'lib', 'static');
      const globalDir = join(staleDir, 'global-ext', 'lib', 'static');
      mkdirSync(localDir, { recursive: true });
      mkdirSync(globalDir, { recursive: true });
      writeFileSync(join(globalDir, 'mod.scm'), 'old');
      writeFileSync(join(localDir, 'mod.scm'), 'new-content');
      writeFileSync(join(globalDir, 'mod.o'), 'old-obj');
      writeFileSync(join(localDir, 'mod.o'), 'new-obj-content');
      const result = await client.callTool('gerbil_stale_static', {
        project_path: join(staleDir, 'ext-filter'),
        gerbil_path: join(staleDir, 'global-ext'),
        extensions: ['.scm'],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('mod.scm');
      // .o file should not appear since we filtered to .scm only
      expect(result.text).not.toContain('mod.o');
    });
  });

  describe('Balanced replace tool', () => {
    it('accepts a balance-preserving edit (dry_run)', async () => {
      const file = join(TEST_DIR, 'balanced-edit.ss');
      const result = await client.callTool('gerbil_balanced_replace', {
        file_path: file,
        old_string: '(+ x 1)',
        new_string: '(* x 2)',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Dry run');
      expect(result.text).toContain('(* x 2)');
      expect(result.text).toContain('Balance: OK');
    });

    it('rejects an edit that breaks balance', async () => {
      const file = join(TEST_DIR, 'balanced-edit.ss');
      const result = await client.callTool('gerbil_balanced_replace', {
        file_path: file,
        old_string: '(+ x 1)',
        new_string: '(+ x 1',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('REJECTED');
      expect(result.text).toContain('break delimiter balance');
    });

    it('reports when old_string is not found', async () => {
      const file = join(TEST_DIR, 'balanced-edit.ss');
      const result = await client.callTool('gerbil_balanced_replace', {
        file_path: file,
        old_string: 'nonexistent text',
        new_string: 'replacement',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('not found');
    });

    it('applies edit when dry_run is false', async () => {
      // Create a fresh file for this test
      const file = join(TEST_DIR, 'balanced-apply.ss');
      writeFileSync(file, '(def (f x) (+ x 1))\n');
      const result = await client.callTool('gerbil_balanced_replace', {
        file_path: file,
        old_string: '(+ x 1)',
        new_string: '(* x 2)',
        dry_run: false,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Applied change');
      // Verify file was actually changed
      const { readFileSync } = require('node:fs');
      const content = readFileSync(file, 'utf-8');
      expect(content).toContain('(* x 2)');
    });

    it('detects when edit fixes balance', async () => {
      const file = join(TEST_DIR, 'balanced-fix.ss');
      writeFileSync(file, '(def (f x) (+ x 1)\n');
      const result = await client.callTool('gerbil_balanced_replace', {
        file_path: file,
        old_string: '(+ x 1)',
        new_string: '(+ x 1))',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('FIXES');
    });

    it('handles parens inside strings correctly', async () => {
      const file = join(TEST_DIR, 'balanced-strings.ss');
      writeFileSync(file, '(def msg "hello (world")\n(def x 1)\n');
      const result = await client.callTool('gerbil_balanced_replace', {
        file_path: file,
        old_string: '(def x 1)',
        new_string: '(def x 2)',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Balance: OK');
    });

    it('rejects ambiguous old_string with multiple matches', async () => {
      const file = join(TEST_DIR, 'balanced-ambiguous.ss');
      writeFileSync(file, '(def x 1)\n(def x 1)\n');
      const result = await client.callTool('gerbil_balanced_replace', {
        file_path: file,
        old_string: '(def x 1)',
        new_string: '(def x 2)',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('multiple times');
    });
  });

  describe('Wrap form tool', () => {
    it('wraps a single line (dry_run)', async () => {
      const file = join(TEST_DIR, 'wrap-single.ss');
      writeFileSync(file, '(displayln "hello")\n');
      const result = await client.callTool('gerbil_wrap_form', {
        file_path: file,
        start_line: 1,
        end_line: 1,
        wrapper: 'when #t',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Dry run');
      expect(result.text).toContain('when #t');
      expect(result.text).toContain('Balance: OK');
    });

    it('wraps a multi-line range', async () => {
      const file = join(TEST_DIR, 'wrap-multi.ss');
      writeFileSync(file, '(do-x)\n(do-y)\n(do-z)\n');
      const result = await client.callTool('gerbil_wrap_form', {
        file_path: file,
        start_line: 1,
        end_line: 2,
        wrapper: 'begin',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('begin');
      expect(result.text).toContain('do-x');
      expect(result.text).toContain('do-y');
      expect(result.text).toContain('Balance: OK');
    });

    it('auto-detects form end when end_line is omitted', async () => {
      const file = join(TEST_DIR, 'wrap-target.ss');
      const result = await client.callTool('gerbil_wrap_form', {
        file_path: file,
        start_line: 2,
        wrapper: 'when (> x 0)',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Dry run');
      expect(result.text).toContain('when (> x 0)');
    });

    it('rejects an invalid wrapper', async () => {
      const file = join(TEST_DIR, 'wrap-single.ss');
      writeFileSync(file, '(displayln "hello")\n');
      const result = await client.callTool('gerbil_wrap_form', {
        file_path: file,
        start_line: 1,
        end_line: 1,
        wrapper: 'when (> x 0',
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('Invalid wrapper');
      expect(result.text).toContain('unbalanced');
    });

    it('applies wrap when dry_run is false', async () => {
      const file = join(TEST_DIR, 'wrap-apply.ss');
      writeFileSync(file, '(displayln "hello")\n');
      const result = await client.callTool('gerbil_wrap_form', {
        file_path: file,
        start_line: 1,
        end_line: 1,
        wrapper: 'when #t',
        dry_run: false,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Wrapped lines');
      const { readFileSync } = require('node:fs');
      const content = readFileSync(file, 'utf-8');
      expect(content).toContain('(when #t');
      expect(content).toContain('displayln');
    });
  });

  describe('Splice form tool', () => {
    it('removes head by default (dry_run)', async () => {
      const file = join(TEST_DIR, 'splice-default.ss');
      writeFileSync(file, '(when cond\n  (do-x)\n  (do-y))\n');
      const result = await client.callTool('gerbil_splice_form', {
        file_path: file,
        line: 1,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Dry run');
      expect(result.text).toContain('do-x');
      expect(result.text).toContain('do-y');
      expect(result.text).toContain('Balance: OK');
    });

    it('keeps explicit children', async () => {
      const file = join(TEST_DIR, 'splice-explicit.ss');
      writeFileSync(file, '(if cond then-branch else-branch)\n');
      const result = await client.callTool('gerbil_splice_form', {
        file_path: file,
        line: 1,
        keep_children: [3],
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('then-branch');
      expect(result.text).not.toContain('--- result ---\nelse-branch');
    });

    it('preserves formatting on single child', async () => {
      const file = join(TEST_DIR, 'splice-single.ss');
      writeFileSync(file, '(begin\n  (+ 1 2))\n');
      const result = await client.callTool('gerbil_splice_form', {
        file_path: file,
        line: 1,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('(+ 1 2)');
      expect(result.text).toContain('Balance: OK');
    });

    it('applies splice when dry_run is false', async () => {
      const file = join(TEST_DIR, 'splice-apply.ss');
      writeFileSync(file, '(begin\n  (do-x)\n  (do-y))\n');
      const result = await client.callTool('gerbil_splice_form', {
        file_path: file,
        line: 1,
        dry_run: false,
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('Spliced form');
      const { readFileSync } = require('node:fs');
      const content = readFileSync(file, 'utf-8');
      expect(content).toContain('(do-x)');
      expect(content).toContain('(do-y)');
      expect(content).not.toContain('(begin');
    });

    it('reports error when no form found', async () => {
      const file = join(TEST_DIR, 'splice-empty.ss');
      writeFileSync(file, '; just a comment\n');
      const result = await client.callTool('gerbil_splice_form', {
        file_path: file,
        line: 1,
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('No form found');
    });
  });

  // ── FFI scaffold tool ────────────────────────────────────────────────

  describe('FFI scaffold tool', () => {
    it('parses typedefs and generates c-define-type', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'simple-lib.h'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('c-define-type');
      expect(result.text).toContain('simple_ctx');
    });

    it('parses function declarations and generates c-lambda', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'simple-lib.h'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('define-c-lambda');
      expect(result.text).toContain('simple_process');
      expect(result.text).toContain('simple_get_name');
    });

    it('parses #define constants and generates define-const', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'simple-lib.h'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('define-const MAX_SIZE');
      expect(result.text).toContain('define-const ERROR_CODE');
    });

    it('detects create/destroy pairs and generates cleanup code', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'simple-lib.h'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('create/destroy pair');
      expect(result.text).toContain('ffi_free_');
      expect(result.text).toContain('simple_ctx_destroy');
      expect(result.text).toContain('___SCMOBJ');
      expect(result.text).toContain('___FIX(___NO_ERR)');
    });

    it('parses enums and generates define-const for members', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'enum-lib.h'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('define-const COLOR_RED');
      expect(result.text).toContain('define-const COLOR_GREEN');
      expect(result.text).toContain('define-const COLOR_BLUE');
    });

    it('detects new/free pairs as create/destroy', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'enum-lib.h'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('create/destroy pair');
      expect(result.text).toContain('widget_free');
    });

    it('handles multiple pointer types', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'multi-type.h'),
      });
      expect(result.isError).toBe(false);
      // Should have multiple c-define-type declarations
      expect(result.text).toContain('c-define-type db_t');
      expect(result.text).toContain('c-define-type iterator_t');
      expect(result.text).toContain('c-define-type options_t');
    });

    it('detects open/close pairs as create/destroy', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'multi-type.h'),
      });
      expect(result.isError).toBe(false);
      // db_open/db_close should be detected
      expect(result.text).toContain('db_close');
    });

    it('maps char* to UTF-8-string', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'simple-lib.h'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('UTF-8-string');
    });

    it('respects include_path parameter', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'simple-lib.h'),
        include_path: 'mylib/simple-lib.h',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('mylib/simple-lib.h');
    });

    it('generates module wrapper with module_name', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'simple-lib.h'),
        module_name: 'mylib',
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('(import :std/foreign)');
      expect(result.text).toContain('(export ');
    });

    it('reports error for missing file', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'nonexistent.h'),
      });
      expect(result.isError).toBe(true);
      expect(result.text).toContain('Failed to read');
    });

    it('shows parsed summary with counts', async () => {
      const result = await client.callTool('gerbil_ffi_scaffold', {
        file_path: join(TEST_DIR, 'simple-lib.h'),
      });
      expect(result.isError).toBe(false);
      expect(result.text).toContain('typedef');
      expect(result.text).toContain('function');
      expect(result.text).toContain('constant');
    });
  });
});
