import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { mkdirSync, writeFileSync, existsSync } from 'node:fs';
import { join } from 'node:path';

interface TemplateFile {
  path: string;
  content: string;
}

interface ProjectTemplate {
  name: string;
  description: string;
  files: (projectName: string, packageName: string) => TemplateFile[];
}

const TEMPLATES: Record<string, ProjectTemplate> = {
  'cli': {
    name: 'CLI Tool with Subcommands',
    description: 'Command-line application with getopt argument parsing and subcommands',
    files: (proj, pkg) => [
      {
        path: 'gerbil.pkg',
        content: `(package: ${pkg})\n`,
      },
      {
        path: 'build.ss',
        content: `(import :std/build-script)\n\n(defbuild-script\n  '("main"\n    "cli"))\n`,
      },
      {
        path: 'main.ss',
        content: `(import :std/getopt\n        :std/format\n        (pkg: cli))\n(export main)\n\n(def (main . args)\n  (call-with-getopt ${pkg}-main args\n    program: "${proj}"\n    help: "${proj} — a CLI tool"\n    (command 'run help: "Run the main command"\n      (optional-argument 'input help: "Input file"))\n    (command 'version help: "Show version"\n      )))\n\n(def (${pkg}-main opt)\n  (let ((cmd (hash-ref opt 'command)))\n    (case cmd\n      ((run) (run-command opt))\n      ((version) (displayln "${proj} v0.1.0")))))\n\n(def (run-command opt)\n  (let ((input (hash-ref opt 'input #f)))\n    (if input\n      (displayln "Processing: " input)\n      (displayln "No input specified"))))\n`,
      },
      {
        path: 'cli.ss',
        content: `(export #t)\n\n;; CLI utilities for ${proj}\n\n(def (print-error msg)\n  (parameterize ((current-output-port (current-error-port)))\n    (displayln "error: " msg)))\n\n(def (exit-with-error msg (code 1))\n  (print-error msg)\n  (exit code))\n`,
      },
      {
        path: 'Makefile',
        content: `.PHONY: build clean test\n\nbuild:\n\tgerbil build\n\nclean:\n\tgerbil clean\n\ntest:\n\tgerbil test ./...\n`,
      },
    ],
  },
  'http-api': {
    name: 'HTTP API Server',
    description: 'REST API server with JSON endpoints using :std/net/httpd',
    files: (proj, pkg) => [
      {
        path: 'gerbil.pkg',
        content: `(package: ${pkg})\n`,
      },
      {
        path: 'build.ss',
        content: `(import :std/build-script)\n\n(defbuild-script\n  '("main"\n    "handlers"\n    "routes"))\n`,
      },
      {
        path: 'main.ss',
        content: `(import :std/net/httpd\n        :std/getopt\n        (pkg: routes))\n(export main)\n\n(def (main . args)\n  (def port\n    (let ((p (if (pair? args) (string->number (car args)) #f)))\n      (or p 8080)))\n  (def httpd (start-http-server! (format "0.0.0.0:~a" port)))\n  (register-routes! httpd)\n  (displayln "Server running on port " port)\n  (thread-join! httpd))\n`,
      },
      {
        path: 'routes.ss',
        content: `(import :std/net/httpd\n        (pkg: handlers))\n(export register-routes!)\n\n(def (register-routes! httpd)\n  (http-register-handler httpd "/api/health" health-handler)\n  (http-register-handler httpd "/api/echo" echo-handler))\n`,
      },
      {
        path: 'handlers.ss',
        content: `(import :std/net/httpd\n        :std/text/json)\n(export health-handler echo-handler)\n\n(def (json-response req obj)\n  (http-response-write req 200 '(("Content-Type" . "application/json"))\n    (json-object->string obj)))\n\n(def (health-handler req res)\n  (json-response req (hash ("status" "ok"))))\n\n(def (echo-handler req res)\n  (let ((body (http-request-body req)))\n    (json-response req (hash ("echo" (or (and body (bytes->string body)) ""))))))\n`,
      },
      {
        path: 'Makefile',
        content: `.PHONY: build clean run test\n\nbuild:\n\tgerbil build\n\nclean:\n\tgerbil clean\n\nrun: build\n\tgerbil ${pkg}/main\n\ntest:\n\tgerbil test ./...\n`,
      },
    ],
  },
  'library': {
    name: 'Library Package',
    description: 'Reusable library with public API, internal modules, and tests',
    files: (proj, pkg) => [
      {
        path: 'gerbil.pkg',
        content: `(package: ${pkg})\n`,
      },
      {
        path: 'build.ss',
        content: `(import :std/build-script)\n\n(defbuild-script\n  '("interface"\n    "impl"\n    "util"\n    "main-test"))\n`,
      },
      {
        path: 'interface.ss',
        content: `(export #t)\n\n;; Public API for ${proj}\n;; Import this module in consumer code: (import :${pkg})\n\n(def (hello name)\n  (string-append "Hello, " name "!"))\n\n(def (process data)\n  ;; TODO: implement\n  data)\n`,
      },
      {
        path: 'impl.ss',
        content: `(export #t)\n\n;; Internal implementation details\n;; Not part of the public API\n\n(def (internal-helper x)\n  (* x 2))\n`,
      },
      {
        path: 'util.ss',
        content: `(export #t)\n\n;; Shared utilities\n\n(def (ensure-string x)\n  (if (string? x) x (object->string x)))\n`,
      },
      {
        path: 'main-test.ss',
        content: `(import :std/test\n        (pkg: interface))\n(export main-test)\n\n(def main-test\n  (test-suite "${proj} tests"\n    (test-case "hello"\n      (check (hello "World") => "Hello, World!"))))\n\n(run-tests! main-test)\n(test-report-summary!)\n`,
      },
      {
        path: 'Makefile',
        content: `.PHONY: build clean test\n\nbuild:\n\tgerbil build\n\nclean:\n\tgerbil clean\n\ntest:\n\tgerbil test ./...\n`,
      },
    ],
  },
  'actor-service': {
    name: 'Actor-Based Service',
    description: 'Service using the actor model with supervision and message passing',
    files: (proj, pkg) => [
      {
        path: 'gerbil.pkg',
        content: `(package: ${pkg})\n`,
      },
      {
        path: 'build.ss',
        content: `(import :std/build-script)\n\n(defbuild-script\n  '("main"\n    "worker"\n    "supervisor"))\n`,
      },
      {
        path: 'main.ss',
        content: `(import :std/actor\n        :std/sugar\n        (pkg: supervisor))\n(export main)\n\n(def (main . args)\n  (start-actor-server!)\n  (def sup (start-supervisor!))\n  (displayln "Actor system started")\n  (thread-join! (spawn (lambda () (thread-sleep! +inf.0)))))\n`,
      },
      {
        path: 'worker.ss',
        content: `(import :std/actor\n        :std/sugar\n        :std/logger)\n(export start-worker!)\n\n(def (start-worker! id)\n  (spawn/name (format "worker-~a" id)\n    (lambda ()\n      (let loop ()\n        (<- ((!message data)\n             (debugf "worker ~a got: ~a" id data)\n             (loop))\n            ((!shutdown)\n             (debugf "worker ~a shutting down" id)))))))\n`,
      },
      {
        path: 'supervisor.ss',
        content: `(import :std/actor\n        :std/sugar\n        (pkg: worker))\n(export start-supervisor!)\n\n(def (start-supervisor! (n 4))\n  (def workers\n    (for/collect (i (in-range n))\n      (start-worker! i)))\n  (spawn/name 'supervisor\n    (lambda ()\n      (let loop ()\n        (<- ((!shutdown)\n             (for-each (lambda (w) (!! w (!shutdown))) workers)\n             (displayln "Supervisor: all workers stopped"))\n            (timeout: 60\n             (loop)))))))\n`,
      },
      {
        path: 'Makefile',
        content: `.PHONY: build clean run test\n\nbuild:\n\tgerbil build\n\nclean:\n\tgerbil clean\n\nrun: build\n\tgerbil ${pkg}/main\n\ntest:\n\tgerbil test ./...\n`,
      },
    ],
  },
  'db-crud': {
    name: 'Database CRUD Application',
    description: 'SQLite-backed application with connection pooling and CRUD operations',
    files: (proj, pkg) => [
      {
        path: 'gerbil.pkg',
        content: `(package: ${pkg})\n`,
      },
      {
        path: 'build.ss',
        content: `(import :std/build-script)\n\n(defbuild-script\n  '("main"\n    "db"\n    "model"))\n`,
      },
      {
        path: 'main.ss',
        content: `(import :std/getopt\n        (pkg: db)\n        (pkg: model))\n(export main)\n\n(def (main . args)\n  (init-db! "app.db")\n  (create-table!)\n  (insert-item! "First item")\n  (insert-item! "Second item")\n  (displayln "All items:")\n  (for-each (lambda (item) (displayln "  " item))\n    (list-items))\n  (close-db!))\n`,
      },
      {
        path: 'db.ss',
        content: `(import :std/db/sqlite)\n(export init-db! close-db! with-db)\n\n(def *db* #f)\n\n(def (init-db! path)\n  (set! *db* (sqlite-open path)))\n\n(def (close-db!)\n  (when *db*\n    (sqlite-close *db*)\n    (set! *db* #f)))\n\n(def (with-db fn)\n  (unless *db* (error "Database not initialized"))\n  (fn *db*))\n`,
      },
      {
        path: 'model.ss',
        content: `(import :std/db/sqlite\n        (pkg: db))\n(export create-table! insert-item! list-items get-item delete-item!)\n\n(def (create-table!)\n  (with-db (lambda (db)\n    (sqlite-exec db\n      "CREATE TABLE IF NOT EXISTS items (id INTEGER PRIMARY KEY, name TEXT, created_at TEXT DEFAULT CURRENT_TIMESTAMP)"))))\n\n(def (insert-item! name)\n  (with-db (lambda (db)\n    (sqlite-exec db "INSERT INTO items (name) VALUES (?)" name))))\n\n(def (list-items)\n  (with-db (lambda (db)\n    (let ((stmt (sqlite-prepare db "SELECT id, name, created_at FROM items ORDER BY id")))\n      (let loop ((rows '()))\n        (if (sqlite-step stmt)\n          (loop (cons (list (sqlite-column stmt 0)\n                            (sqlite-column stmt 1)\n                            (sqlite-column stmt 2))\n                      rows))\n          (begin (sqlite-finalize stmt)\n                 (reverse rows))))))))\n\n(def (get-item id)\n  (with-db (lambda (db)\n    (let ((stmt (sqlite-prepare db "SELECT id, name, created_at FROM items WHERE id = ?")))\n      (sqlite-bind stmt 1 id)\n      (if (sqlite-step stmt)\n        (let ((result (list (sqlite-column stmt 0)\n                           (sqlite-column stmt 1)\n                           (sqlite-column stmt 2))))\n          (sqlite-finalize stmt)\n          result)\n        (begin (sqlite-finalize stmt) #f))))))\n\n(def (delete-item! id)\n  (with-db (lambda (db)\n    (sqlite-exec db "DELETE FROM items WHERE id = ?" id))))\n`,
      },
      {
        path: 'Makefile',
        content: `.PHONY: build clean run test\n\nbuild:\n\tgerbil build\n\nclean:\n\tgerbil clean\n\nrun: build\n\tgerbil ${pkg}/main\n\ntest:\n\tgerbil test ./...\n`,
      },
    ],
  },
  'parser': {
    name: 'Parser/Compiler',
    description: 'Language parser with lexer, grammar, and AST using :std/parser',
    files: (proj, pkg) => [
      {
        path: 'gerbil.pkg',
        content: `(package: ${pkg})\n`,
      },
      {
        path: 'build.ss',
        content: `(import :std/build-script)\n\n(defbuild-script\n  '("main"\n    "lexer"\n    "parser"\n    "ast"))\n`,
      },
      {
        path: 'main.ss',
        content: `(import (pkg: lexer)\n        (pkg: parser)\n        (pkg: ast))\n(export main)\n\n(def (main . args)\n  (let* ((input (if (pair? args) (car args) "1 + 2 * 3"))\n         (tokens (tokenize input))\n         (ast (parse-expr tokens)))\n    (displayln "Input: " input)\n    (displayln "AST: " ast)))\n`,
      },
      {
        path: 'lexer.ss',
        content: `(export tokenize token-type token-value)\n\n(defstruct token (type value) transparent: #t)\n\n(def (tokenize input)\n  (let loop ((chars (string->list input)) (tokens '()))\n    (cond\n      ((null? chars) (reverse tokens))\n      ((char-whitespace? (car chars))\n       (loop (cdr chars) tokens))\n      ((char-numeric? (car chars))\n       (let num-loop ((cs (cdr chars)) (digits (list (car chars))))\n         (if (and (pair? cs) (char-numeric? (car cs)))\n           (num-loop (cdr cs) (cons (car cs) digits))\n           (loop cs (cons (make-token 'number (string->number (list->string (reverse digits))))\n                         tokens)))))\n      ((memv (car chars) '(#\\+ #\\- #\\* #\\/))\n       (loop (cdr chars) (cons (make-token 'op (car chars)) tokens)))\n      ((eqv? (car chars) #\\()\n       (loop (cdr chars) (cons (make-token 'lparen #f) tokens)))\n      ((eqv? (car chars) #\\))\n       (loop (cdr chars) (cons (make-token 'rparen #f) tokens)))\n      (else (error "Unexpected character" (car chars))))))\n`,
      },
      {
        path: 'parser.ss',
        content: `(import (pkg: lexer) (pkg: ast))\n(export parse-expr)\n\n;; Simple recursive-descent expression parser\n;; Handles: number, +, -, *, / with precedence\n\n(def (parse-expr tokens)\n  (let-values (((expr rest) (parse-additive tokens)))\n    expr))\n\n(def (parse-additive tokens)\n  (let-values (((left rest) (parse-multiplicative tokens)))\n    (let loop ((left left) (rest rest))\n      (if (and (pair? rest)\n               (eq? (token-type (car rest)) 'op)\n               (memv (token-value (car rest)) '(#\\+ #\\-)))\n        (let ((op (token-value (car rest))))\n          (let-values (((right rest2) (parse-multiplicative (cdr rest))))\n            (loop (make-binop op left right) rest2)))\n        (values left rest)))))\n\n(def (parse-multiplicative tokens)\n  (let-values (((left rest) (parse-primary tokens)))\n    (let loop ((left left) (rest rest))\n      (if (and (pair? rest)\n               (eq? (token-type (car rest)) 'op)\n               (memv (token-value (car rest)) '(#\\* #\\/)))\n        (let ((op (token-value (car rest))))\n          (let-values (((right rest2) (parse-primary (cdr rest))))\n            (loop (make-binop op left right) rest2)))\n        (values left rest)))))\n\n(def (parse-primary tokens)\n  (cond\n    ((null? tokens) (error "Unexpected end of input"))\n    ((eq? (token-type (car tokens)) 'number)\n     (values (make-literal (token-value (car tokens))) (cdr tokens)))\n    ((eq? (token-type (car tokens)) 'lparen)\n     (let-values (((expr rest) (parse-additive (cdr tokens))))\n       (if (and (pair? rest) (eq? (token-type (car rest)) 'rparen))\n         (values expr (cdr rest))\n         (error "Expected closing parenthesis"))))\n    (else (error "Unexpected token" (car tokens)))))\n`,
      },
      {
        path: 'ast.ss',
        content: `(export #t)\n\n;; AST node types\n\n(defstruct literal (value) transparent: #t)\n(defstruct binop (op left right) transparent: #t)\n(defstruct unaryop (op operand) transparent: #t)\n`,
      },
      {
        path: 'Makefile',
        content: `.PHONY: build clean run test\n\nbuild:\n\tgerbil build\n\nclean:\n\tgerbil clean\n\nrun: build\n\tgerbil ${pkg}/main "1 + 2 * 3"\n\ntest:\n\tgerbil test ./...\n`,
      },
    ],
  },
  'ffi-wrapper': {
    name: 'FFI Wrapper Library',
    description: 'C foreign function interface wrapper with build system integration',
    files: (proj, pkg) => [
      {
        path: 'gerbil.pkg',
        content: `(package: ${pkg})\n`,
      },
      {
        path: 'build.ss',
        content: `(import :std/build-script)\n\n(defbuild-script\n  '("ffi"\n    "wrapper"\n    "main"))\n`,
      },
      {
        path: 'ffi.ss',
        content: `(export #t)\n\n;; Low-level FFI declarations\n(begin-foreign\n  (namespace ("${pkg}/ffi#" ffi/))\n  (c-declare "#include <string.h>")\n\n  ;; Example: wrap strlen\n  (define ffi/c-strlen\n    (c-lambda (UTF-8-string) int "strlen"))\n)\n`,
      },
      {
        path: 'wrapper.ss',
        content: `(import (pkg: ffi))\n(export string-byte-length)\n\n;; Safe Gerbil wrappers around FFI functions\n\n(def (string-byte-length s)\n  (unless (string? s)\n    (error "string-byte-length: expected string" s))\n  (ffi/c-strlen s))\n`,
      },
      {
        path: 'main.ss',
        content: `(import (pkg: wrapper))\n(export main)\n\n(def (main . args)\n  (let ((s (if (pair? args) (car args) "Hello, Gerbil!")))\n    (displayln "String: " s)\n    (displayln "Byte length: " (string-byte-length s))))\n`,
      },
      {
        path: 'Makefile',
        content: `.PHONY: build clean run test\n\nbuild:\n\tgerbil build\n\nclean:\n\tgerbil clean\n\nrun: build\n\tgerbil ${pkg}/main\n\ntest:\n\tgerbil test ./...\n`,
      },
    ],
  },
  'test-project': {
    name: 'Test-Heavy Project',
    description: 'Project structure optimized for testing with fixtures and mocks',
    files: (proj, pkg) => [
      {
        path: 'gerbil.pkg',
        content: `(package: ${pkg})\n`,
      },
      {
        path: 'build.ss',
        content: `(import :std/build-script)\n\n(defbuild-script\n  '("core"\n    "util"\n    "core-test"\n    "util-test"))\n`,
      },
      {
        path: 'core.ss',
        content: `(export #t)\n\n;; Core module\n\n(def (process-data items)\n  (filter (lambda (x) (> x 0)) items))\n\n(def (transform-item x)\n  (* x 2))\n\n(def (batch-transform items)\n  (map transform-item (process-data items)))\n`,
      },
      {
        path: 'util.ss',
        content: `(export #t)\n\n;; Utility functions\n\n(def (safe-divide a b)\n  (if (zero? b) #f (/ a b)))\n\n(def (clamp val lo hi)\n  (max lo (min hi val)))\n`,
      },
      {
        path: 'core-test.ss',
        content: `(import :std/test\n        (pkg: core))\n(export core-test)\n\n(def core-test\n  (test-suite "core tests"\n    (test-case "process-data filters negatives"\n      (check (process-data '(1 -2 3 -4 5)) => '(1 3 5)))\n    (test-case "process-data empty list"\n      (check (process-data '()) => '()))\n    (test-case "transform-item doubles"\n      (check (transform-item 5) => 10))\n    (test-case "batch-transform end-to-end"\n      (check (batch-transform '(1 -2 3)) => '(2 6)))))\n\n(run-tests! core-test)\n(test-report-summary!)\n`,
      },
      {
        path: 'util-test.ss',
        content: `(import :std/test\n        (pkg: util))\n(export util-test)\n\n(def util-test\n  (test-suite "util tests"\n    (test-case "safe-divide normal"\n      (check (safe-divide 10 2) => 5))\n    (test-case "safe-divide by zero"\n      (check (safe-divide 10 0) => #f))\n    (test-case "clamp within range"\n      (check (clamp 5 0 10) => 5))\n    (test-case "clamp below"\n      (check (clamp -5 0 10) => 0))\n    (test-case "clamp above"\n      (check (clamp 15 0 10) => 10))))\n\n(run-tests! util-test)\n(test-report-summary!)\n`,
      },
      {
        path: 'Makefile',
        content: `.PHONY: build clean test\n\nbuild:\n\tgerbil build\n\nclean:\n\tgerbil clean\n\ntest:\n\tgerbil test ./...\n`,
      },
    ],
  },
};

export function registerProjectTemplateTool(server: McpServer): void {
  server.registerTool(
    'gerbil_project_template',
    {
      title: 'Generate Project from Template',
      description:
        'Generate a complete multi-file Gerbil project from a template. ' +
        'Available templates: cli (CLI with subcommands), http-api (REST server), ' +
        'library (reusable lib with tests), actor-service (actor supervision), ' +
        'db-crud (SQLite CRUD), parser (lexer+grammar+AST), ' +
        'ffi-wrapper (C FFI bindings), test-project (test-heavy structure). ' +
        'Each template includes gerbil.pkg, build.ss, Makefile, source modules, and tests.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        template: z
          .enum(['cli', 'http-api', 'library', 'actor-service', 'db-crud', 'parser', 'ffi-wrapper', 'test-project'])
          .describe('Template type to generate'),
        project_name: z
          .string()
          .describe('Project name (e.g. "my-app"). Used for directory name and display.'),
        output_dir: z
          .string()
          .describe('Parent directory where the project directory will be created'),
        package_name: z
          .string()
          .optional()
          .describe('Gerbil package name (default: derived from project_name)'),
        list_templates: z
          .boolean()
          .optional()
          .describe('If true, list available templates instead of generating'),
      },
    },
    async ({ template, project_name, output_dir, package_name, list_templates }) => {
      // List mode
      if (list_templates) {
        const sections: string[] = ['Available project templates:\n'];
        for (const [id, tmpl] of Object.entries(TEMPLATES)) {
          sections.push(`  **${id}** — ${tmpl.name}`);
          sections.push(`    ${tmpl.description}\n`);
        }
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      const tmpl = TEMPLATES[template];
      if (!tmpl) {
        return {
          content: [{
            type: 'text' as const,
            text: `Unknown template: ${template}. Available: ${Object.keys(TEMPLATES).join(', ')}`,
          }],
          isError: true,
        };
      }

      const pkg = package_name || project_name.replace(/-/g, '_');
      const projectDir = join(output_dir, project_name);

      if (existsSync(projectDir)) {
        return {
          content: [{
            type: 'text' as const,
            text: `Directory already exists: ${projectDir}`,
          }],
          isError: true,
        };
      }

      // Create project directory and files
      try {
        mkdirSync(projectDir, { recursive: true });
        const files = tmpl.files(project_name, pkg);

        for (const file of files) {
          const filePath = join(projectDir, file.path);
          const dir = join(filePath, '..');
          mkdirSync(dir, { recursive: true });
          writeFileSync(filePath, file.content, 'utf-8');
        }

        const sections: string[] = [
          `Created ${tmpl.name} project: ${projectDir}\n`,
          'Files:',
        ];
        for (const file of files) {
          sections.push(`  ${file.path}`);
        }
        sections.push('');
        sections.push('Next steps:');
        sections.push(`  cd ${projectDir}`);
        sections.push('  make build');
        sections.push('  make test');

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      } catch (err) {
        return {
          content: [{
            type: 'text' as const,
            text: `Failed to create project: ${err instanceof Error ? err.message : String(err)}`,
          }],
          isError: true,
        };
      }
    },
  );
}
