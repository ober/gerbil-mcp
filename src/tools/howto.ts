import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { z } from 'zod';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/** Path to the repo-local cookbook that accumulates recipes across sessions. */
export const REPO_COOKBOOK_PATH = resolve(__dirname, '..', '..', 'cookbooks.json');

export function loadCookbook(path: string): Recipe[] {
  try {
    const raw = readFileSync(path, 'utf-8');
    const parsed = JSON.parse(raw);
    if (Array.isArray(parsed)) return parsed;
  } catch {
    // Missing or invalid file — skip silently
  }
  return [];
}

export interface Recipe {
  id: string;
  title: string;
  tags: string[];
  imports: string[];
  code: string;
  notes?: string;
  related?: string[];
}

export const RECIPES: Recipe[] = [
  // ── File I/O ──────────────────────────────────────────────────────
  {
    id: 'read-file-string',
    title: 'Read file to string',
    tags: ['file', 'read', 'string', 'io', 'input'],
    imports: [],
    code: `(call-with-input-file "path.txt" read-line)
;; or read entire file:
(call-with-input-file "path.txt"
  (lambda (port)
    (let loop ((lines '()) (line (read-line port)))
      (if (eof-object? line)
        (string-join (reverse lines) "\\n")
        (loop (cons line lines) (read-line port))))))`,
    notes: 'For large files, consider buffered I/O with :std/io.',
  },
  {
    id: 'read-file-lines',
    title: 'Read file as list of lines',
    tags: ['file', 'read', 'lines', 'io', 'input', 'list'],
    imports: [':std/misc/ports'],
    code: `(import :std/misc/ports)
(def lines (call-with-input-file "path.txt" read-all-as-lines))`,
    notes: 'read-all-as-lines returns a list of strings without newlines.',
  },
  {
    id: 'write-file-string',
    title: 'Write string to file',
    tags: ['file', 'write', 'string', 'io', 'output', 'save'],
    imports: [],
    code: `(call-with-output-file "output.txt"
  (lambda (port)
    (display "hello world" port)
    (newline port)))`,
  },
  {
    id: 'read-file-bytes',
    title: 'Read file as bytes (u8vector)',
    tags: ['file', 'read', 'bytes', 'binary', 'u8vector'],
    imports: [':std/misc/ports'],
    code: `(import :std/misc/ports)
(def data (call-with-input-file "file.bin" read-all-as-u8vector))`,
  },
  {
    id: 'write-file-bytes',
    title: 'Write bytes to file',
    tags: ['file', 'write', 'bytes', 'binary', 'u8vector'],
    imports: [],
    code: `(call-with-output-file "file.bin"
  (lambda (port)
    (write-u8vector #u8(0 1 2 255) port)))`,
  },

  // ── JSON ──────────────────────────────────────────────────────────
  {
    id: 'json-parse',
    title: 'Parse JSON string',
    tags: ['json', 'parse', 'read', 'string', 'deserialize'],
    imports: [':std/text/json'],
    code: `(import :std/text/json)
(def data (call-with-input-string "{\\"name\\":\\"alice\\",\\"age\\":30}" read-json))
;; data is a hash-table: (hash-ref data "name") => "alice"`,
    notes: 'read-json returns hash-tables for objects, lists for arrays.',
    related: ['json-generate'],
  },
  {
    id: 'json-generate',
    title: 'Generate JSON string',
    tags: ['json', 'write', 'generate', 'string', 'serialize', 'output'],
    imports: [':std/text/json'],
    code: `(import :std/text/json)
(def json-str
  (call-with-output-string
    (lambda (p) (write-json (hash ("name" "alice") ("age" 30)) p))))
;; or use json-object->string:
(json-object->string (hash ("key" "value")))`,
    related: ['json-parse'],
  },
  {
    id: 'json-file',
    title: 'Read/write JSON files',
    tags: ['json', 'file', 'read', 'write', 'io'],
    imports: [':std/text/json'],
    code: `(import :std/text/json)
;; Read:
(def config (call-with-input-file "config.json" read-json))
;; Write:
(call-with-output-file "output.json"
  (lambda (p) (write-json config p)))`,
    related: ['json-parse', 'json-generate'],
  },

  // ── HTTP ──────────────────────────────────────────────────────────
  {
    id: 'http-get',
    title: 'HTTP GET request',
    tags: ['http', 'get', 'request', 'web', 'api', 'fetch', 'network'],
    imports: [':std/net/request'],
    code: `(import :std/net/request)
(def resp (http-get "https://api.example.com/data"
            headers: '(("Authorization" . "Bearer token"))))
(request-status resp)   ;; HTTP status code
(request-text resp)     ;; response body as string
(request-json resp)     ;; parse as JSON`,
    related: ['http-post-json'],
  },
  {
    id: 'http-post-json',
    title: 'HTTP POST with JSON body',
    tags: ['http', 'post', 'json', 'request', 'web', 'api', 'network'],
    imports: [':std/net/request', ':std/text/json'],
    code: `(import :std/net/request :std/text/json)
(def body (json-object->string (hash ("name" "alice"))))
(def resp (http-post "https://api.example.com/users"
            data: body
            headers: '(("Content-Type" . "application/json"))))
(request-status resp)`,
    related: ['http-get'],
  },

  // ── Hash Tables ───────────────────────────────────────────────────
  {
    id: 'hash-table-basics',
    title: 'Hash table operations',
    tags: ['hash', 'table', 'map', 'dictionary', 'lookup', 'create'],
    imports: [],
    code: `(def ht (hash ("name" "alice") ("age" 30)))   ;; literal syntax
(hash-ref ht "name")          ;; => "alice" (error if missing)
(hash-get ht "missing")       ;; => #f (safe lookup)
(hash-get ht "missing" 42)    ;; => 42 (custom default)
(hash-put! ht "email" "a@b")  ;; mutate
(hash-key? ht "name")         ;; => #t
(hash-remove! ht "age")
(hash->list ht)               ;; => (("name" . "alice") ...)`,
    notes: 'Use (hash ...) for string keys, (hash-eq ...) for symbol keys.',
    related: ['iterate-hash', 'hash-table-merge'],
  },
  {
    id: 'hash-table-merge',
    title: 'Merge hash tables',
    tags: ['hash', 'merge', 'combine', 'table'],
    imports: [],
    code: `(def defaults (hash ("port" 80) ("host" "localhost")))
(def overrides (hash ("port" 8080)))
(def config (hash-merge defaults overrides))
;; config has port=8080, host="localhost"`,
    related: ['hash-table-basics'],
  },
  {
    id: 'iterate-hash',
    title: 'Iterate over hash table',
    tags: ['hash', 'iterate', 'loop', 'for', 'keys', 'values'],
    imports: [':std/iter'],
    code: `(import :std/iter)
(def ht (hash ("a" 1) ("b" 2) ("c" 3)))
;; Key-value pairs:
(for ((k v) (in-hash ht)) (displayln k " => " v))
;; Keys only:
(for (k (in-hash-keys ht)) (displayln k))
;; Values only:
(for (v (in-hash-values ht)) (displayln v))`,
    related: ['hash-table-basics', 'for-collect'],
  },

  // ── Strings ───────────────────────────────────────────────────────
  {
    id: 'string-split-join',
    title: 'Split and join strings',
    tags: ['string', 'split', 'join', 'substring', 'text'],
    imports: [':std/misc/string', ':std/srfi/13'],
    code: `(import :std/misc/string :std/srfi/13)
;; Split:
(string-split "a,b,c" #\\,)        ;; => ("a" "b" "c")
;; Join:
(string-join '("a" "b" "c") ",")  ;; => "a,b,c"
;; Other:
(string-prefix? "hello" "hello world")   ;; => #t
(string-contains "hello world" "world")  ;; => 6 (index)`,
    related: ['string-regex'],
  },
  {
    id: 'string-regex',
    title: 'Regular expressions',
    tags: ['regex', 'regexp', 'pattern', 'match', 'string', 'search', 'replace'],
    imports: [':std/pregexp'],
    code: `(import :std/pregexp)
(pregexp-match "([a-z]+)@([a-z.]+)" "user@example.com")
;; => ("user@example.com" "user" "example.com")
(pregexp-replace "world" "hello world" "gerbil")
;; => "hello gerbil"
(pregexp-split "\\\\s+" "one  two   three")
;; => ("one" "two" "three")`,
  },

  // ── Iteration ─────────────────────────────────────────────────────
  {
    id: 'for-collect',
    title: 'Collect results with for/collect',
    tags: ['for', 'collect', 'map', 'list', 'iterate', 'transform', 'loop'],
    imports: [':std/iter'],
    code: `(import :std/iter)
(for/collect (x '(1 2 3 4 5))
  (* x x))
;; => (1 4 9 16 25)

;; With filter:
(for/collect (x (in-range 1 20) when (zero? (modulo x 3)))
  x)
;; => (3 6 9 12 15 18)

;; With index:
(for/collect ((x i) (in-indexed '(a b c)))
  (list i x))
;; => ((0 a) (1 b) (2 c))`,
    related: ['for-fold'],
  },
  {
    id: 'for-fold',
    title: 'Reduce with for/fold',
    tags: ['for', 'fold', 'reduce', 'accumulate', 'sum', 'iterate', 'loop'],
    imports: [':std/iter'],
    code: `(import :std/iter)
;; Sum a list:
(for/fold (acc 0) (x '(1 2 3 4 5))
  (+ acc x))
;; => 15

;; Build a hash-table from pairs:
(for/fold (ht (make-hash-table)) ((k v) (in-hash (hash ("a" 1) ("b" 2))))
  (hash-put! ht k (* v 10))
  ht)`,
    related: ['for-collect'],
  },

  // ── Error Handling ────────────────────────────────────────────────
  {
    id: 'error-handling',
    title: 'Try/catch error handling',
    tags: ['error', 'exception', 'try', 'catch', 'finally', 'handle'],
    imports: [],
    code: `(try
  (risky-operation)
  (catch (io-error? e)
    (displayln "I/O error: " (error-message e)))
  (catch (e)
    (displayln "Unknown error: " e))
  (finally
    (cleanup-resources)))

;; Simple: ignore errors and return #f
(ignore-errors (/ 1 0))  ;; => #f

;; Functional form:
(with-catch
  (lambda (e) (displayln "caught: " e) #f)
  (lambda () (risky-operation)))`,
    related: ['custom-error'],
  },
  {
    id: 'custom-error',
    title: 'Define custom error types',
    tags: ['error', 'custom', 'exception', 'define', 'struct', 'type'],
    imports: [':std/error'],
    code: `(import :std/error)
(defstruct (my-error Error) (detail)
  transparent: #t)

(def (validate x)
  (unless (number? x)
    (raise (make-my-error "validation failed"
             where: 'validate
             irritants: [x]
             detail: "expected number"))))

(try (validate "oops")
  (catch (my-error? e)
    (displayln "detail: " (my-error-detail e))))`,
    related: ['error-handling'],
  },

  // ── Concurrency ───────────────────────────────────────────────────
  {
    id: 'spawn-thread',
    title: 'Spawn threads',
    tags: ['thread', 'spawn', 'concurrent', 'parallel', 'async'],
    imports: [],
    code: `(def t (spawn (lambda () (thread-sleep! 0.1) 42)))
(thread-join! t)  ;; => 42

;; Named thread:
(def worker (spawn/name 'my-worker
  (lambda ()
    (let loop ((i 0))
      (when (< i 10)
        (displayln "working " i)
        (loop (+ i 1)))))))
(thread-join! worker)`,
    related: ['channel-pattern'],
  },
  {
    id: 'channel-pattern',
    title: 'Channel-based communication',
    tags: ['channel', 'thread', 'concurrent', 'message', 'producer', 'consumer', 'async'],
    imports: [':std/misc/channel'],
    code: `(import :std/misc/channel)
(def ch (make-channel))

;; Producer:
(spawn (lambda ()
  (for-each (lambda (x) (channel-put ch x)) '(1 2 3))
  (channel-close ch)))

;; Consumer — iterate until closed:
(let loop ()
  (let ((val (channel-try-get ch eof)))
    (unless (eof-object? val)
      (displayln "got: " val)
      (loop))))`,
    notes: 'Use channel-get for blocking reads. Avoid spinning on channel-try-get in a tight loop — use channel-get instead.',
    related: ['spawn-thread'],
  },

  // ── Testing ───────────────────────────────────────────────────────
  {
    id: 'test-basics',
    title: 'Write tests with :std/test',
    tags: ['test', 'check', 'assert', 'suite', 'unit', 'testing'],
    imports: [':std/test'],
    code: `(import :std/test)
(export my-test)

(def my-test
  (test-suite "my module"
    (test-case "basic arithmetic"
      (check (+ 1 2) => 3)
      (check (* 3 4) => 12))
    (test-case "predicates"
      (check (string? "hi") ? values))
    (test-case "exceptions"
      (check-exception (error "boom") error-exception?))))

;; Run: (run-tests! my-test) (test-report-summary!)`,
    notes: 'Test files should be named *-test.ss and export *-test symbols. Run with: gerbil test ./...',
  },

  // ── Sorting ───────────────────────────────────────────────────────
  {
    id: 'sort-list',
    title: 'Sort a list',
    tags: ['sort', 'order', 'list', 'compare'],
    imports: [':std/sort'],
    code: `(import :std/sort)
(sort '(3 1 4 1 5 9) <)         ;; => (1 1 3 4 5 9)
(sort '("banana" "apple" "cherry") string<?)
;; => ("apple" "banana" "cherry")

;; Sort by a key:
(sort '((3 "c") (1 "a") (2 "b"))
  (lambda (a b) (< (car a) (car b))))`,
  },

  // ── Structs & Classes ─────────────────────────────────────────────
  {
    id: 'struct-basics',
    title: 'Define and use structs',
    tags: ['struct', 'record', 'type', 'data', 'define', 'constructor'],
    imports: [],
    code: `(defstruct point (x y) transparent: #t)
(def p (make-point 3 4))
(point-x p)              ;; => 3
(point? p)               ;; => #t
(point-x-set! p 10)      ;; mutate

;; Inheritance:
(defstruct (point3d point) (z) transparent: #t)
(def p3 (make-point3d 1 2 3))
(point-x p3)             ;; => 1 (inherited)
(point3d-z p3)            ;; => 3`,
  },

  // ── Optional / Keyword Args ───────────────────────────────────────
  {
    id: 'optional-keyword-args',
    title: 'Optional and keyword arguments',
    tags: ['optional', 'keyword', 'argument', 'parameter', 'default', 'function', 'def'],
    imports: [],
    code: `;; Optional argument with default:
(def (greet name (greeting "Hello"))
  (string-append greeting ", " name "!"))
(greet "Alice")           ;; => "Hello, Alice!"
(greet "Alice" "Hi")      ;; => "Hi, Alice!"

;; Keyword argument:
(def (connect host port: (port 80) ssl: (ssl #f))
  (list host port ssl))
(connect "example.com")                    ;; => ("example.com" 80 #f)
(connect "example.com" port: 443 ssl: #t)  ;; => ("example.com" 443 #t)

;; Rest arguments:
(def (log level . messages)
  (displayln "[" level "] " (string-join (map object->string messages) " ")))`,
  },
];

const MAX_RESULTS = 5;

export function registerHowtoTool(server: McpServer): void {
  server.registerTool(
    'gerbil_howto',
    {
      title: 'Gerbil Cookbook',
      description:
        'Search curated Gerbil Scheme idioms and recipes by keyword. ' +
        'Returns code examples with imports and usage notes. ' +
        'Examples: "read json", "hash table iterate", "http post", "error handling".',
      inputSchema: {
        query: z
          .string()
          .describe(
            'Search keywords (e.g. "json parse", "file read", "channel thread")',
          ),
        cookbook_path: z
          .string()
          .optional()
          .describe(
            'Absolute path to a JSON cookbook file with additional recipes to merge (e.g. "/home/user/project/.claude/cookbooks.json")',
          ),
      },
    },
    async ({ query, cookbook_path }) => {
      const words = query
        .toLowerCase()
        .split(/\s+/)
        .filter((w) => w.length > 0);

      if (words.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Please provide search keywords (e.g. "json parse", "file read").',
            },
          ],
          isError: true,
        };
      }

      // Always merge repo cookbook, then optionally an extra cookbook_path
      let recipes: Recipe[] = [...RECIPES];
      const sources = [REPO_COOKBOOK_PATH];
      if (cookbook_path) sources.push(cookbook_path);
      for (const src of sources) {
        const external = loadCookbook(src);
        if (external.length > 0) {
          const externalIds = new Set(external.map((r) => r.id));
          recipes = recipes.filter((r) => !externalIds.has(r.id));
          recipes.push(...external);
        }
      }

      // Score each recipe
      const scored = recipes.map((recipe) => {
        let score = 0;
        for (const word of words) {
          // Tags: weight 5
          for (const tag of recipe.tags) {
            if (tag.includes(word) || word.includes(tag)) score += 5;
          }
          // Title: weight 3
          if (recipe.title.toLowerCase().includes(word)) score += 3;
          // ID: weight 2
          if (recipe.id.includes(word)) score += 2;
          // Notes: weight 1
          if (recipe.notes?.toLowerCase().includes(word)) score += 1;
          // Code: weight 1
          if (recipe.code.toLowerCase().includes(word)) score += 1;
        }
        return { recipe, score };
      });

      // Sort by score descending, take top results
      const matches = scored
        .filter((s) => s.score > 0)
        .sort((a, b) => b.score - a.score)
        .slice(0, MAX_RESULTS);

      if (matches.length === 0) {
        const available = [...new Set(recipes.flatMap((r) => r.tags))]
          .sort()
          .join(', ');
        return {
          content: [
            {
              type: 'text' as const,
              text: `No recipes found for "${query}".\n\nAvailable topics: ${available}`,
            },
          ],
        };
      }

      const sections: string[] = [
        `Found ${matches.length} recipe(s) for "${query}":`,
      ];

      for (const { recipe } of matches) {
        sections.push('');
        sections.push(`## ${recipe.title}`);
        if (recipe.imports.length > 0) {
          sections.push(`Imports: ${recipe.imports.join(' ')}`);
        }
        sections.push('```scheme');
        sections.push(recipe.code);
        sections.push('```');
        if (recipe.notes) {
          sections.push(`Note: ${recipe.notes}`);
        }
        if (recipe.related && recipe.related.length > 0) {
          sections.push(`Related: ${recipe.related.join(', ')}`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
