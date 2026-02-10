import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi, escapeSchemeString, RESULT_MARKER } from '../gxi.js';

/**
 * Known behavior cards for common functions with edge cases.
 * Each entry maps a function name to a set of test cases.
 */
const BEHAVIOR_CARDS: Record<string, {
  module?: string;
  cases: Array<{
    label: string;
    expr: string;
    note?: string;
  }>;
}> = {
  'hash-get': {
    cases: [
      { label: 'existing key', expr: '(hash-get (hash ("a" 1)) "a")', note: 'returns value' },
      { label: 'missing key', expr: '(hash-get (hash ("a" 1)) "b")', note: 'returns #f' },
      { label: 'wrong arity (3 args)', expr: '(hash-get (hash ("a" 1)) "a" 42)', note: 'ERROR: hash-get is strictly 2-arity' },
    ],
  },
  'hash-ref': {
    cases: [
      { label: 'existing key', expr: '(hash-ref (hash ("a" 1)) "a")', note: 'returns value' },
      { label: 'missing key (no default)', expr: '(hash-ref (hash ("a" 1)) "b")', note: 'returns #!void (not an error!)' },
      { label: 'missing key with default', expr: '(hash-ref (hash ("a" 1)) "b" 42)', note: 'returns default value' },
    ],
  },
  'hash-key?': {
    cases: [
      { label: 'existing key', expr: '(hash-key? (hash ("a" 1)) "a")' },
      { label: 'missing key', expr: '(hash-key? (hash ("a" 1)) "b")' },
    ],
  },
  'assoc': {
    cases: [
      { label: 'found', expr: '(assoc "b" \'(("a" . 1) ("b" . 2)))' },
      { label: 'not found', expr: '(assoc "z" \'(("a" . 1) ("b" . 2)))', note: 'returns #f' },
      { label: 'empty list', expr: '(assoc "a" \'())', note: 'returns #f' },
    ],
  },
  'string-ref': {
    cases: [
      { label: 'valid index', expr: '(string-ref "hello" 0)' },
      { label: 'out of bounds', expr: '(string-ref "hello" 10)', note: 'ERROR: index out of range' },
      { label: 'empty string', expr: '(string-ref "" 0)', note: 'ERROR' },
    ],
  },
  'list-ref': {
    cases: [
      { label: 'valid index', expr: '(list-ref \'(a b c) 1)' },
      { label: 'out of bounds', expr: '(list-ref \'(a b c) 5)', note: 'ERROR' },
      { label: 'empty list', expr: '(list-ref \'() 0)', note: 'ERROR' },
    ],
  },
  'car': {
    cases: [
      { label: 'normal pair', expr: '(car \'(1 2 3))' },
      { label: 'empty list', expr: '(car \'())', note: 'ERROR' },
    ],
  },
  'cdr': {
    cases: [
      { label: 'normal list', expr: '(cdr \'(1 2 3))' },
      { label: 'single element', expr: '(cdr \'(1))', note: 'returns ()' },
      { label: 'empty list', expr: '(cdr \'())', note: 'ERROR' },
    ],
  },
  'string->number': {
    cases: [
      { label: 'valid integer', expr: '(string->number "42")' },
      { label: 'valid float', expr: '(string->number "3.14")' },
      { label: 'invalid string', expr: '(string->number "hello")', note: 'returns #f (not an error!)' },
      { label: 'empty string', expr: '(string->number "")', note: 'returns #f' },
    ],
  },
  'vector-ref': {
    cases: [
      { label: 'valid index', expr: '(vector-ref #(a b c) 1)' },
      { label: 'out of bounds', expr: '(vector-ref #(a b c) 5)', note: 'ERROR' },
    ],
  },
  'read-json': {
    module: ':std/text/json',
    cases: [
      { label: 'object', expr: '(call-with-input-string "{\\"a\\":1}" read-json)', note: 'returns hash-table' },
      { label: 'array', expr: '(call-with-input-string "[1,2,3]" read-json)', note: 'returns list' },
      { label: 'null', expr: '(call-with-input-string "null" read-json)' },
      { label: 'invalid JSON', expr: '(call-with-input-string "not json" read-json)', note: 'ERROR' },
    ],
  },
  'string-split': {
    module: ':std/misc/string',
    cases: [
      { label: 'normal split', expr: '(string-split "a,b,c" #\\,)' },
      { label: 'no delimiter found', expr: '(string-split "abc" #\\,)', note: 'returns list with original string' },
      { label: 'empty string', expr: '(string-split "" #\\,)' },
    ],
  },

  // ── Hash table operations ────────────────────────────────────────
  'hash-update!': {
    cases: [
      { label: 'increment existing', expr: '(let ((h (hash ("a" 1)))) (hash-update! h "a" 1+) (hash-ref h "a"))', note: 'increments value' },
      { label: 'missing key with default', expr: '(let ((h (hash))) (hash-update! h "x" 1+ 0) (hash-ref h "x"))', note: 'default is passed to update proc' },
      { label: 'missing key no default', expr: '(let ((h (hash))) (hash-update! h "x" 1+))', note: 'ERROR: key not found' },
    ],
  },
  'hash-merge': {
    cases: [
      { label: 'two hashes', expr: '(let ((a (hash ("x" 1))) (b (hash ("y" 2)))) (hash->list (hash-merge a b)))', note: 'combines both' },
      { label: 'overlapping keys', expr: '(let ((a (hash ("x" 1))) (b (hash ("x" 99)))) (hash-ref (hash-merge a b) "x"))', note: 'left hash wins' },
      { label: 'empty merge', expr: '(hash->list (hash-merge (hash) (hash)))', note: 'empty result' },
    ],
  },
  'hash-merge!': {
    cases: [
      { label: 'mutating merge', expr: '(let ((a (hash ("x" 1))) (b (hash ("y" 2)))) (hash-merge! a b) (hash->list a))', note: 'a is modified in-place' },
      { label: 'overlapping (right wins)', expr: '(let ((a (hash ("x" 1))) (b (hash ("x" 99)))) (hash-merge! a b) (hash-ref a "x"))', note: 'right hash overwrites' },
    ],
  },
  'hash-keys': {
    cases: [
      { label: 'normal hash', expr: '(sort (hash-keys (hash ("b" 2) ("a" 1))) string<?)' },
      { label: 'empty hash', expr: '(hash-keys (hash))', note: 'returns empty list' },
    ],
  },
  'hash-values': {
    cases: [
      { label: 'normal hash', expr: '(sort (hash-values (hash ("b" 2) ("a" 1))) <)' },
      { label: 'empty hash', expr: '(hash-values (hash))', note: 'returns empty list' },
    ],
  },
  'hash->list': {
    cases: [
      { label: 'normal hash', expr: '(hash->list (hash ("a" 1)))', note: 'list of (key . value) pairs' },
      { label: 'empty hash', expr: '(hash->list (hash))', note: 'returns empty list' },
    ],
  },
  'hash-copy': {
    cases: [
      { label: 'shallow copy', expr: '(let* ((a (hash ("x" 1))) (b (hash-copy a))) (hash-put! b "x" 99) (hash-ref a "x"))', note: 'original unchanged' },
    ],
  },
  'hash-clear!': {
    cases: [
      { label: 'clear all', expr: '(let ((h (hash ("a" 1) ("b" 2)))) (hash-clear! h) (hash->list h))', note: 'empty after clear' },
    ],
  },
  'hash-for-each': {
    cases: [
      { label: 'iterate pairs', expr: '(let ((r \'())) (hash-for-each (lambda (k v) (set! r (cons k r))) (hash ("a" 1) ("b" 2))) (sort r string<?))', note: 'callback gets key and value' },
      { label: 'empty hash', expr: '(hash-for-each (lambda (k v) (error "never")) (hash))', note: 'callback never called' },
    ],
  },
  'hash-fold': {
    cases: [
      { label: 'sum values', expr: '(hash-fold (lambda (k v acc) (+ acc v)) 0 (hash ("a" 1) ("b" 2) ("c" 3)))', note: 'accumulator pattern' },
      { label: 'empty hash', expr: '(hash-fold (lambda (k v acc) (error "never")) 42 (hash))', note: 'returns initial value' },
    ],
  },
  'hash-find': {
    cases: [
      { label: 'found', expr: '(hash-find (lambda (k v) (> v 1)) (hash ("a" 1) ("b" 2)))', note: 'returns first matching (k . v) pair' },
      { label: 'not found', expr: '(hash-find (lambda (k v) (> v 99)) (hash ("a" 1) ("b" 2)))', note: 'returns #f' },
    ],
  },
  'hash-count': {
    cases: [
      { label: 'count entries', expr: '(hash-count (hash ("a" 1) ("b" 2) ("c" 3)))' },
      { label: 'empty hash', expr: '(hash-count (hash))', note: 'returns 0' },
    ],
  },
  'hash-remove!': {
    cases: [
      { label: 'remove existing', expr: '(let ((h (hash ("a" 1) ("b" 2)))) (hash-remove! h "a") (hash->list h))' },
      { label: 'remove missing', expr: '(let ((h (hash ("a" 1)))) (hash-remove! h "z") (hash->list h))', note: 'no error, no change' },
    ],
  },

  // ── Hash filter/select (std/misc/hash) ───────────────────────────
  'hash-filter': {
    module: ':std/misc/hash',
    cases: [
      { label: 'keep matching', expr: '(hash->list (hash-filter (lambda (k v) (> v 1)) (hash ("a" 1) ("b" 2) ("c" 3))))', note: 'predicate gets (key, value)' },
      { label: 'empty result', expr: '(hash->list (hash-filter (lambda (k v) #f) (hash ("a" 1))))', note: 'all filtered out' },
    ],
  },
  'hash-remove': {
    module: ':std/misc/hash',
    cases: [
      { label: 'remove matching', expr: '(hash->list (hash-remove (lambda (k v) (> v 1)) (hash ("a" 1) ("b" 2) ("c" 3))))', note: 'inverse of hash-filter!' },
    ],
  },

  // ── Sorting ──────────────────────────────────────────────────────
  'sort': {
    module: ':std/sort',
    cases: [
      { label: 'sort numbers', expr: '(sort \'(3 1 2) <)' },
      { label: 'sort strings', expr: '(sort \'("banana" "apple" "cherry") string<?)' },
      { label: 'empty list', expr: '(sort \'() <)', note: 'returns empty list' },
      { label: 'single element', expr: '(sort \'(1) <)', note: 'returns list with single element' },
      { label: 'sort vector', expr: '(sort #(3 1 2) <)', note: 'also works on vectors' },
    ],
  },
  'stable-sort': {
    module: ':std/sort',
    cases: [
      { label: 'preserves equal order', expr: '(stable-sort \'((1 . "a") (2 . "b") (1 . "c")) (lambda (a b) (< (car a) (car b))))', note: 'equal elements keep original order' },
    ],
  },

  // ── List utilities (std/misc/list) ───────────────────────────────
  'flatten': {
    module: ':std/misc/list',
    cases: [
      { label: 'nested lists', expr: '(flatten \'(1 (2 (3 4) 5) 6))', note: 'recursively flattens ALL nesting' },
      { label: 'already flat', expr: '(flatten \'(1 2 3))' },
      { label: 'empty list', expr: '(flatten \'())', note: 'returns empty list' },
    ],
  },
  'flatten1': {
    module: ':std/misc/list',
    cases: [
      { label: 'one level', expr: '(flatten1 \'(1 (2 (3 4)) 5))', note: 'only removes ONE level of nesting' },
      { label: 'empty list', expr: '(flatten1 \'())' },
    ],
  },
  'unique': {
    module: ':std/misc/list',
    cases: [
      { label: 'remove duplicates', expr: '(unique \'(1 2 1 3 2 3))' },
      { label: 'already unique', expr: '(unique \'(1 2 3))' },
      { label: 'empty list', expr: '(unique \'())', note: 'returns empty list' },
    ],
  },
  'butlast': {
    module: ':std/misc/list',
    cases: [
      { label: 'normal list', expr: '(butlast \'(1 2 3))', note: 'returns all but last element' },
      { label: 'single element', expr: '(butlast \'(1))' },
      { label: 'empty list', expr: '(butlast \'())', note: 'returns empty list (not error!)' },
    ],
  },
  'take': {
    module: ':std/misc/list',
    cases: [
      { label: 'take 2', expr: '(take \'(1 2 3 4 5) 2)' },
      { label: 'take 0', expr: '(take \'(1 2 3) 0)', note: 'returns empty list' },
      { label: 'take more than length', expr: '(take \'(1 2) 5)', note: 'check behavior' },
    ],
  },
  'drop': {
    module: ':std/misc/list',
    cases: [
      { label: 'drop 2', expr: '(drop \'(1 2 3 4 5) 2)' },
      { label: 'drop 0', expr: '(drop \'(1 2 3) 0)', note: 'returns full list' },
      { label: 'drop more than length', expr: '(drop \'(1 2) 5)', note: 'check behavior' },
    ],
  },

  // ── String utilities ─────────────────────────────────────────────
  'string-join': {
    module: ':std/misc/string',
    cases: [
      { label: 'join with comma', expr: '(string-join \'("a" "b" "c") ",")' },
      { label: 'join with space', expr: '(string-join \'("hello" "world") " ")' },
      { label: 'empty list', expr: '(string-join \'() ",")', note: 'returns empty string' },
      { label: 'single element', expr: '(string-join \'("only") ",")', note: 'no separator added' },
    ],
  },
  'string-contains': {
    module: ':std/srfi/13',
    cases: [
      { label: 'found', expr: '(string-contains "hello world" "world")', note: 'returns index (not boolean!)' },
      { label: 'not found', expr: '(string-contains "hello world" "xyz")', note: 'returns #f' },
      { label: 'empty needle', expr: '(string-contains "hello" "")', note: 'check behavior' },
    ],
  },

  // ── Path utilities ───────────────────────────────────────────────
  'path-extension': {
    cases: [
      { label: 'with extension', expr: '(path-extension "file.txt")', note: 'returns ".txt" (with dot!)' },
      { label: 'no extension', expr: '(path-extension "file")', note: 'returns ""' },
      { label: 'dotfile', expr: '(path-extension ".gitignore")', note: 'check behavior' },
      { label: 'double extension', expr: '(path-extension "archive.tar.gz")' },
    ],
  },
  'path-strip-extension': {
    cases: [
      { label: 'strip extension', expr: '(path-strip-extension "file.txt")' },
      { label: 'no extension', expr: '(path-strip-extension "file")', note: 'returns unchanged' },
    ],
  },
  'path-directory': {
    cases: [
      { label: 'with directory', expr: '(path-directory "/usr/bin/gxi")' },
      { label: 'no directory', expr: '(path-directory "file.txt")' },
    ],
  },

  // ── Port/IO utilities ────────────────────────────────────────────
  'read-all-as-string': {
    module: ':std/misc/ports',
    cases: [
      { label: 'from string port', expr: '(call-with-input-string "hello\\nworld" read-all-as-string)' },
      { label: 'empty input', expr: '(call-with-input-string "" read-all-as-string)', note: 'returns ""' },
    ],
  },
  'read-all-as-lines': {
    module: ':std/misc/ports',
    cases: [
      { label: 'multi-line', expr: '(call-with-input-string "a\\nb\\nc" read-all-as-lines)' },
      { label: 'empty input', expr: '(call-with-input-string "" read-all-as-lines)', note: 'returns empty list' },
    ],
  },

  // ── JSON output ──────────────────────────────────────────────────
  'write-json': {
    module: ':std/text/json',
    cases: [
      { label: 'hash table', expr: '(call-with-output-string (lambda (p) (write-json (hash ("a" 1)) p)))' },
      { label: 'list as array', expr: '(call-with-output-string (lambda (p) (write-json \'(1 2 3) p)))' },
      { label: 'boolean', expr: '(call-with-output-string (lambda (p) (write-json #t p)))' },
      { label: '#f as false', expr: '(call-with-output-string (lambda (p) (write-json #f p)))' },
      { label: 'void as null', expr: '(call-with-output-string (lambda (p) (write-json (void) p)))', note: '#!void becomes null' },
    ],
  },
  'json-object->string': {
    module: ':std/text/json',
    cases: [
      { label: 'hash to string', expr: '(json-object->string (hash ("key" "value")))' },
      { label: 'number', expr: '(json-object->string 42)' },
      { label: 'null (void)', expr: '(json-object->string (void))' },
    ],
  },

  // ── Alist operations ─────────────────────────────────────────────
  'assq': {
    cases: [
      { label: 'found (eq?)', expr: '(assq \'b \'((a . 1) (b . 2)))' },
      { label: 'not found', expr: '(assq \'z \'((a . 1) (b . 2)))', note: 'returns #f' },
      { label: 'empty alist', expr: '(assq \'a \'())', note: 'returns #f' },
    ],
  },
  'assv': {
    cases: [
      { label: 'found (eqv?)', expr: '(assv 2 \'((1 . "a") (2 . "b")))' },
      { label: 'not found', expr: '(assv 99 \'((1 . "a")))', note: 'returns #f' },
    ],
  },

  // ── Control flow edge cases ──────────────────────────────────────
  'when': {
    cases: [
      { label: 'true condition', expr: '(when #t "yes")', note: 'returns body value' },
      { label: 'false condition', expr: '(when #f "yes")', note: 'returns #!void (not #f!)' },
    ],
  },
  'unless': {
    cases: [
      { label: 'false condition', expr: '(unless #f "yes")', note: 'returns body value' },
      { label: 'true condition', expr: '(unless #t "yes")', note: 'returns #!void (not #f!)' },
    ],
  },
  'cond': {
    cases: [
      { label: 'first match', expr: '(cond ((= 1 1) "yes") (#t "no"))' },
      { label: 'no match, no else', expr: '(cond ((= 1 2) "yes"))', note: 'returns #!void (not an error!)' },
      { label: 'with else', expr: '(cond ((= 1 2) "yes") (else "fallback"))' },
    ],
  },

  // ── Iteration ────────────────────────────────────────────────────
  'for-each': {
    cases: [
      { label: 'side effects', expr: '(let ((r 0)) (for-each (lambda (x) (set! r (+ r x))) \'(1 2 3)) r)', note: 'for side effects, not collecting' },
      { label: 'empty list', expr: '(for-each (lambda (x) (error "never")) \'())', note: 'callback never called' },
    ],
  },
  'map': {
    cases: [
      { label: 'transform list', expr: '(map 1+ \'(1 2 3))' },
      { label: 'empty list', expr: '(map 1+ \'())', note: 'returns empty list' },
      { label: 'two lists', expr: '(map + \'(1 2 3) \'(10 20 30))', note: 'parallel map' },
    ],
  },
  'filter': {
    cases: [
      { label: 'keep matching', expr: '(filter odd? \'(1 2 3 4 5))' },
      { label: 'none match', expr: '(filter odd? \'(2 4 6))', note: 'returns empty list' },
      { label: 'empty list', expr: '(filter odd? \'())' },
    ],
  },
  'apply': {
    cases: [
      { label: 'apply to list', expr: '(apply + \'(1 2 3))' },
      { label: 'with prefix args', expr: '(apply + 1 2 \'(3 4 5))', note: 'prefix args + rest list' },
      { label: 'empty list', expr: '(apply + \'())', note: 'returns 0 (identity for +)' },
    ],
  },
  'number->string': {
    cases: [
      { label: 'integer', expr: '(number->string 42)' },
      { label: 'float', expr: '(number->string 3.14)' },
      { label: 'with radix', expr: '(number->string 255 16)', note: 'hex representation' },
      { label: 'zero', expr: '(number->string 0)' },
    ],
  },
  'string->symbol': {
    cases: [
      { label: 'normal', expr: '(string->symbol "hello")' },
      { label: 'empty string', expr: '(string->symbol "")', note: 'creates empty symbol ||' },
      { label: 'with spaces', expr: '(string->symbol "hello world")', note: 'creates symbol |hello world|' },
    ],
  },
  'symbol->string': {
    cases: [
      { label: 'normal', expr: '(symbol->string \'hello)' },
      { label: 'keyword', expr: '(keyword->string hello:)', note: 'use keyword->string for keywords' },
    ],
  },
};

export function registerFunctionBehaviorTool(server: McpServer): void {
  server.registerTool(
    'gerbil_function_behavior',
    {
      title: 'Function Behavior Card',
      description:
        'Generate a behavior card for a Gerbil function showing return values for ' +
        'normal cases, edge cases, and error conditions. Evaluates the function with ' +
        'representative inputs including empty collections, missing keys, out-of-bounds, ' +
        'zero values, and type mismatches. Returns a table of inputs → outputs. ' +
        'Has built-in cards for common functions; for unknown functions, generates ' +
        'test cases dynamically based on arity.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: false,
      },
      inputSchema: {
        function_name: z
          .string()
          .describe('Function name (e.g. "hash-get", "assoc", "string-ref")'),
        module: z
          .string()
          .optional()
          .describe('Module to import for the function (e.g. ":std/text/json")'),
        custom_cases: z
          .array(z.object({
            label: z.string().describe('Description of the test case'),
            expr: z.string().describe('Gerbil expression to evaluate'),
          }))
          .optional()
          .describe('Additional custom test cases to include'),
      },
    },
    async ({ function_name, module, custom_cases }) => {
      const card = BEHAVIOR_CARDS[function_name];
      const cases: Array<{ label: string; expr: string; note?: string }> = [];
      let importModule = module;

      if (card) {
        cases.push(...card.cases);
        if (card.module && !importModule) {
          importModule = card.module;
        }
      }

      if (custom_cases) {
        cases.push(...custom_cases);
      }

      // If no built-in card and no custom cases, try to generate basic ones
      if (cases.length === 0) {
        // Just try evaluating it with no args, one arg, etc.
        cases.push(
          { label: 'value type', expr: function_name, note: 'check if it\'s a procedure' },
          { label: '0 args', expr: `(${function_name})` },
          { label: '1 arg (number)', expr: `(${function_name} 0)` },
          { label: '1 arg (string)', expr: `(${function_name} "test")` },
          { label: '1 arg (list)', expr: `(${function_name} '())` },
          { label: '2 args', expr: `(${function_name} 0 1)` },
        );
      }

      // Evaluate each case
      const results: Array<{
        label: string;
        expr: string;
        result: string;
        isError: boolean;
        note?: string;
      }> = [];

      for (const testCase of cases) {
        const importExprs = importModule ? [`(import ${importModule})`] : [];
        const evalExpr = [
          ...importExprs,
          [
            '(with-catch',
            '  (lambda (e)',
            `    (display "ERROR: ")`,
            '    (display-exception e (current-output-port)))',
            '  (lambda ()',
            `    (let ((v ${testCase.expr}))`,
            '      (display "OK: ")',
            '      (write v))))',
          ].join(' '),
        ];

        try {
          const gxiResult = await runGxi(evalExpr, { timeout: 5000 });
          const output = (gxiResult.stdout + gxiResult.stderr).trim();
          if (output.startsWith('OK: ')) {
            results.push({
              label: testCase.label,
              expr: testCase.expr,
              result: output.slice(4),
              isError: false,
              note: testCase.note,
            });
          } else if (output.startsWith('ERROR: ')) {
            results.push({
              label: testCase.label,
              expr: testCase.expr,
              result: output.slice(7).split('\n')[0],
              isError: true,
              note: testCase.note,
            });
          } else {
            results.push({
              label: testCase.label,
              expr: testCase.expr,
              result: output || '(no output)',
              isError: gxiResult.exitCode !== 0,
              note: testCase.note,
            });
          }
        } catch (e) {
          results.push({
            label: testCase.label,
            expr: testCase.expr,
            result: String(e).split('\n')[0],
            isError: true,
            note: testCase.note,
          });
        }
      }

      // Format as behavior card
      const sections: string[] = [
        `# Behavior Card: \`${function_name}\``,
        '',
        importModule ? `Module: \`${importModule}\`` : 'Module: built-in',
        '',
        '| Case | Expression | Result | Notes |',
        '|------|-----------|--------|-------|',
      ];

      for (const r of results) {
        const icon = r.isError ? 'ERROR' : 'ok';
        const note = r.note || '';
        const expr = r.expr.length > 50 ? r.expr.slice(0, 47) + '...' : r.expr;
        const result = r.result.length > 60 ? r.result.slice(0, 57) + '...' : r.result;
        sections.push(`| ${r.label} | \`${expr}\` | ${icon}: \`${result}\` | ${note} |`);
      }

      const errors = results.filter(r => r.isError);
      const successes = results.filter(r => !r.isError);
      sections.push('');
      sections.push(`Summary: ${successes.length} ok, ${errors.length} error(s) out of ${results.length} cases`);

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
