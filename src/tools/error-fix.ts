import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFileSync, writeFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export interface ErrorFix {
  id: string;
  pattern: string;            // Regex pattern to match error messages
  type: string;               // Error category
  fix: string;                // The fix explanation
  code_example?: string;      // Working code example
  wrong_example?: string;     // The wrong pattern that causes this error
  imports?: string[];          // Required imports for the fix
  related_recipes?: string[]; // Cookbook recipe IDs
}

const ERROR_FIXES_PATH = join(__dirname, '..', '..', 'error-fixes.json');

/** Built-in error fixes for the most common Gerbil errors */
const BUILTIN_FIXES: ErrorFix[] = [
  {
    id: 'hash-get-3-args',
    pattern: 'hash-get.*[Ww]rong number.*3',
    type: 'Arity Error',
    fix: 'hash-get is strictly 2-arity: (hash-get table key) → returns value or #f. ' +
      'For a default value, use hash-ref with 3 args: (hash-ref table key default).',
    wrong_example: '(hash-get ht "key" "default")  ;; ERROR: 3 args',
    code_example: '(hash-ref ht "key" "default")  ;; OK: returns "default" if missing',
    related_recipes: ['hash-table-basics'],
  },
  {
    id: 'unbound-for-collect',
    pattern: '[Uu]nbound.*for/collect',
    type: 'Missing Import',
    fix: 'for/collect requires (import :std/iter). It is not available by default.',
    code_example: '(import :std/iter)\n(for/collect (x \'(1 2 3)) (* x x))',
    imports: [':std/iter'],
    related_recipes: ['for-collect'],
  },
  {
    id: 'unbound-sort',
    pattern: '[Uu]nbound.*\\bsort\\b',
    type: 'Missing Import',
    fix: 'sort requires (import :std/sort). It is not available by default in gxi.',
    code_example: '(import :std/sort)\n(sort \'(3 1 2) <)',
    imports: [':std/sort'],
    related_recipes: ['sort-list'],
  },
  {
    id: 'unbound-displayln',
    pattern: '[Uu]nbound.*displayln',
    type: 'Missing Import',
    fix: 'displayln is available by default in gxi, but NOT in compiled code. ' +
      'Use (import :std/misc/repr) or define your own: (def (displayln . args) (for-each display args) (newline)).',
    code_example: '(def (displayln . args) (for-each display args) (newline))',
  },
  {
    id: 'unbound-in-hash',
    pattern: '[Uu]nbound.*in-hash',
    type: 'Missing Import',
    fix: 'in-hash, in-hash-keys, in-hash-values require (import :std/iter).',
    code_example: '(import :std/iter)\n(for ((k v) (in-hash ht)) (displayln k " => " v))',
    imports: [':std/iter'],
    related_recipes: ['iterate-hash'],
  },
  {
    id: 'unbound-read-json',
    pattern: '[Uu]nbound.*read-json',
    type: 'Missing Import',
    fix: 'read-json requires (import :std/text/json).',
    code_example: '(import :std/text/json)\n(call-with-input-string "{\\"a\\":1}" read-json)',
    imports: [':std/text/json'],
    related_recipes: ['json-parse'],
  },
  {
    id: 'unbound-pregexp',
    pattern: '[Uu]nbound.*pregexp',
    type: 'Missing Import',
    fix: 'pregexp-match, pregexp-replace, etc. require (import :std/pregexp).',
    code_example: '(import :std/pregexp)\n(pregexp-match "\\\\d+" "abc123")',
    imports: [':std/pregexp'],
    related_recipes: ['string-regex'],
  },
  {
    id: 'dot-in-brackets',
    pattern: '[Bb]ad syntax.*illegal use of \\.',
    type: 'Syntax Error',
    fix: 'Dotted pairs inside [...] brackets are not allowed. Use parentheses instead: (a . b) not [a . b].',
    wrong_example: '[a . b]',
    code_example: '(a . b)',
  },
  {
    id: 'import-conflict',
    pattern: '[Bb]ad binding.*import conflict',
    type: 'Import Conflict',
    fix: 'A locally defined symbol conflicts with an imported module export. ' +
      'Either rename your local definition, or use (only-in module ...) to selectively import.',
    code_example: '(import (only-in :std/iter for for/collect))  ;; selective import',
  },
  {
    id: 'hash-ref-missing-void',
    pattern: 'hash-ref.*#!void|#!unbound',
    type: 'Semantic Gotcha',
    fix: 'hash-ref returns #!void for missing keys (not #f, not an error!). ' +
      'Use hash-get for safe lookup returning #f, or hash-ref with 3 args for explicit default.',
    wrong_example: '(if (hash-ref ht "key") ...)  ;; WRONG: #!void is truthy!',
    code_example: '(hash-get ht "key")           ;; returns #f for missing\n(hash-ref ht "key" #f)       ;; explicit default',
    related_recipes: ['hash-table-basics'],
  },
  {
    id: 'wrong-arity-generic',
    pattern: '[Ww]rong number of arguments.*passed to procedure',
    type: 'Arity Error',
    fix: 'Use gerbil_function_signature to check the exact arity. Common gotchas: ' +
      'hash-get is 2-arity, not 3. Many :std/iter forms take iterators, not lists directly.',
  },
  {
    id: 'module-not-found',
    pattern: 'cannot find module|[Mm]odule not found',
    type: 'Module Not Found',
    fix: 'Check the module path spelling. Use gerbil_list_std_modules to discover available modules. ' +
      'For project-local modules, ensure GERBIL_LOADPATH includes the project directory.',
  },
  {
    id: 'type-string-expected',
    pattern: '[Tt]ype exception.*expected.*string|argument.*must be.*string',
    type: 'Type Error',
    fix: 'A function expected a string but received a different type. Use object->string for conversion.',
    code_example: '(object->string 42)  ;; => "42"',
  },
  {
    id: 'unwind-protect-port',
    pattern: 'port.*leak|[Cc]lose.*port',
    type: 'Resource Leak',
    fix: 'Always use call-with-input-file / call-with-output-file instead of open-input-file / open-output-file. ' +
      'The call-with-* forms automatically close the port.',
    wrong_example: '(let ((p (open-input-file "f"))) (read-line p) (close-port p))',
    code_example: '(call-with-input-file "f" read-line)',
    related_recipes: ['read-file-string'],
  },
  {
    id: 'segfault-stale',
    pattern: '[Ss]egmentation fault|SIGSEGV|#!unbound',
    type: 'Stale Build Artifact',
    fix: 'Segfaults in compiled code are often caused by stale artifacts. Run: ' +
      'gerbil_stale_static to check, then gerbil clean && gerbil build.',
  },
  {
    id: 'unbound-channel',
    pattern: '[Uu]nbound.*(make-channel|channel-put|channel-get)',
    type: 'Missing Import',
    fix: 'Channel operations require (import :std/misc/channel).',
    code_example: '(import :std/misc/channel)\n(def ch (make-channel))',
    imports: [':std/misc/channel'],
    related_recipes: ['channel-pattern'],
  },
  {
    id: 'unbound-http-get',
    pattern: '[Uu]nbound.*(http-get|http-post)',
    type: 'Missing Import',
    fix: 'HTTP request functions require (import :std/net/request).',
    code_example: '(import :std/net/request)\n(def resp (http-get "https://example.com"))',
    imports: [':std/net/request'],
    related_recipes: ['http-get'],
  },
  {
    id: 'unbound-string-split',
    pattern: '[Uu]nbound.*string-split',
    type: 'Missing Import',
    fix: 'string-split requires (import :std/misc/string).',
    code_example: '(import :std/misc/string)\n(string-split "a,b,c" #\\,)',
    imports: [':std/misc/string'],
    related_recipes: ['string-split-join'],
  },
  {
    id: 'unbound-string-join',
    pattern: '[Uu]nbound.*string-join',
    type: 'Missing Import',
    fix: 'string-join requires (import :std/srfi/13) or (import :std/misc/string).',
    code_example: '(import :std/srfi/13)\n(string-join \'("a" "b" "c") ",")',
    imports: [':std/srfi/13'],
    related_recipes: ['string-split-join'],
  },
  {
    id: 'unbound-hash-merge',
    pattern: '[Uu]nbound.*hash-merge',
    type: 'Built-in Function',
    fix: 'hash-merge is built into Gerbil — no import needed. Check spelling and syntax: ' +
      '(hash-merge ht1 ht2) returns a new merged hash table.',
    code_example: '(hash-merge (hash ("a" 1)) (hash ("b" 2)))',
    related_recipes: ['hash-table-merge'],
  },
];

function loadErrorFixes(): ErrorFix[] {
  try {
    const raw = readFileSync(ERROR_FIXES_PATH, 'utf-8');
    return JSON.parse(raw) as ErrorFix[];
  } catch {
    return [];
  }
}

function saveErrorFixes(fixes: ErrorFix[]): void {
  writeFileSync(ERROR_FIXES_PATH, JSON.stringify(fixes, null, 2) + '\n', 'utf-8');
}

export function registerErrorFixLookupTool(server: McpServer): void {
  server.registerTool(
    'gerbil_error_fix_lookup',
    {
      title: 'Look Up Error Fix',
      description:
        'Look up an exact fix for a Gerbil/Gambit error message. Searches a structured ' +
        'database of known error→fix mappings with working code examples. Much faster than ' +
        'gerbil_explain_error for common errors — returns the specific fix immediately. ' +
        'Covers ~20 most common errors including missing imports, arity mismatches, ' +
        'semantic gotchas, and type errors.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        error_message: z
          .string()
          .describe('The error message or relevant portion of it'),
        search_all: z
          .boolean()
          .optional()
          .describe('If true, return all matching fixes (default: first match only)'),
      },
    },
    async ({ error_message, search_all }) => {
      // Load external fixes and merge with builtins
      const externalFixes = loadErrorFixes();
      const allFixes = [...BUILTIN_FIXES, ...externalFixes];

      const matches: Array<ErrorFix & { matchScore: number }> = [];

      for (const fix of allFixes) {
        try {
          const regex = new RegExp(fix.pattern, 'i');
          if (regex.test(error_message)) {
            // Score by specificity (longer pattern = more specific)
            matches.push({ ...fix, matchScore: fix.pattern.length });
          }
        } catch {
          // Invalid regex in fix.pattern — skip
        }
      }

      if (matches.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: `No direct fix found for this error. Try gerbil_explain_error for general guidance.\n\n` +
              `Error: ${error_message}`,
          }],
        };
      }

      // Sort by specificity
      matches.sort((a, b) => b.matchScore - a.matchScore);
      const results = search_all ? matches : [matches[0]];

      const sections: string[] = [];
      for (const fix of results) {
        sections.push(`## ${fix.type}: \`${fix.id}\`\n`);
        sections.push(`**Fix**: ${fix.fix}\n`);
        if (fix.wrong_example) {
          sections.push('**Wrong**:');
          sections.push('```scheme');
          sections.push(fix.wrong_example);
          sections.push('```\n');
        }
        if (fix.code_example) {
          sections.push('**Correct**:');
          sections.push('```scheme');
          sections.push(fix.code_example);
          sections.push('```\n');
        }
        if (fix.imports && fix.imports.length > 0) {
          sections.push(`**Required imports**: ${fix.imports.join(' ')}\n`);
        }
        if (fix.related_recipes && fix.related_recipes.length > 0) {
          sections.push(`**Cookbook recipes**: ${fix.related_recipes.join(', ')}\n`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

export function registerErrorFixAddTool(server: McpServer): void {
  server.registerTool(
    'gerbil_error_fix_add',
    {
      title: 'Add Error Fix',
      description:
        'Add a new error→fix mapping to the database. Use when you discover a ' +
        'recurring error pattern with a specific fix. The mapping will be available ' +
        'in future gerbil_error_fix_lookup calls.',
      annotations: {
        readOnlyHint: false,
        idempotentHint: false,
      },
      inputSchema: {
        id: z.string().describe('Unique ID in kebab-case (e.g. "unbound-my-func")'),
        pattern: z.string().describe('Regex pattern to match the error message'),
        type: z.string().describe('Error category (e.g. "Missing Import", "Arity Error")'),
        fix: z.string().describe('Explanation of the fix'),
        code_example: z.string().optional().describe('Working code example'),
        wrong_example: z.string().optional().describe('The wrong pattern that causes this error'),
        imports: z.array(z.string()).optional().describe('Required imports for the fix'),
        related_recipes: z.array(z.string()).optional().describe('Related cookbook recipe IDs'),
      },
    },
    async ({ id, pattern, type, fix, code_example, wrong_example, imports, related_recipes }) => {
      // Validate regex
      try {
        new RegExp(pattern);
      } catch (e) {
        return {
          content: [{
            type: 'text' as const,
            text: `Invalid regex pattern: ${e}`,
          }],
          isError: true,
        };
      }

      const newFix: ErrorFix = {
        id,
        pattern,
        type,
        fix,
        ...(code_example ? { code_example } : {}),
        ...(wrong_example ? { wrong_example } : {}),
        ...(imports ? { imports } : {}),
        ...(related_recipes ? { related_recipes } : {}),
      };

      // Load existing, update or append
      const existing = loadErrorFixes();
      const idx = existing.findIndex(f => f.id === id);
      if (idx >= 0) {
        existing[idx] = newFix;
      } else {
        existing.push(newFix);
      }

      try {
        saveErrorFixes(existing);
        return {
          content: [{
            type: 'text' as const,
            text: `Error fix "${id}" ${idx >= 0 ? 'updated' : 'added'} successfully.`,
          }],
        };
      } catch (e) {
        return {
          content: [{
            type: 'text' as const,
            text: `Failed to save error fix: ${e}`,
          }],
          isError: true,
        };
      }
    },
  );
}
