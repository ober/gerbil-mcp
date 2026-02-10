import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';

interface TranslationRule {
  pattern: RegExp;
  replacement: string | ((match: string, ...groups: string[]) => string);
  warning?: string;
  category: 'syntax' | 'function' | 'import' | 'macro' | 'pattern';
}

const TRANSLATION_RULES: TranslationRule[] = [
  // ── Syntax transforms ─────────────────────────────────────────
  {
    pattern: /\(define\s+\((\w[^\s)]*)\s/g,
    replacement: '(def ($1 ',
    category: 'syntax',
  },
  {
    pattern: /\(define\s+(\w[^\s)]*)\s/g,
    replacement: '(def $1 ',
    category: 'syntax',
  },
  {
    pattern: /\(lambda\s/g,
    replacement: '(lambda ',
    category: 'syntax',
    // lambda is valid in Gerbil too — no change needed
  },
  {
    pattern: /\(let-values\s/g,
    replacement: '(let-values ',
    category: 'syntax',
  },
  {
    pattern: /\(define-struct\s+(\w+)\s+\(([^)]*)\)/g,
    replacement: '(defstruct $1 ($2) transparent: #t)',
    category: 'syntax',
    warning: 'Racket define-struct → Gerbil defstruct. Check field names and options.',
  },
  {
    pattern: /\(struct\s+(\w+)\s+\(([^)]*)\)/g,
    replacement: '(defstruct $1 ($2) transparent: #t)',
    category: 'syntax',
    warning: 'Racket struct → Gerbil defstruct. Gerbil uses #t for transparent.',
  },

  // ── Function renames with semantic warnings ───────────────────
  {
    pattern: /hash-has-key\?/g,
    replacement: 'hash-key?',
    category: 'function',
    warning: 'Racket hash-has-key? → Gerbil hash-key? (same semantics).',
  },
  {
    pattern: /(?<![a-zA-Z-])hash-set(?![a-zA-Z?!-])/g,
    replacement: 'hash-put!',
    category: 'function',
    warning: 'Racket hash-set (functional) → Gerbil hash-put! (mutating!). ' +
      'Gerbil hash tables are mutable by default.',
  },
  {
    pattern: /\bfor\/list\b/g,
    replacement: 'for/collect',
    category: 'function',
    warning: 'Racket for/list → Gerbil for/collect. Requires (import :std/iter).',
  },
  {
    pattern: /\bfor\/hash\b/g,
    replacement: 'for/fold',
    category: 'function',
    warning: 'Racket for/hash has no direct equivalent. Use for/fold with hash-put!.',
  },
  {
    pattern: /string-contains\?/g,
    replacement: 'string-contains',
    category: 'function',
    warning: 'Racket string-contains? → Gerbil string-contains (returns index or #f, not boolean). ' +
      'Requires (import :std/srfi/13).',
  },
  {
    pattern: /\bdisplayln\b/g,
    replacement: 'displayln',
    category: 'function',
    // Same in both — no warning needed
  },
  {
    pattern: /\bwith-input-from-string\b/g,
    replacement: 'call-with-input-string',
    category: 'function',
  },
  {
    pattern: /\bwith-output-to-string\b/g,
    replacement: 'call-with-output-string',
    category: 'function',
  },
  {
    pattern: /\bregexp-match\b/g,
    replacement: 'pregexp-match',
    category: 'function',
    warning: 'Racket regexp-match → Gerbil pregexp-match. Requires (import :std/pregexp).',
  },
  {
    pattern: /\bregexp-replace\b/g,
    replacement: 'pregexp-replace',
    category: 'function',
    warning: 'Racket regexp-replace → Gerbil pregexp-replace. Requires (import :std/pregexp).',
  },

  // ── Import translation ────────────────────────────────────────
  {
    pattern: /\(require\s+([^)]+)\)/g,
    replacement: '(import $1)',
    category: 'import',
    warning: 'Racket require → Gerbil import. Module paths need manual translation ' +
      '(e.g. racket/list → :std/misc/list, json → :std/text/json).',
  },
  {
    pattern: /\(provide\s+([^)]+)\)/g,
    replacement: '(export $1)',
    category: 'import',
  },

  // ── Macro system ──────────────────────────────────────────────
  {
    pattern: /\(define-syntax-rule\s/g,
    replacement: '(defrule ',
    category: 'macro',
    warning: 'Racket define-syntax-rule → Gerbil defrule. Syntax is similar but check for ' +
      'template variable usage differences.',
  },
  {
    pattern: /\(syntax-rules\s/g,
    replacement: '(syntax-rules ',
    category: 'macro',
    warning: 'syntax-rules works in Gerbil but defrules is preferred for multiple patterns.',
  },

  // ── Racket error / contract forms ────────────────────────────────
  {
    pattern: /\braise-argument-error\b/g,
    replacement: 'error',
    category: 'function',
    warning: 'Racket raise-argument-error → Gerbil error. ' +
      'Gerbil uses (error "message" irritant ...) instead of typed contract errors.',
  },
  {
    pattern: /\braise-arguments-error\b/g,
    replacement: 'error',
    category: 'function',
    warning: 'Racket raise-arguments-error → Gerbil error.',
  },

  // ── Hash table operations ──────────────────────────────────────
  {
    pattern: /(?<![a-zA-Z-])hash-set!(?![a-zA-Z?!-])/g,
    replacement: 'hash-put!',
    category: 'function',
    warning: 'Racket hash-set! → Gerbil hash-put!.',
  },
  {
    pattern: /\bmake-hash\b/g,
    replacement: 'make-hash-table',
    category: 'function',
    warning: 'Racket make-hash → Gerbil make-hash-table. ' +
      'Or use the (hash (key val) ...) literal syntax.',
  },
  {
    pattern: /\bmake-hasheq\b/g,
    replacement: 'make-hash-table-eq',
    category: 'function',
    warning: 'Racket make-hasheq → Gerbil make-hash-table-eq.',
  },
  {
    pattern: /\bhash-count\b/g,
    replacement: 'hash-length',
    category: 'function',
    warning: 'Racket hash-count → Gerbil hash-length.',
  },
  {
    pattern: /\bhash-empty\?\b/g,
    replacement: '(zero? (hash-length',
    category: 'function',
    warning: 'Racket hash-empty? has no direct Gerbil equivalent. Use (zero? (hash-length h)). ' +
      'The replacement is partial — adjust the closing parenthesis manually.',
  },

  // ── Iteration ──────────────────────────────────────────────────
  {
    pattern: /\bfor\/and\b/g,
    replacement: 'for/fold',
    category: 'function',
    warning: 'Racket for/and has no direct equivalent. Use for/fold with (and acc val). ' +
      'Requires (import :std/iter).',
  },
  {
    pattern: /\bfor\/or\b/g,
    replacement: 'for/fold',
    category: 'function',
    warning: 'Racket for/or has no direct equivalent. Use for/fold with (or acc val). ' +
      'Requires (import :std/iter).',
  },
  {
    pattern: /\bfor\/first\b/g,
    replacement: 'for/fold',
    category: 'function',
    warning: 'Racket for/first has no direct equivalent. Use for/fold to find first match.',
  },
  {
    pattern: /\bin-naturals\b/g,
    replacement: 'in-naturals',
    category: 'function',
    warning: 'Racket in-naturals → use in-range or in-iota in Gerbil. ' +
      'Requires (import :std/iter).',
  },

  // ── Syntax / definition forms ──────────────────────────────────
  {
    pattern: /\(define-syntax\s+(?!\()/g,
    replacement: '(defsyntax ',
    category: 'macro',
    warning: 'Racket define-syntax → Gerbil defsyntax.',
  },
  {
    pattern: /\(define-values\s/g,
    replacement: '(def-values ',
    category: 'syntax',
    warning: 'Racket define-values → Gerbil def-values.',
  },

  // ── Exception handling ─────────────────────────────────────────
  {
    pattern: /\(with-handlers\s/g,
    replacement: '(try ',
    category: 'pattern',
    warning: 'Racket with-handlers → Gerbil try/catch. ' +
      'Syntax differs: (try body ... (catch (Pred var) handler)).',
  },

  // ── I/O ────────────────────────────────────────────────────────
  {
    pattern: /\bport->string\b/g,
    replacement: 'read-all-as-string',
    category: 'function',
    warning: 'Racket port->string → Gerbil read-all-as-string. ' +
      'Requires (import :std/misc/ports).',
  },
  {
    pattern: /\bport->lines\b/g,
    replacement: 'read-all-as-lines',
    category: 'function',
    warning: 'Racket port->lines → Gerbil read-all-as-lines. ' +
      'Requires (import :std/misc/ports).',
  },

  // ── String operations ──────────────────────────────────────────
  {
    pattern: /\bstring-trim\b/g,
    replacement: 'string-trim-both',
    category: 'function',
    warning: 'Racket string-trim → Gerbil string-trim-both (trims both sides). ' +
      'Requires (import :std/srfi/13).',
  },
  {
    pattern: /\bstring-upcase\b/g,
    replacement: 'string-upcase',
    category: 'function',
    warning: 'Racket string-upcase → Gerbil string-upcase. Requires (import :std/srfi/13).',
  },
  {
    pattern: /\bstring-downcase\b/g,
    replacement: 'string-downcase',
    category: 'function',
    warning: 'Racket string-downcase → Gerbil string-downcase. Requires (import :std/srfi/13).',
  },

  // ── R7RS specific ─────────────────────────────────────────────
  {
    pattern: /\(define-record-type\s+<(\w+)>/g,
    replacement: '(defstruct $1',
    category: 'syntax',
    warning: 'R7RS define-record-type → Gerbil defstruct. Constructor and predicate names differ.',
  },
  {
    pattern: /\(import\s+\(scheme\s+[^)]+\)\)/g,
    replacement: '; R7RS import removed — Gerbil has these built-in',
    category: 'import',
    warning: 'R7RS (import (scheme ...)) libraries are mostly built into Gerbil.',
  },
];

/** Module path mappings from Racket to Gerbil */
const MODULE_MAPPINGS: Record<string, string> = {
  'racket/list': ':std/misc/list',
  'racket/string': ':std/misc/string',
  'racket/hash': '',  // built-in
  'racket/port': ':std/misc/ports',
  'json': ':std/text/json',
  'net/url': ':std/net/request',
  'racket/match': '',  // built-in (match is core)
  'racket/format': ':std/format',
  'racket/file': '',  // various
  'srfi/1': ':std/srfi/1',
  'srfi/13': ':std/srfi/13',
  'srfi/19': ':std/srfi/19',
  'racket/set': ':std/misc/list',
  'racket/path': ':std/os/path',
  'racket/system': ':std/os',
  'racket/tcp': ':std/net/socket',
  'racket/cmdline': ':std/getopt',
  'racket/pretty': ':std/format',
  'racket/bytes': '',  // built-in
  'racket/vector': '',  // built-in
  'racket/bool': '',  // built-in
};

export function registerTranslateSchemeTool(server: McpServer): void {
  server.registerTool(
    'gerbil_translate_scheme',
    {
      title: 'Translate Scheme to Gerbil',
      description:
        'Mechanically translate R7RS or Racket Scheme code to idiomatic Gerbil. ' +
        'Handles syntax transforms (define→def, struct→defstruct), function renames ' +
        '(hash-has-key?→hash-key?, for/list→for/collect), import translation ' +
        '(require→import), and macro system differences (define-syntax-rule→defrule). ' +
        'Returns translated code plus semantic warnings where behavior differs ' +
        '(e.g. hash-ref missing-key semantics).',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        code: z
          .string()
          .describe('Scheme (R7RS or Racket) code to translate'),
        dialect: z
          .enum(['racket', 'r7rs', 'auto'])
          .optional()
          .describe('Source dialect (default: auto-detect)'),
      },
    },
    async ({ code, dialect }) => {
      const warnings: string[] = [];
      let translated = code;

      // Auto-detect dialect
      const effectiveDialect = dialect || (
        code.includes('#lang racket') || code.includes('(require ') ? 'racket' :
        code.includes('(import (scheme') || code.includes('define-record-type') ? 'r7rs' :
        'auto'
      );

      // Remove #lang line if present
      if (translated.includes('#lang ')) {
        translated = translated.replace(/^#lang\s+\S+\s*\n?/m, '');
        warnings.push('Removed #lang directive (Gerbil does not use #lang).');
      }

      // Apply translation rules
      for (const rule of TRANSLATION_RULES) {
        const before = translated;
        if (typeof rule.replacement === 'string') {
          translated = translated.replace(rule.pattern, rule.replacement);
        } else {
          translated = translated.replace(rule.pattern, rule.replacement);
        }
        if (before !== translated && rule.warning) {
          warnings.push(rule.warning);
        }
      }

      // Translate module paths in imports
      for (const [racketMod, gerbilMod] of Object.entries(MODULE_MAPPINGS)) {
        if (translated.includes(racketMod)) {
          if (gerbilMod) {
            translated = translated.replace(
              new RegExp(racketMod.replace(/\//g, '\\/'), 'g'),
              gerbilMod,
            );
            warnings.push(`Module: ${racketMod} → ${gerbilMod}`);
          } else {
            warnings.push(`Module ${racketMod} — Gerbil has this built-in, import may not be needed.`);
          }
        }
      }

      // Check for remaining Racket-isms
      if (translated.includes('(module ')) {
        warnings.push('Racket (module ...) form detected — Gerbil uses file-based modules with package.ss/gerbil.pkg.');
      }
      if (/\(match\s/.test(translated) && !translated.includes('(import') && !translated.includes('(export')) {
        warnings.push('match is built into Gerbil — no import needed. Syntax is the same as Racket match.');
      }
      if (/\(define\/contract\b/.test(translated)) {
        warnings.push('define/contract has no Gerbil equivalent — use assertions or pre/postcondition checks manually.');
      }
      if (/\(class\s/.test(translated)) {
        warnings.push('Racket class system → Gerbil uses defclass with different syntax. Manual translation needed.');
      }
      if (/\(module\+\s+test\b/.test(translated)) {
        warnings.push('Racket module+ test → Gerbil uses separate *-test.ss files with (import :std/test).');
      }
      if (/\(for\s+\(/.test(translated) && !translated.includes('for/collect') && !translated.includes('for/fold')) {
        warnings.push('Racket for loop → Gerbil for requires (import :std/iter). Syntax is similar but check iteration forms.');
      }
      if (/\(place\s/.test(translated)) {
        warnings.push('Racket places → Gerbil uses :std/actor for distributed computation.');
      }

      // Format output
      const sections: string[] = [
        `## Translated Code (${effectiveDialect} → Gerbil)\n`,
        '```scheme',
        translated,
        '```',
      ];

      if (warnings.length > 0) {
        sections.push('');
        sections.push('## Semantic Warnings\n');
        for (const w of warnings) {
          sections.push(`- ${w}`);
        }
      }

      sections.push('');
      sections.push('## Recommended Next Steps\n');
      sections.push('1. `gerbil_check_syntax` — verify the translated code parses');
      sections.push('2. `gerbil_compile_check` — catch unbound identifiers');
      sections.push('3. `gerbil_suggest_imports` — resolve any missing imports');
      sections.push('4. `gerbil_howto` — search for idiomatic Gerbil patterns');

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
