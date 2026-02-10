import { McpServer, ResourceTemplate } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { runGxi } from './gxi.js';

interface Recipe {
  id: string;
  title: string;
  tags: string[];
  imports: string[];
  code: string;
  notes?: string;
  related?: string[];
  deprecated?: boolean;
  superseded_by?: string;
  gerbil_version?: string;
  valid_for?: string[];
}

function loadCookbook(): Recipe[] {
  try {
    const thisDir = dirname(fileURLToPath(import.meta.url));
    const cookbookPath = join(thisDir, '..', 'cookbooks.json');
    const raw = readFileSync(cookbookPath, 'utf-8');
    return JSON.parse(raw) as Recipe[];
  } catch {
    return [];
  }
}

// ── Stdlib module introspection cache ──────────────────────────
const stdlibCache = new Map<string, string>();

/**
 * Introspect a Gerbil module and generate markdown reference docs.
 * Results are cached for the lifetime of the server process.
 */
async function getModuleReference(modulePath: string): Promise<string> {
  const cached = stdlibCache.get(modulePath);
  if (cached) return cached;

  const exprs = [
    '(import :gerbil/expander)',
    `(import ${modulePath})`,
    [
      `(let* ((mod (import-module (quote ${modulePath}) #t #t))`,
      '       (exports (module-context-export mod))',
      '       (names (sort! (map module-export-name exports)',
      '                      (lambda (a b) (string<? (symbol->string a) (symbol->string b))))))',
      '  (for-each (lambda (name)',
      '    (let ((resolved (with-catch (lambda (e) #f) (lambda () (eval name)))))',
      '      (display "EXPORT:")',
      '      (display name)',
      '      (display "|")',
      '      (cond',
      '        ((not resolved) (display "macro|0"))',
      '        ((procedure? resolved)',
      '         (display "procedure|")',
      '         (let ((arity (with-catch (lambda (e) -1)',
      '                        (lambda () (##procedure-length resolved)))))',
      '           (display arity)))',
      '        ((number? resolved) (display "constant|") (write resolved))',
      '        ((string? resolved) (display "constant|") (write resolved))',
      '        ((boolean? resolved) (display "constant|") (write resolved))',
      '        (else (display "value|0")))',
      '      (newline)))',
      '    names)',
      '  (display "TOTAL:")',
      '  (display (length names))',
      '  (newline))',
    ].join(' '),
  ];

  let result;
  try {
    result = await runGxi(exprs, { timeout: 15000 });
  } catch (e) {
    const msg = `Failed to introspect ${modulePath}: ${e}`;
    stdlibCache.set(modulePath, msg);
    return msg;
  }

  const output = result.stdout + result.stderr;
  const exportLines = output.split('\n').filter(l => l.startsWith('EXPORT:'));
  const total = output.match(/TOTAL:(\d+)/)?.[1] || '?';

  if (exportLines.length === 0) {
    const msg = `Could not introspect ${modulePath} (no exports found).`;
    stdlibCache.set(modulePath, msg);
    return msg;
  }

  // Parse and classify
  const procs: Array<{ name: string; arity: string }> = [];
  const macros: string[] = [];
  const constants: Array<{ name: string; value: string }> = [];
  const values: string[] = [];

  for (const line of exportLines) {
    const parts = line.slice(7).split('|');
    if (parts.length < 3) continue;
    const [name, kind, info] = parts;

    switch (kind) {
      case 'procedure':
        procs.push({ name, arity: info === '-1' ? '?' : info });
        break;
      case 'macro':
        macros.push(name);
        break;
      case 'constant':
        constants.push({ name, value: info });
        break;
      default:
        values.push(name);
    }
  }

  // Format as markdown
  const sections: string[] = [
    `# ${modulePath} — API Reference`,
    '',
    `> Auto-generated reference. Use \`gerbil_function_signature\` for keyword arg details.`,
    '',
    `**Total exports**: ${total} | Procedures: ${procs.length} | Macros: ${macros.length} | Constants: ${constants.length} | Values: ${values.length}`,
    '',
    `**Import**: \`(import ${modulePath})\``,
    '',
  ];

  if (procs.length > 0) {
    sections.push('## Procedures\n');
    sections.push('| Name | Arity |');
    sections.push('|------|-------|');
    for (const p of procs) {
      sections.push(`| \`${p.name}\` | ${p.arity} |`);
    }
    sections.push('');
  }

  if (macros.length > 0) {
    sections.push('## Macros / Syntax\n');
    for (const m of macros) {
      sections.push(`- \`${m}\``);
    }
    sections.push('');
  }

  if (constants.length > 0) {
    sections.push('## Constants\n');
    sections.push('| Name | Value |');
    sections.push('|------|-------|');
    for (const c of constants) {
      sections.push(`| \`${c.name}\` | \`${c.value}\` |`);
    }
    sections.push('');
  }

  if (values.length > 0) {
    sections.push('## Values\n');
    for (const v of values) {
      sections.push(`- \`${v}\``);
    }
    sections.push('');
  }

  const content = sections.join('\n');
  stdlibCache.set(modulePath, content);
  return content;
}

export function registerResources(server: McpServer): void {
  const recipes = loadCookbook();

  // Static resource: full cookbook index
  server.registerResource(
    'cookbook-index',
    'gerbil://cookbooks',
    {
      description: 'Index of all Gerbil cookbook recipes with id, title, and tags',
      mimeType: 'application/json',
    },
    async () => {
      const index = recipes.map(r => ({
        id: r.id,
        title: r.title,
        tags: r.tags,
        ...(r.deprecated ? { deprecated: true } : {}),
        ...(r.gerbil_version ? { gerbil_version: r.gerbil_version } : {}),
      }));
      return {
        contents: [{
          uri: 'gerbil://cookbooks',
          mimeType: 'application/json',
          text: JSON.stringify(index, null, 2),
        }],
      };
    },
  );

  // Dynamic resource: individual recipe by id
  server.registerResource(
    'cookbook-recipe',
    new ResourceTemplate('gerbil://cookbooks/{id}', { list: undefined }),
    {
      description: 'A single Gerbil cookbook recipe with full code, imports, and notes',
      mimeType: 'application/json',
    },
    async (uri, variables) => {
      const id = variables.id as string;
      const recipe = recipes.find(r => r.id === id);
      if (!recipe) {
        return {
          contents: [{
            uri: uri.href,
            mimeType: 'text/plain',
            text: `Recipe not found: ${id}`,
          }],
        };
      }
      return {
        contents: [{
          uri: uri.href,
          mimeType: 'application/json',
          text: JSON.stringify(recipe, null, 2),
        }],
      };
    },
  );

  // Static reference documentation resources
  const thisDir = dirname(fileURLToPath(import.meta.url));
  const resourcesDir = join(thisDir, 'resources');

  const REFERENCE_RESOURCES: Array<{
    name: string;
    uri: string;
    description: string;
    filename: string;
  }> = [
    {
      name: 'Gerbil Idiom Cheat Sheet',
      uri: 'gerbil://reference/idioms',
      description:
        'Core syntax differences from standard Scheme: def vs define, bracket lists, ' +
        'hash tables, keyword args, defstruct/defclass, using, chain, iteration, ' +
        'pattern matching, import/export, error handling, and common gotchas.',
      filename: 'gerbil-idioms.md',
    },
    {
      name: 'Gerbil Pattern Matching Reference',
      uri: 'gerbil://reference/pattern-matching',
      description:
        'Complete guide to match syntax: literals, binding, wildcards, list/cons ' +
        'destructuring, struct patterns, predicates (?), and/or/not patterns, ' +
        'quasiquote, ellipsis, match lambda (<>), match*, when-let, if-let.',
      filename: 'gerbil-pattern-matching.md',
    },
    {
      name: 'Gerbil Actor System Reference',
      uri: 'gerbil://reference/actors',
      description:
        'Actor-oriented programming: spawn, message passing (<-, -->, ->>, !!), ' +
        'stateful loops, request/reply, supervisor patterns, worker pools, ' +
        'actor servers, ensembles, remote handles, and Gambit threading primitives.',
      filename: 'gerbil-actors.md',
    },
    {
      name: 'Gerbil Standard Library Map',
      uri: 'gerbil://reference/stdlib-map',
      description:
        'Complete standard library overview organized by domain: text processing, ' +
        'networking, concurrency/actors, data structures, iteration, I/O, OS/system, ' +
        'database, crypto, syntax/macros, logging, testing, serialization, FFI, SRFIs.',
      filename: 'gerbil-stdlib-map.md',
    },
    {
      name: 'Gerbil-Gambit Interop Guide',
      uri: 'gerbil://reference/gambit-interop',
      description:
        'When and how to use Gambit primitives from Gerbil: ## prefix, threading ' +
        'model comparison, I/O layers, C FFI (begin-foreign, c-lambda, extern), ' +
        'declare blocks for optimization, reader extensions, and common mistakes.',
      filename: 'gerbil-gambit-interop.md',
    },
  ];

  for (const res of REFERENCE_RESOURCES) {
    const filePath = join(resourcesDir, res.filename);

    server.registerResource(
      res.name,
      res.uri,
      {
        description: res.description,
        mimeType: 'text/markdown',
      },
      async () => {
        let content: string;
        try {
          content = readFileSync(filePath, 'utf-8');
        } catch (err) {
          content = `Error reading resource file ${res.filename}: ${err}`;
        }
        return {
          contents: [{
            uri: res.uri,
            mimeType: 'text/markdown',
            text: content,
          }],
        };
      },
    );
  }

  // ── Dynamic stdlib reference resources (top 20 modules) ─────────
  // These are generated lazily via gxi introspection and cached.
  const STDLIB_MODULES: Array<{
    name: string;
    module: string;
    description: string;
  }> = [
    { name: 'std-iter', module: ':std/iter', description: 'Iteration: for, for/collect, for/fold, in-range, in-hash, in-list, in-indexed' },
    { name: 'std-sugar', module: ':std/sugar', description: 'Syntax sugar: try, catch, finally, defrule, chain, with-id, cut, is' },
    { name: 'std-text-json', module: ':std/text/json', description: 'JSON: read-json, write-json, json-object->string, json->string' },
    { name: 'std-net-request', module: ':std/net/request', description: 'HTTP: http-get, http-post, http-put, http-delete, request-text, request-json' },
    { name: 'std-misc-list', module: ':std/misc/list', description: 'List utilities: flatten, alist->hash, plist->hash, unique, group-by' },
    { name: 'std-sort', module: ':std/sort', description: 'Sorting: sort, sort!, stable-sort' },
    { name: 'std-format', module: ':std/format', description: 'String formatting: format, fprintf, printf, eprintf' },
    { name: 'std-error', module: ':std/error', description: 'Error handling: Error class, error-message, error-irritants, with-exception-catcher' },
    { name: 'std-getopt', module: ':std/getopt', description: 'CLI argument parsing: getopt, call-with-getopt, option, flag, argument' },
    { name: 'std-misc-string', module: ':std/misc/string', description: 'String utilities: string-split, string-trim, string-index' },
    { name: 'std-os-path', module: ':std/os/path', description: 'Path manipulation: path-expand, path-normalize, path-directory, path-extension' },
    { name: 'std-db-sqlite', module: ':std/db/sqlite', description: 'SQLite: sqlite-open, sqlite-close, sqlite-exec, sqlite-prepare, sqlite-step' },
    { name: 'std-net-httpd', module: ':std/net/httpd', description: 'HTTP server: start-http-server!, http-register-handler, http-response-write' },
    { name: 'std-actor', module: ':std/actor', description: 'Actors: spawn, <-, ->, !!, actor-server, start-actor-server!' },
    { name: 'std-event', module: ':std/event', description: 'Events: sync, select, choice, wrap, handle, never-evt, always-evt' },
    { name: 'std-misc-channel', module: ':std/misc/channel', description: 'Channels: make-channel, channel-put, channel-get, channel-close, channel-try-get' },
    { name: 'std-crypto', module: ':std/crypto', description: 'Cryptography: digest, hmac, encrypt, decrypt, random-bytes' },
    { name: 'std-test', module: ':std/test', description: 'Testing: test-suite, test-case, check, check-exception, run-tests!, test-report-summary!' },
    { name: 'std-pregexp', module: ':std/pregexp', description: 'Regular expressions: pregexp-match, pregexp-replace, pregexp-split' },
    { name: 'std-misc-ports', module: ':std/misc/ports', description: 'Port utilities: read-all-as-string, read-all-as-lines, read-all-as-u8vector' },
  ];

  for (const mod of STDLIB_MODULES) {
    const uri = `gerbil://reference/${mod.name}`;

    server.registerResource(
      `Gerbil ${mod.module} Reference`,
      uri,
      {
        description: `API reference for ${mod.module}: ${mod.description}`,
        mimeType: 'text/markdown',
      },
      async () => {
        const content = await getModuleReference(mod.module);
        return {
          contents: [{
            uri,
            mimeType: 'text/markdown',
            text: content,
          }],
        };
      },
    );
  }
}
