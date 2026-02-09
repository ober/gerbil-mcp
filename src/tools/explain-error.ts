import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

interface Recipe {
  id: string;
  title: string;
  tags: string[];
  code: string;
  imports: string[];
  notes?: string;
}

/**
 * Known Gerbil/Gambit error patterns with explanations and suggested tool calls.
 */
const ERROR_PATTERNS: Array<{
  pattern: RegExp;
  type: string;
  explanation: string;
  suggestedTools: string[];
  cookbookTags: string[];
}> = [
  {
    pattern: /[Ww]rong number of arguments/i,
    type: 'Arity Error',
    explanation:
      'A function was called with the wrong number of arguments. This is very common in Gerbil ' +
      'due to niche APIs with non-obvious arities, keyword arguments, and optional parameters.',
    suggestedTools: [
      'gerbil_function_signature — check the correct arity and keyword args',
      'gerbil_module_exports — verify the function name is correct',
      'gerbil_doc — look up the symbol for detailed info',
    ],
    cookbookTags: ['arity', 'arguments', 'wrong number'],
  },
  {
    pattern: /[Uu]nbound (?:identifier|variable)|not bound/i,
    type: 'Unbound Identifier',
    explanation:
      'A symbol was used but is not defined or imported in the current scope. ' +
      'This usually means a missing import or a misspelled function name.',
    suggestedTools: [
      'gerbil_suggest_imports — find which module exports this symbol',
      'gerbil_module_exports — check what a module actually exports',
      'gerbil_resolve_imports — auto-resolve all unbound identifiers in a file',
    ],
    cookbookTags: ['unbound', 'import', 'identifier'],
  },
  {
    pattern: /[Bb]ad binding.*import conflict/i,
    type: 'Import Conflict',
    explanation:
      'A locally defined symbol conflicts with an imported module\'s export, or ' +
      'two imports export the same symbol. Gerbil does not allow ambiguous bindings.',
    suggestedTools: [
      'gerbil_check_import_conflicts — detect all conflicts before building',
      'gerbil_module_exports — check what each import provides',
    ],
    cookbookTags: ['import', 'conflict', 'binding'],
  },
  {
    pattern: /[Ss]yntax error|[Ii]ll-formed/i,
    type: 'Syntax Error',
    explanation:
      'The code has a syntax error — possibly mismatched parentheses, incorrect ' +
      'macro usage, or invalid form structure.',
    suggestedTools: [
      'gerbil_check_balance — check for mismatched delimiters',
      'gerbil_check_syntax — validate syntax with the Gerbil expander',
      'gerbil_trace_macro — debug macro expansion issues step by step',
    ],
    cookbookTags: ['syntax', 'error', 'parse'],
  },
  {
    pattern: /[Tt]ype exception|expected a |argument [0-9]+ must be/i,
    type: 'Type Error',
    explanation:
      'A function received an argument of the wrong type. Gerbil/Gambit has runtime type ' +
      'checking for primitive operations.',
    suggestedTools: [
      'gerbil_describe — inspect the actual value to see its type',
      'gerbil_function_signature — check expected parameter types',
      'gerbil_doc — look up the function\'s documentation',
    ],
    cookbookTags: ['type', 'error', 'argument'],
  },
  {
    pattern: /[Ss]egmentation fault|SIGSEGV|#!unbound/i,
    type: 'Segfault / Unbound in Compiled Code',
    explanation:
      'Segmentation faults in compiled Gerbil code are often caused by stale build artifacts ' +
      'in the global static directory shadowing the current project build, or by FFI issues.',
    suggestedTools: [
      'gerbil_stale_static — check for stale global artifacts shadowing the build',
      'gerbil_bisect_crash — binary-search to find the minimal crashing form',
      'gerbil_demangle — decode mangled C symbol names from crash backtraces',
      'gerbil_ffi_type_check — check for FFI type mismatches',
    ],
    cookbookTags: ['segfault', 'crash', 'stale', 'compiled'],
  },
  {
    pattern: /[Mm]acro expansion|[Ee]xpander|expand/i,
    type: 'Macro Expansion Error',
    explanation:
      'A macro failed to expand correctly. This can happen with incorrect macro invocation, ' +
      'wrong number of sub-forms, or hygiene issues.',
    suggestedTools: [
      'gerbil_trace_macro — see step-by-step expansion',
      'gerbil_expand_macro — see the fully expanded core form',
      'gerbil_check_syntax — validate the expression',
    ],
    cookbookTags: ['macro', 'expand', 'syntax'],
  },
  {
    pattern: /[Dd]ivision by zero|[Zz]ero divisor/i,
    type: 'Division by Zero',
    explanation: 'An arithmetic operation attempted to divide by zero.',
    suggestedTools: [
      'gerbil_eval — reproduce and isolate the issue',
      'gerbil_trace_eval — trace let bindings to find the zero value',
    ],
    cookbookTags: ['division', 'zero', 'arithmetic'],
  },
  {
    pattern: /[Ff]ile not found|[Nn]o such file|ENOENT|cannot open/i,
    type: 'File Not Found',
    explanation: 'A file or module could not be found. Check file paths and GERBIL_LOADPATH.',
    suggestedTools: [
      'gerbil_project_info — check project structure and build targets',
      'gerbil_stale_linked_pkg — check for stale linked packages',
    ],
    cookbookTags: ['file', 'path', 'loadpath'],
  },
  {
    pattern: /[Hh]eap overflow|out of memory|GC/i,
    type: 'Memory Error',
    explanation: 'The program ran out of memory or triggered excessive GC.',
    suggestedTools: [
      'gerbil_heap_profile — measure memory usage before/after',
      'gerbil_profile — identify hot functions that allocate',
    ],
    cookbookTags: ['memory', 'heap', 'gc'],
  },
];

function loadCookbookRecipes(): Recipe[] {
  try {
    const thisDir = dirname(fileURLToPath(import.meta.url));
    const cookbookPath = join(thisDir, '..', '..', 'cookbooks.json');
    const data = readFileSync(cookbookPath, 'utf-8');
    return JSON.parse(data) as Recipe[];
  } catch {
    return [];
  }
}

function searchCookbook(recipes: Recipe[], tags: string[]): Recipe[] {
  const matches: Array<{ recipe: Recipe; score: number }> = [];
  for (const recipe of recipes) {
    if ((recipe as { deprecated?: boolean }).deprecated) continue;
    let score = 0;
    const searchable = [
      ...recipe.tags,
      recipe.title.toLowerCase(),
      recipe.id.toLowerCase(),
    ].join(' ');
    for (const tag of tags) {
      if (searchable.includes(tag.toLowerCase())) {
        score++;
      }
    }
    if (score > 0) {
      matches.push({ recipe, score });
    }
  }
  matches.sort((a, b) => b.score - a.score);
  return matches.slice(0, 3).map((m) => m.recipe);
}

export function registerExplainErrorTool(server: McpServer): void {
  server.registerTool(
    'gerbil_explain_error',
    {
      title: 'Explain Gerbil Error',
      description:
        'Take a raw Gerbil/Gambit error message and return a structured explanation: ' +
        'error type, likely cause, common fix patterns from the cookbook, and suggested ' +
        'tool calls to investigate further. Automates the debugging workflow.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        error_message: z.string().describe('The error message or stack trace from Gerbil/Gambit'),
        code: z
          .string()
          .optional()
          .describe('The code that produced the error (if available)'),
      },
    },
    async ({ error_message, code }) => {
      const sections: string[] = [];

      // 1. Match error patterns
      const matched = ERROR_PATTERNS.filter((p) => p.pattern.test(error_message));

      if (matched.length === 0) {
        sections.push('## Error Type\nUnknown/unclassified error\n');
        sections.push('## Raw Error\n```\n' + error_message + '\n```\n');
        sections.push(
          '## Suggested Tools\n' +
          '- `gerbil_eval` — reproduce and isolate the issue\n' +
          '- `gerbil_compile_check` — check for compilation errors\n' +
          '- `gerbil_howto` — search cookbook for related patterns\n' +
          '- `gerbil_describe` — inspect unexpected return values\n',
        );
      } else {
        for (const match of matched) {
          sections.push(`## Error Type: ${match.type}\n`);
          sections.push(`**Explanation**: ${match.explanation}\n`);
          sections.push(
            '**Suggested Tools**:\n' +
            match.suggestedTools.map((t) => `- \`${t}\``).join('\n') +
            '\n',
          );
        }
      }

      // 2. Search cookbook for relevant recipes
      const allTags = matched.flatMap((m) => m.cookbookTags);
      // Also extract key terms from the error message itself
      const errorWords = error_message
        .toLowerCase()
        .replace(/[^a-z0-9\s-]/g, ' ')
        .split(/\s+/)
        .filter((w) => w.length > 3);
      const searchTags = [...new Set([...allTags, ...errorWords.slice(0, 5)])];

      if (searchTags.length > 0) {
        const recipes = loadCookbookRecipes();
        const relevant = searchCookbook(recipes, searchTags);
        if (relevant.length > 0) {
          sections.push(
            '## Related Cookbook Recipes\n' +
            relevant
              .map(
                (r) =>
                  `- **${r.title}** (\`${r.id}\`): ${r.notes?.slice(0, 100) || r.tags.join(', ')}`,
              )
              .join('\n') +
            '\n\nUse `gerbil_howto_get` with the recipe id to view the full code example.\n',
          );
        }
      }

      // 3. If code was provided, suggest specific checks
      if (code) {
        sections.push(
          '## Code Analysis Suggestions\n' +
          'The code that produced this error was provided. Consider:\n' +
          '- `gerbil_check_syntax` — validate syntax\n' +
          '- `gerbil_compile_check` — check for compilation issues\n' +
          '- `gerbil_lint` — run static analysis\n' +
          '- `gerbil_check_balance` — verify delimiter balance\n',
        );
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
