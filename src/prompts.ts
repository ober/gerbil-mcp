import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';

export function registerPrompts(server: McpServer): void {
  server.registerPrompt('explain-code', {
    title: 'Explain Gerbil Code',
    description: 'Explain a piece of Gerbil Scheme code',
    argsSchema: {
      code: z.string().describe('The Gerbil Scheme code to explain'),
    },
  }, async ({ code }) => ({
    messages: [{
      role: 'user' as const,
      content: {
        type: 'text' as const,
        text:
          `Explain the following Gerbil Scheme code. Gerbil is a Scheme dialect built on Gambit with unique features including: bracket syntax [x y z] for lists (not vectors), keyword arguments with trailing colons (name:), dot syntax for method calls ({obj.method args}), and a syntax-case macro system.\n\n` +
          `Code:\n\`\`\`scheme\n${code}\n\`\`\`\n\n` +
          `Please explain what this code does, identify any Gerbil-specific idioms or patterns used, and describe the overall purpose.`,
      },
    }],
  }));

  server.registerPrompt('convert-to-gerbil', {
    title: 'Convert to Gerbil',
    description: 'Convert code from another language to idiomatic Gerbil Scheme',
    argsSchema: {
      code: z.string().describe('The source code to convert'),
      source_language: z
        .string()
        .optional()
        .describe('The source language (e.g. "Python", "JavaScript", "Common Lisp"). If omitted, it will be auto-detected.'),
    },
  }, async ({ code, source_language }) => {
    const langClause = source_language
      ? `The source language is ${source_language}.`
      : 'Please auto-detect the source language.';
    return {
      messages: [{
        role: 'user' as const,
        content: {
          type: 'text' as const,
          text:
            `Convert the following code to idiomatic Gerbil Scheme. ${langClause}\n\n` +
            `Gerbil conventions to follow:\n` +
            `- Use \`def\` instead of \`define\` (supports optional/keyword args)\n` +
            `- Use bracket syntax \`[x y z]\` for list literals\n` +
            `- Keywords end with colon: \`name:\`, \`port:\`\n` +
            `- Use \`:std/iter\` for iteration (\`for\`, \`for/collect\`, \`for/fold\`)\n` +
            `- Use \`match\` for destructuring\n` +
            `- Use \`chain\` from \`:std/sugar\` for pipelines\n` +
            `- Use \`defstruct\`/\`defclass\` for data types\n` +
            `- Use \`try\`/\`catch\`/\`finally\` for error handling\n` +
            `- Prefer \`hash\` literal syntax for hash tables\n\n` +
            `Source code:\n\`\`\`\n${code}\n\`\`\`\n\n` +
            `Provide the idiomatic Gerbil translation with comments explaining significant differences.`,
        },
      }],
    };
  });

  server.registerPrompt('generate-tests', {
    title: 'Generate Tests',
    description: 'Generate tests for a Gerbil module using :std/test',
    argsSchema: {
      module_path: z
        .string()
        .describe('The module path to generate tests for (e.g. ":myapp/utils" or a file path)'),
    },
  }, async ({ module_path }) => ({
    messages: [{
      role: 'user' as const,
      content: {
        type: 'text' as const,
        text:
          `Generate comprehensive tests for the Gerbil module \`${module_path}\` using the \`:std/test\` framework.\n\n` +
          `Follow these Gerbil testing conventions:\n` +
          `- Test file: name it with \`-test.ss\` suffix (e.g., \`utils-test.ss\`)\n` +
          `- Import: \`(import :std/test ${module_path})\`\n` +
          `- Export a symbol ending in \`-test\`: \`(export utils-test)\`\n` +
          `- Structure: \`(def utils-test (test-suite "suite name" (test-case "case name" ...)))\`\n` +
          `- Check forms:\n` +
          `  - \`(check expr => expected)\` for equality\n` +
          `  - \`(check expr ? predicate)\` for predicate checks\n` +
          `  - \`(check-exception expr predicate?)\` for expected exceptions\n` +
          `  - \`(check-output (expr) "expected output")\` for output checks\n\n` +
          `Please first examine the module exports, then generate tests covering:\n` +
          `1. Normal/expected behavior for each exported function\n` +
          `2. Edge cases (empty inputs, boundary values)\n` +
          `3. Error cases (invalid inputs that should raise exceptions)\n` +
          `4. Any struct/class constructors and predicates`,
      },
    }],
  }));

  server.registerPrompt('review-code', {
    title: 'Review Gerbil Code',
    description: 'Review Gerbil Scheme code for issues and improvements',
    argsSchema: {
      code: z.string().describe('The Gerbil Scheme code to review'),
    },
  }, async ({ code }) => ({
    messages: [{
      role: 'user' as const,
      content: {
        type: 'text' as const,
        text:
          `Review the following Gerbil Scheme code for issues and suggest improvements.\n\n` +
          `Common Gerbil pitfalls to check for:\n` +
          `- Using \`define\` instead of \`def\` (loses optional/keyword arg support)\n` +
          `- Missing \`-O\` flag when compiling (10-100x slower without it)\n` +
          `- Confusing \`#f\` (false) with \`'()\` (empty list, which is truthy)\n` +
          `- Using vectors \`#(...)\` where lists \`[...]\` are intended or vice versa\n` +
          `- Unsafe \`(declare (not safe))\` usage outside performance-critical inner loops\n` +
          `- Missing error handling for I/O operations\n` +
          `- Not using \`with-destroy\` for resource cleanup\n` +
          `- Inefficient patterns that could use \`:std/iter\` or \`:std/sugar\` idioms\n\n` +
          `Code:\n\`\`\`scheme\n${code}\n\`\`\`\n\n` +
          `Please review for:\n` +
          `1. Correctness issues (bugs, logic errors)\n` +
          `2. Gerbil idiom violations (non-idiomatic patterns)\n` +
          `3. Performance concerns\n` +
          `4. Error handling gaps\n` +
          `5. Style improvements`,
      },
    }],
  }));

  server.registerPrompt('write-gerbil-module', {
    title: 'Write Gerbil Module',
    description: 'Get guidance for writing a new Gerbil Scheme module',
    argsSchema: {
      module_name: z.string().describe('The module name (e.g. "myapp/handler")'),
      purpose: z.string().describe('What the module should do'),
    },
  }, async ({ module_name, purpose }) => ({
    messages: [{
      role: 'user' as const,
      content: {
        type: 'text' as const,
        text:
          `Help me write a Gerbil Scheme module named \`${module_name}\` that ${purpose}.\n\n` +
          `Follow these Gerbil module conventions:\n` +
          `- Start with \`(import ...)\` for dependencies\n` +
          `- Use \`(export ...)\` to declare the public API\n` +
          `- Use \`def\` instead of \`define\` for function definitions\n` +
          `- Use \`defstruct\` (transparent:) for data types\n` +
          `- Use keyword arguments (trailing colon) for optional parameters\n` +
          `- Use \`:std/error\` and \`raise\` for error handling\n` +
          `- Use \`:std/iter\` for iteration patterns\n` +
          `- Use \`:std/sugar\` for convenience macros (try, chain, etc.)\n` +
          `- Use bracket syntax \`[x y z]\` for list literals\n` +
          `- Group related definitions together\n\n` +
          `Please:\n` +
          `1. Use the gerbil_module_exports tool to check available APIs in imported modules\n` +
          `2. Use gerbil_check_syntax to verify the code compiles\n` +
          `3. Include proper error handling\n` +
          `4. Follow Gerbil naming conventions (kebab-case for functions, CamelCase? for predicates)`,
      },
    }],
  }));

  server.registerPrompt('debug-gerbil-error', {
    title: 'Debug Gerbil Error',
    description: 'Get help debugging a Gerbil Scheme error',
    argsSchema: {
      error_message: z.string().describe('The error message or stack trace'),
      code: z
        .string()
        .optional()
        .describe('The code that produced the error (if available)'),
    },
  }, async ({ error_message, code }) => {
    const codeSection = code
      ? `\nCode that produced the error:\n\`\`\`scheme\n${code}\n\`\`\`\n`
      : '';
    return {
      messages: [{
        role: 'user' as const,
        content: {
          type: 'text' as const,
          text:
            `Help me debug this Gerbil Scheme error:\n\n` +
            `\`\`\`\n${error_message}\n\`\`\`\n` +
            `${codeSection}\n` +
            `Debugging workflow:\n` +
            `1. Parse the error message and identify the error type\n` +
            `2. Use gerbil_error_hierarchy to understand the exception type\n` +
            `3. If it mentions a specific symbol, use gerbil_doc or gerbil_function_signature to check its API\n` +
            `4. If it's a compilation error, use gerbil_compile_check on the code\n` +
            `5. If it's a runtime error, use gerbil_eval to reproduce and isolate\n` +
            `6. Check gerbil_howto for known patterns and workarounds\n\n` +
            `Common Gerbil error patterns:\n` +
            `- "Wrong number of arguments" → check arity with gerbil_function_signature\n` +
            `- "Unbound identifier" → check imports with gerbil_module_exports or gerbil_suggest_imports\n` +
            `- "Bad binding; import conflict" → use gerbil_check_import_conflicts\n` +
            `- Segfault in compiled code → use gerbil_stale_static to check for stale artifacts\n` +
            `- Macro expansion errors → use gerbil_trace_macro to see expansion steps`,
        },
      }],
    };
  });

  server.registerPrompt('port-to-gerbil', {
    title: 'Port to Gerbil',
    description: 'Port code from another Scheme dialect to Gerbil',
    argsSchema: {
      code: z.string().describe('The Scheme code to port'),
      source_dialect: z
        .string()
        .optional()
        .describe('The source Scheme dialect (e.g. "Racket", "Guile", "Chicken", "Chez"). Auto-detected if omitted.'),
    },
  }, async ({ code, source_dialect }) => {
    const dialectClause = source_dialect
      ? `The source dialect is ${source_dialect}.`
      : 'Please auto-detect the source Scheme dialect.';
    return {
      messages: [{
        role: 'user' as const,
        content: {
          type: 'text' as const,
          text:
            `Port the following Scheme code to idiomatic Gerbil Scheme. ${dialectClause}\n\n` +
            `Source code:\n\`\`\`scheme\n${code}\n\`\`\`\n\n` +
            `Key Gerbil differences from other Scheme dialects:\n` +
            `- Use \`def\` instead of \`define\` (supports optional args, keyword args, rest args)\n` +
            `- Brackets \`[x y z]\` create lists (not vectors as in Racket)\n` +
            `- Keywords use trailing colon: \`name:\` not \`#:name\`\n` +
            `- Hash tables: \`(hash ("key" value))\` literal syntax, \`hash-get\`/\`hash-put!\` (not \`hash-ref\`/\`hash-set!\`)\n` +
            `- Pattern matching: \`match\` (similar to Racket but with Gerbil-specific patterns)\n` +
            `- Structs: \`(defstruct name (field1 field2) transparent:)\`\n` +
            `- Classes: \`(defclass name (field1 field2) constructor: :init!)\`\n` +
            `- Module system: \`(import :std/...)\` and \`(export ...)\`\n` +
            `- Iteration: \`:std/iter\` provides \`for\`, \`for/collect\`, \`for/fold\`, \`in-list\`, \`in-range\`, etc.\n` +
            `- Error handling: \`try\`/\`catch\`/\`finally\` from \`:std/sugar\`\n` +
            `- No \`call-with-current-continuation\` by default; use Gambit's \`call/cc\` if needed\n` +
            `- SRFI support via \`:std/srfi/...\` imports\n\n` +
            `Please:\n` +
            `1. Use gerbil_module_exports to verify available APIs\n` +
            `2. Use gerbil_suggest_imports to find correct import paths\n` +
            `3. Use gerbil_check_syntax to validate the ported code\n` +
            `4. Note any features that don't have direct Gerbil equivalents`,
        },
      }],
    };
  });
}
