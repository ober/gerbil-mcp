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
    description: 'Guide for writing a new Gerbil module following standard conventions',
    argsSchema: {
      module_name: z
        .string()
        .describe('The name of the module to create (e.g. "myapp/utils")'),
      purpose: z
        .string()
        .optional()
        .describe('Brief description of the module purpose'),
    },
  }, async ({ module_name, purpose }) => {
    const purposeClause = purpose
      ? `\n\nModule purpose: ${purpose}`
      : '';
    return {
      messages: [{
        role: 'user' as const,
        content: {
          type: 'text' as const,
          text:
            `Create a new Gerbil Scheme module \`${module_name}\` following standard Gerbil conventions.${purposeClause}\n\n` +
            `## Standard Gerbil Module Structure\n\n` +
            `1. **Package Declaration** (for application modules):\n` +
            `   \`\`\`scheme\n` +
            `   (package myapp)\n` +
            `   \`\`\`\n\n` +
            `2. **Export List** (always explicit):\n` +
            `   \`\`\`scheme\n` +
            `   (export\n` +
            `     function-name\n` +
            `     another-function\n` +
            `     #t)  ; Export all\n` +
            `   \`\`\`\n\n` +
            `3. **Import Statements**:\n` +
            `   \`\`\`scheme\n` +
            `   (import :std/sugar\n` +
            `           :std/iter\n` +
            `           :std/error\n` +
            `           (only-in :std/format format))\n` +
            `   \`\`\`\n\n` +
            `4. **Standard Conventions**:\n` +
            `   - Use \`def\` for top-level definitions (supports optional/keyword args)\n` +
            `   - Use bracket syntax \`[...]\` for list literals\n` +
            `   - Keywords end with colon: \`name:\`, \`port:\`, \`timeout:\`\n` +
            `   - Use \`defstruct\` or \`defclass\` for data types\n` +
            `   - Use \`try\`/\`catch\`/\`finally\` for error handling\n` +
            `   - Use \`with-destroy\` for resource cleanup\n\n` +
            `5. **Build System**:\n` +
            `   - Create \`gerbil.pkg\` for package metadata:\n` +
            `     \`\`\`scheme\n` +
            `     (package: myapp\n` +
            `      depend: (:std))\n` +
            `     \`\`\`\n` +
            `   - Create \`build.ss\` for build configuration:\n` +
            `     \`\`\`scheme\n` +
            `     (import :std/build-script)\n` +
            `     (defbuild-script\n` +
            `       '("myapp/module1"\n` +
            `         "myapp/module2"))\n` +
            `     \`\`\`\n\n` +
            `Please create the module file with appropriate imports, exports, and structure.`,
        },
      }],
    };
  });

  server.registerPrompt('debug-gerbil-error', {
    title: 'Debug Gerbil Error',
    description: 'Structured workflow for debugging Gerbil errors',
    argsSchema: {
      code: z
        .string()
        .describe('The Gerbil code that is producing an error'),
      error_message: z
        .string()
        .optional()
        .describe('The error message (if available)'),
    },
  }, async ({ code, error_message }) => {
    const errorClause = error_message
      ? `\n\nError message:\n\`\`\`\n${error_message}\n\`\`\`\n`
      : '';
    return {
      messages: [{
        role: 'user' as const,
        content: {
          type: 'text' as const,
          text:
            `Debug the following Gerbil Scheme code using a systematic approach.${errorClause}\n` +
            `Code:\n\`\`\`scheme\n${code}\n\`\`\`\n\n` +
            `## Debugging Workflow\n\n` +
            `Follow these steps in order:\n\n` +
            `1. **Syntax Check** - Use \`gerbil_check_syntax\` to verify the code expands correctly.\n` +
            `   - Catches: macro errors, malformed syntax, missing imports for macros\n` +
            `   - If this fails, the issue is in the macro expansion phase\n\n` +
            `2. **Expand Macros** - Use \`gerbil_expand_macro\` to see what sugar forms expand to.\n` +
            `   - Shows: what \`when\`, \`unless\`, \`match\`, \`for\`, etc. become\n` +
            `   - Helps identify: incorrect macro usage, unexpected expansions\n\n` +
            `3. **Compile Check** - Use \`gerbil_compile_check\` to catch compile-time errors.\n` +
            `   - Catches: unbound identifiers, type errors, missing imports for values\n` +
            `   - More thorough than syntax checking alone\n\n` +
            `4. **Evaluate Subexpressions** - Use \`gerbil_eval\` to test parts of the code.\n` +
            `   - Test each subexpression in isolation\n` +
            `   - Verify assumptions about function signatures and return values\n` +
            `   - Check module imports are correct\n\n` +
            `5. **Common Issues**:\n` +
            `   - **Unbound identifier**: Missing import or typo. Use \`gerbil_suggest_imports\` to find the module.\n` +
            `   - **Wrong number of arguments**: Use \`gerbil_function_signature\` to check arity.\n` +
            `   - **Import conflict**: Use \`gerbil_check_import_conflicts\` to detect clashes.\n` +
            `   - **Type mismatch**: Check function signatures with \`gerbil_doc\`.\n` +
            `   - **Macro expansion error**: Use \`gerbil_trace_macro\` for step-by-step expansion.\n\n` +
            `Please analyze the error and provide a fix.`,
        },
      }],
    };
  });

  server.registerPrompt('port-to-gerbil', {
    title: 'Port to Gerbil',
    description: 'Guide for porting code from other Scheme dialects to Gerbil',
    argsSchema: {
      code: z
        .string()
        .describe('The Scheme code to port to Gerbil'),
      source_dialect: z
        .enum(['racket', 'guile', 'chicken', 'chez', 'gambit', 'mit-scheme', 'other'])
        .describe('The source Scheme dialect'),
    },
  }, async ({ code, source_dialect }) => ({
    messages: [{
      role: 'user' as const,
      content: {
        type: 'text' as const,
        text:
          `Port the following ${source_dialect} code to Gerbil Scheme.\n\n` +
          `Source code:\n\`\`\`scheme\n${code}\n\`\`\`\n\n` +
          `## Key Differences to Address\n\n` +
          `### From Racket:\n` +
          `- Replace \`#lang racket\` with \`(import :std/...)\` statements\n` +
          `- Replace \`require\` with \`import\` (different syntax)\n` +
          `- Replace \`provide\` with \`export\`\n` +
          `- Replace \`for/list\`, \`for/fold\` with Gerbil's \`:std/iter\` equivalents\n` +
          `- Replace \`struct\` with \`defstruct\` or \`defclass\`\n` +
          `- Replace \`hash\` with Gerbil's \`hash\` literal syntax or constructors\n\n` +
          `### From Guile:\n` +
          `- Replace \`use-modules\` with \`import\`\n` +
          `- Replace Guile-specific I/O with \`:std/io\` or Gambit equivalents\n` +
          `- Replace GOOPS classes with \`defclass\`\n` +
          `- Replace Guile's FFI with Gambit FFI (\`c-lambda\`, \`c-define\`)\n\n` +
          `### From Chicken:\n` +
          `- Replace \`use\` with \`import\`\n` +
          `- Replace Chicken FFI with Gambit FFI\n` +
          `- Replace Chicken's module system with Gerbil packages\n` +
          `- Replace srfi imports with Gerbil equivalents (many SRFIs in \`:std\`)\n\n` +
          `### Common Gerbil Idioms:\n` +
          `- Use \`def\` instead of \`define\` (supports keyword arguments)\n` +
          `- Use bracket syntax \`[x y z]\` for list literals\n` +
          `- Keywords end with colon: \`name:\`, \`port:\`\n` +
          `- Use \`:std/sugar\` for \`chain\`, \`when\`, \`unless\`, \`and-let*\`, etc.\n` +
          `- Use \`:std/iter\` for \`for\`, \`for/collect\`, \`for/fold\`\n` +
          `- Use \`:std/error\` for \`try\`/\`catch\`/\`finally\`\n` +
          `- Use \`:std/format\` for \`format\`, \`printf\`\n` +
          `- Use \`:std/text/json\` for JSON parsing\n` +
          `- Use \`:std/net/httpd\` for HTTP servers\n\n` +
          `### Standard Library Mapping:\n` +
          `Check these Gerbil modules for equivalents:\n` +
          `- \`:std/sugar\` - control flow sugar\n` +
          `- \`:std/iter\` - iteration and loops\n` +
          `- \`:std/error\` - exception handling\n` +
          `- \`:std/format\` - string formatting\n` +
          `- \`:std/text/json\` - JSON\n` +
          `- \`:std/text/yaml\` - YAML\n` +
          `- \`:std/net/httpd\` - HTTP server\n` +
          `- \`:std/net/request\` - HTTP client\n` +
          `- \`:std/db/dbi\` - database interface\n` +
          `- \`:std/actor\` - actor model concurrency\n` +
          `- \`:std/coroutine\` - coroutines\n\n` +
          `Please provide the ported Gerbil version with explanatory comments.`,
      },
    }],
  }));
}
