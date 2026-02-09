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
            `- Prefer \`hash\` literal syntax for hash tables\n` +
            `- Keyword arguments use trailing colon convention: \`(func name: "foo" port: 8080)\`\n\n` +
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
          `Advanced testing patterns:\n` +
          `- For async/concurrent tests: use \`(spawn ...)\` and \`(thread-sleep! ...)\` for timing\n` +
          `- For test fixtures: define setup/teardown with \`(let (...) (test-case ...))\` bindings\n` +
          `- For mocking: use \`parameterize\` to override dynamic parameters\n\n` +
          `Please first examine the module exports (use \`gerbil_module_exports\`), then generate tests covering:\n` +
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
          `- Inefficient patterns that could use \`:std/iter\` or \`:std/sugar\` idioms\n` +
          `- FFI safety: type mismatches in c-lambda, missing null checks, buffer overflow risks\n` +
          `- Macro hygiene: accidental variable capture, missing syntax-case fenders\n\n` +
          `Code:\n\`\`\`scheme\n${code}\n\`\`\`\n\n` +
          `Please review for:\n` +
          `1. Correctness issues (bugs, logic errors)\n` +
          `2. Gerbil idiom violations (non-idiomatic patterns)\n` +
          `3. Performance concerns\n` +
          `4. Error handling gaps\n` +
          `5. Style improvements\n` +
          `6. Security issues (use \`gerbil_security_scan\` for FFI/shell/I/O code)`,
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
          `**IMPORTANT**: Before writing code, use \`gerbil_howto\` to search the cookbook for relevant patterns. ` +
          `The cookbook contains verified, working examples with correct imports and arities.\n\n` +
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
          `1. Use \`gerbil_howto\` to check the cookbook for relevant patterns first\n` +
          `2. Use \`gerbil_module_exports\` to check available APIs in imported modules\n` +
          `3. Use \`gerbil_check_syntax\` to verify the code compiles\n` +
          `4. Include proper error handling\n` +
          `5. Follow Gerbil naming conventions (kebab-case for functions, CamelCase? for predicates)`,
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
            `2. Use \`gerbil_explain_error\` for automated error classification and fix suggestions\n` +
            `3. Use \`gerbil_error_hierarchy\` to understand the exception type\n` +
            `4. If it mentions a specific symbol, use \`gerbil_doc\` or \`gerbil_function_signature\` to check its API\n` +
            `5. If it's a compilation error, use \`gerbil_compile_check\` on the code\n` +
            `6. If it's a runtime error, use \`gerbil_eval\` to reproduce and isolate\n` +
            `7. Use \`gerbil_describe\` to inspect unexpected return values\n` +
            `8. Check \`gerbil_howto\` for known patterns and workarounds\n\n` +
            `Common Gerbil error patterns:\n` +
            `- "Wrong number of arguments" → check arity with \`gerbil_function_signature\`\n` +
            `- "Unbound identifier" → check imports with \`gerbil_module_exports\` or \`gerbil_suggest_imports\`\n` +
            `- "Bad binding; import conflict" → use \`gerbil_check_import_conflicts\`\n` +
            `- Segfault in compiled code → use \`gerbil_stale_static\` to check for stale artifacts\n` +
            `- Macro expansion errors → use \`gerbil_trace_macro\` to see expansion steps`,
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
            `- SRFI support via \`:std/srfi/...\` imports\n` +
            `- Quasiquote/unquote: same as standard Scheme, but watch for Gerbil bracket syntax in quasiquote\n\n` +
            `Please:\n` +
            `1. Use \`gerbil_howto\` to search the cookbook for relevant patterns\n` +
            `2. Use \`gerbil_module_exports\` to verify available APIs\n` +
            `3. Use \`gerbil_suggest_imports\` to find correct import paths\n` +
            `4. Use \`gerbil_check_syntax\` to validate the ported code\n` +
            `5. Note any features that don't have direct Gerbil equivalents`,
        },
      }],
    };
  });

  // ── New prompts ──────────────────────────────────────────────

  server.registerPrompt('optimize-gerbil-code', {
    title: 'Optimize Gerbil Code',
    description: 'Performance tuning guidance for Gerbil Scheme code',
    argsSchema: {
      code: z.string().describe('The Gerbil Scheme code to optimize'),
      bottleneck: z
        .string()
        .optional()
        .describe('Description of the performance bottleneck (if known)'),
    },
  }, async ({ code, bottleneck }) => {
    const bottleneckClause = bottleneck
      ? `\nKnown bottleneck: ${bottleneck}\n`
      : '';
    return {
      messages: [{
        role: 'user' as const,
        content: {
          type: 'text' as const,
          text:
            `Optimize the following Gerbil Scheme code for performance.${bottleneckClause}\n\n` +
            `Code:\n\`\`\`scheme\n${code}\n\`\`\`\n\n` +
            `Gerbil/Gambit optimization techniques:\n\n` +
            `**Compilation flags:**\n` +
            `- Always compile with \`-O\` flag (10-100x faster than interpreted)\n` +
            `- Use \`(declare (not safe))\` in inner loops only (disables runtime type checks)\n` +
            `- Use \`(declare (fixnum))\` for integer-only arithmetic\n` +
            `- Use \`(declare (flonum))\` for floating-point arithmetic\n\n` +
            `**Data structure choices:**\n` +
            `- Use \`u8vector\`/\`f64vector\` instead of lists for numeric data\n` +
            `- Use \`hash-table\` with \`eq?\` test for symbol keys (faster than \`equal?\`)\n` +
            `- Prefer \`for/fold\` over recursive accumulation (avoids stack pressure)\n` +
            `- Use \`string-append!\` with output ports instead of repeated \`string-append\`\n\n` +
            `**Allocation reduction:**\n` +
            `- Avoid allocation in hot loops (pre-allocate vectors, reuse buffers)\n` +
            `- Use \`begin\` to sequence side effects without creating intermediate values\n` +
            `- Use \`using\` macro for typed access to struct fields (avoids dispatch overhead)\n` +
            `- Consider \`fx+\`/\`fl+\` for fixnum/flonum arithmetic without boxing\n\n` +
            `**GC-friendly patterns:**\n` +
            `- Minimize closures over large environments\n` +
            `- Use weak references for caches\n` +
            `- Prefer tail-recursive loops (constant stack space)\n\n` +
            `**Profiling tools:**\n` +
            `- Use \`gerbil_benchmark\` to measure wall-clock/CPU/GC time\n` +
            `- Use \`gerbil_profile\` to instrument specific functions\n` +
            `- Use \`gerbil_heap_profile\` to measure memory allocation\n` +
            `- Use \`gerbil_trace_calls\` to count function invocations\n\n` +
            `Please analyze the code and suggest specific optimizations with before/after examples.`,
        },
      }],
    };
  });

  server.registerPrompt('migrate-gerbil-version', {
    title: 'Migrate Gerbil Version',
    description: 'Guide for migrating code between Gerbil versions (v0.18 to v0.19)',
    argsSchema: {
      code: z.string().describe('The Gerbil code to migrate'),
      from_version: z
        .string()
        .optional()
        .describe('Source version (default: "v0.18")'),
      to_version: z
        .string()
        .optional()
        .describe('Target version (default: "v0.19")'),
    },
  }, async ({ code, from_version, to_version }) => {
    const from = from_version ?? 'v0.18';
    const to = to_version ?? 'v0.19';
    return {
      messages: [{
        role: 'user' as const,
        content: {
          type: 'text' as const,
          text:
            `Migrate the following Gerbil Scheme code from ${from} to ${to}.\n\n` +
            `Code:\n\`\`\`scheme\n${code}\n\`\`\`\n\n` +
            `Known ${from} → ${to} breaking changes:\n\n` +
            `**Module renames:**\n` +
            `- \`:std/getopt\` → \`:std/cli/getopt\`\n` +
            `- Actor protocol modules reorganized under \`:std/actor\`\n` +
            `- Some SRFI modules moved to \`:scheme/*\` R7RS equivalents\n\n` +
            `**API changes:**\n` +
            `- \`call-with-getopt\` handler signature changed (receives options hash)\n` +
            `- \`:std/io\` buffered IO system rewritten\n` +
            `- Some \`:std/net/httpd\` handler signatures changed\n\n` +
            `**New features in ${to}:**\n` +
            `- \`:scheme/*\` R7RS module system (generators, comparators, sets, etc.)\n` +
            `- Enhanced \`:std/sugar\` macros\n` +
            `- Improved compilation and linking\n\n` +
            `**Migration workflow:**\n` +
            `1. Use \`gerbil_migration_check\` to scan for known issues\n` +
            `2. Use \`gerbil_diff_modules\` to compare old vs new module exports\n` +
            `3. Use \`gerbil_howto\` with query "v0.19" to find migration recipes\n` +
            `4. Use \`gerbil_module_exports\` to verify available functions\n` +
            `5. Use \`gerbil_compile_check\` to verify the migrated code\n\n` +
            `Please identify all changes needed and provide the migrated code.`,
        },
      }],
    };
  });

  server.registerPrompt('design-ffi-bindings', {
    title: 'Design FFI Bindings',
    description: 'Step-by-step guidance for creating safe FFI bindings from a C library',
    argsSchema: {
      header_or_api: z.string().describe('C header content, API description, or library name'),
      use_case: z
        .string()
        .optional()
        .describe('What you want to achieve with these bindings'),
    },
  }, async ({ header_or_api, use_case }) => {
    const useClause = use_case ? `\nUse case: ${use_case}\n` : '';
    return {
      messages: [{
        role: 'user' as const,
        content: {
          type: 'text' as const,
          text:
            `Design safe Gerbil FFI bindings for the following C API:${useClause}\n\n` +
            `\`\`\`c\n${header_or_api}\n\`\`\`\n\n` +
            `**FFI Design Workflow:**\n\n` +
            `1. **Generate initial bindings**: Use \`gerbil_ffi_scaffold\` on the header file\n` +
            `2. **Check type safety**: Use \`gerbil_ffi_type_check\` on the generated code\n` +
            `3. **Security scan**: Use \`gerbil_security_scan\` to check for FFI vulnerabilities\n` +
            `4. **Debug callbacks**: Use \`gerbil_ffi_callback_debug\` for c-define/extern issues\n\n` +
            `**Gerbil FFI conventions:**\n\n` +
            `- Use \`begin-ffi\` block for all FFI declarations\n` +
            `- Use \`c-define-type\` with \`(pointer type)\` for opaque pointers\n` +
            `- Use \`c-lambda\` for function bindings\n` +
            `- Wrap create/destroy pairs with \`make-will\` for automatic GC cleanup\n` +
            `- Use \`(nonnull-pointer type)\` to prevent null pointer passes\n` +
            `- Use \`extern\` declarations for callback registration\n` +
            `- Always use \`unwind-protect\` for resource cleanup in Scheme wrappers\n\n` +
            `**Memory management:**\n` +
            `- Track ownership: who allocates, who frees?\n` +
            `- Use finalizers (\`make-will\`) for C-allocated resources\n` +
            `- Never let Scheme GC a pointer that C still references\n` +
            `- Use \`u8vector\` for passing byte buffers (GC-managed, pinned during FFI call)\n\n` +
            `**Error handling:**\n` +
            `- Check return codes from all C functions\n` +
            `- Convert C error codes to Gerbil exceptions\n` +
            `- Use \`(c-lambda ... type (result ...))\` for nullable returns\n\n` +
            `**Callback safety:**\n` +
            `- Use \`c-define\` inside \`begin-foreign\` block\n` +
            `- Match \`c-define\` with \`extern\` declaration\n` +
            `- Be careful with GC during callbacks (Scheme heap may move)\n\n` +
            `Please provide the complete FFI binding module with proper error handling and resource management.`,
        },
      }],
    };
  });

  server.registerPrompt('refactor-gerbil-module', {
    title: 'Refactor Gerbil Module',
    description: 'Guidance for splitting, merging, or reorganizing Gerbil modules',
    argsSchema: {
      module_path: z.string().describe('Module path or file path to refactor'),
      goal: z.string().describe('What you want to achieve (e.g. "split into smaller modules", "extract interface", "reduce dependencies")'),
    },
  }, async ({ module_path, goal }) => ({
    messages: [{
      role: 'user' as const,
      content: {
        type: 'text' as const,
        text:
          `Refactor the Gerbil module \`${module_path}\` to ${goal}.\n\n` +
          `**Refactoring workflow:**\n\n` +
          `1. **Understand current structure**: Use \`gerbil_file_summary\` and \`gerbil_document_symbols\`\n` +
          `2. **Analyze dependencies**: Use \`gerbil_module_deps\` and \`gerbil_project_dep_graph\`\n` +
          `3. **Check exports**: Use \`gerbil_check_exports\` for consistency\n` +
          `4. **Find callers**: Use \`gerbil_find_callers\` to see who depends on each symbol\n` +
          `5. **Check for cycles**: Use \`gerbil_dependency_cycles\` to detect circular dependencies\n` +
          `6. **Rename safely**: Use \`gerbil_rename_symbol\` with dry-run to preview changes\n` +
          `7. **Verify**: Use \`gerbil_check_import_conflicts\` after restructuring\n\n` +
          `**Module design principles:**\n\n` +
          `- Each module should have a single, clear responsibility\n` +
          `- Minimize the number of exports (smaller API surface)\n` +
          `- Avoid circular dependencies between modules\n` +
          `- Use re-exports to maintain backward compatibility during transitions\n` +
          `- Keep related types and their operations together\n` +
          `- Separate pure logic from I/O and side effects\n\n` +
          `**Gerbil-specific patterns:**\n\n` +
          `- Use \`(export #t)\` during development, then narrow to specific symbols\n` +
          `- Use \`(import (only-in :module symbol1 symbol2))\` for precise imports\n` +
          `- Use \`(export (import: :sub-module))\` for re-exporting sub-module APIs\n` +
          `- Consider a \`types.ss\` module for shared type definitions\n` +
          `- Consider an \`interface.ss\` module for public API re-exports\n\n` +
          `Please analyze the module and provide a concrete refactoring plan with new module boundaries.`,
      },
    }],
  }));
}
