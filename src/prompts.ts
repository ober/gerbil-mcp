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
}
