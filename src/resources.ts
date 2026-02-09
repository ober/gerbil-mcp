import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { ResourceTemplate } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Load cookbooks.json from project root (parent of dist)
let cookbooksData: any[] = [];
try {
  // When running from dist/resources.js, go up one level to reach project root
  const projectRoot = join(__dirname, '..');
  const cookbooksPath = join(projectRoot, 'cookbooks.json');
  const content = await readFile(cookbooksPath, 'utf-8');
  cookbooksData = JSON.parse(content);
} catch (err) {
  console.error('Warning: Could not load cookbooks.json:', err);
}

export function registerResources(server: McpServer): void {
  // Register cookbook list resource
  server.registerResource(
    'gerbil-cookbooks-index',
    'gerbil://cookbooks',
    {
      title: 'Gerbil Cookbooks Index',
      description: 'List of all available Gerbil cookbook recipes',
      mimeType: 'application/json',
    },
    async () => {
      const recipes = cookbooksData.map((recipe) => ({
        id: recipe.id,
        title: recipe.title,
        tags: recipe.tags,
      }));

      return {
        contents: [
          {
            uri: 'gerbil://cookbooks',
            mimeType: 'application/json',
            text: JSON.stringify(recipes, null, 2),
          },
        ],
      };
    }
  );

  // Register cookbook recipe template
  const cookbookTemplate = new ResourceTemplate(
    'gerbil://cookbooks/{id}',
    {
      list: async () => {
        const resources = cookbooksData.map((recipe) => ({
          uri: `gerbil://cookbooks/${recipe.id}`,
          name: recipe.title,
          description: `Cookbook recipe: ${recipe.title}`,
          mimeType: 'application/json',
        }));
        return { resources };
      },
      complete: {
        id: async () => {
          return cookbooksData.map((r) => r.id);
        },
      },
    }
  );

  server.registerResource(
    'gerbil-cookbook-recipe',
    cookbookTemplate,
    {
      title: 'Gerbil Cookbook Recipe',
      description: 'Individual cookbook recipe with code examples and notes',
      mimeType: 'application/json',
    },
    async (uri) => {
      const id = uri.pathname.replace('/cookbooks/', '');
      const recipe = cookbooksData.find((r) => r.id === id);

      if (!recipe) {
        throw new Error(`Cookbook recipe not found: ${id}`);
      }

      return {
        contents: [
          {
            uri: uri.toString(),
            mimeType: 'application/json',
            text: JSON.stringify(recipe, null, 2),
          },
        ],
      };
    }
  );

  // Register Gerbil documentation resources
  server.registerResource(
    'gerbil-docs-guide',
    'gerbil://docs/guide',
    {
      title: 'Gerbil Guide',
      description: 'Introductory guide to Gerbil Scheme',
      mimeType: 'text/markdown',
    },
    async () => {
      const guide = `# Gerbil Scheme Guide

## What is Gerbil?

Gerbil is a meta-dialect of Scheme built on top of Gambit Scheme. It provides:
- A batteries-included standard library
- A powerful macro system based on syntax-case
- Modern features like actors, coroutines, and pattern matching
- Full access to Gambit's efficient runtime

## Key Features

### Bracket Syntax
Gerbil uses \`[...]\` for list literals (not vectors):
\`\`\`scheme
(def my-list [1 2 3 4])  ; list, not vector
(def my-vector #(1 2 3 4))  ; actual vector
\`\`\`

### The \`def\` Form
Use \`def\` instead of \`define\` for better keyword argument support:
\`\`\`scheme
(def (greet name: (name "World") loud: (loud #f))
  (let ((msg (string-append "Hello, " name "!")))
    (if loud
        (string-upcase msg)
        msg)))

(greet)  ; "Hello, World!"
(greet name: "Alice")  ; "Hello, Alice!"
(greet name: "Bob" loud: #t)  ; "HELLO, BOB!"
\`\`\`

### Keywords
Keywords end with a colon:
\`\`\`scheme
name:  ; keyword
port: 8080  ; keyword argument
timeout: 30
\`\`\`

### Error Handling
Use \`try\`/\`catch\`/\`finally\`:
\`\`\`scheme
(import :std/error)

(try
  (risky-operation)
  (catch (e)
    (displayln "Error: " (error-message e)))
  (finally
    (cleanup)))
\`\`\`

### Iteration
Use \`:std/iter\` for functional iteration:
\`\`\`scheme
(import :std/iter)

(for (x [1 2 3 4])
  (displayln x))

(def squares (for/collect (x [1 2 3 4]) (* x x)))
; => [1 4 9 16]

(def sum (for/fold (acc 0) (x [1 2 3 4]) (+ acc x)))
; => 10
\`\`\`

### Pattern Matching
\`\`\`scheme
(match value
  ([x y z] (displayln "Three element list"))
  ((cons head tail) (displayln "Pair"))
  (#t (displayln "Literal true"))
  (_ (displayln "Anything else")))
\`\`\`

### Data Types
\`\`\`scheme
(defstruct point (x y))

(def p (make-point 10 20))
(point-x p)  ; => 10

(defclass person (name age)
  constructor: :init!)

(def alice (make-person "Alice" 30))
\`\`\`

## Standard Library Modules

- \`:std/sugar\` - Control flow sugar (\`when\`, \`unless\`, \`chain\`, etc.)
- \`:std/iter\` - Iteration (\`for\`, \`for/collect\`, \`for/fold\`)
- \`:std/error\` - Exception handling (\`try\`, \`catch\`, \`raise\`)
- \`:std/format\` - String formatting
- \`:std/text/json\` - JSON parsing and generation
- \`:std/net/httpd\` - HTTP server
- \`:std/net/request\` - HTTP client
- \`:std/actor\` - Actor model concurrency
- \`:std/coroutine\` - Coroutines

## Module Structure

\`\`\`scheme
(package myapp)  ; Optional for application code

(export
  my-function
  MyStruct
  my-constant)

(import :std/sugar
        :std/iter
        (only-in :std/format format))

(def my-constant 42)

(defstruct MyStruct (field1 field2))

(def (my-function arg1 arg2)
  ...)
\`\`\`

## Building Projects

Create \`gerbil.pkg\`:
\`\`\`scheme
(package: myapp
 depend: (:std))
\`\`\`

Create \`build.ss\`:
\`\`\`scheme
(import :std/build-script)

(defbuild-script
  '("myapp/module1"
    "myapp/module2"))
\`\`\`

Build with:
\`\`\`bash
gerbil build -O  # -O flag is critical for performance!
\`\`\`

## Common Pitfalls

1. **Forgetting \`-O\` flag**: Code runs 10-100x slower without optimization
2. **Using \`define\` instead of \`def\`**: Loses keyword argument support
3. **Confusing \`#f\` and \`'()\`**: Empty list is truthy in Gerbil
4. **Brackets vs vectors**: \`[...]\` are lists, \`#(...)\` are vectors
5. **Missing imports**: Many features require explicit imports

## Learning More

- Use the MCP tools to explore the standard library
- Check the cookbooks for common patterns
- Read the official docs at https://cons.io
`;

      return {
        contents: [
          {
            uri: 'gerbil://docs/guide',
            mimeType: 'text/markdown',
            text: guide,
          },
        ],
      };
    }
  );

  server.registerResource(
    'gerbil-docs-syntax',
    'gerbil://docs/syntax',
    {
      title: 'Gerbil Syntax Reference',
      description: 'Reference for Gerbil-specific syntax and forms',
      mimeType: 'text/markdown',
    },
    async () => {
      const syntax = `# Gerbil Syntax Reference

## Core Forms

### \`def\` - Enhanced Definition
\`\`\`scheme
(def name value)  ; Simple binding
(def (func arg1 arg2) body ...)  ; Function
(def (func arg1 opt: (opt default)) body ...)  ; Optional args
(def (func arg1 key: (key default)) body ...)  ; Keyword args
\`\`\`

### \`defstruct\` - Structure Types
\`\`\`scheme
(defstruct name (field1 field2 ...))
(defstruct point (x y) constructor: :init!)
\`\`\`
Generates:
- Constructor: \`make-name\`
- Predicate: \`name?\`
- Accessors: \`name-field1\`, \`name-field2\`
- Mutators: \`name-field1-set!\`, \`name-field2-set!\`

### \`defclass\` - Object-Oriented Types
\`\`\`scheme
(defclass name (super ...) (field1 field2 ...)
  constructor: method
  final: #t)
\`\`\`

### \`match\` - Pattern Matching
\`\`\`scheme
(match expr
  (pattern body ...)
  ...)

Patterns:
- Literal: 42, "hello", #t, #f
- Variable: x, name
- List: [a b c], [head . tail]
- Cons: (cons a b)
- Vector: #(a b c)
- Struct: (point x y)
- Quote: 'symbol
- Or: (or pat1 pat2)
- And: (and pat1 pat2)
- Not: (not pat)
- Wildcard: _
- Guard: (? predicate)
- Binding: (= expr pat)
\`\`\`

### \`defrule\` - Macro Rules
\`\`\`scheme
(defrule (name pattern ...) template ...)
\`\`\`

### \`defsyntax\` - Syntax Transformers
\`\`\`scheme
(defsyntax name
  (lambda (stx)
    ...))
\`\`\`

## Control Flow (\`:std/sugar\`)

### \`when\` / \`unless\`
\`\`\`scheme
(when condition body ...)
(unless condition body ...)
\`\`\`

### \`and-let*\` - Sequential Binding with Short-Circuit
\`\`\`scheme
(and-let* ((var1 expr1)
           (var2 expr2)
           ...)
  body ...)
\`\`\`

### \`cond\`
\`\`\`scheme
(cond
  (test1 result1 ...)
  (test2 result2 ...)
  (else default ...))
\`\`\`

### \`case\`
\`\`\`scheme
(case expr
  ((val1 val2) result1 ...)
  ((val3) result2 ...)
  (else default ...))
\`\`\`

### \`chain\` - Threading Macro
\`\`\`scheme
(chain initial-value
  (func1 arg ...)
  (func2 arg ...)
  ...)

; Equivalent to:
(func2 (func1 initial-value arg ...) arg ...)
\`\`\`

## Iteration (\`:std/iter\`)

### \`for\` - Imperative Loop
\`\`\`scheme
(for (x list) body ...)
(for ((x list) (y other-list)) body ...)
\`\`\`

### \`for/collect\` - Build List
\`\`\`scheme
(for/collect (x list) expr)
; => [expr1 expr2 ...]
\`\`\`

### \`for/fold\` - Reduce/Accumulate
\`\`\`scheme
(for/fold (acc init) (x list) body ...)
\`\`\`

## Error Handling (\`:std/error\`)

### \`try\` / \`catch\` / \`finally\`
\`\`\`scheme
(try
  body ...
  (catch (e) handler ...)
  (finally cleanup ...))
\`\`\`

### \`with-catch\`
\`\`\`scheme
(with-catch
  (lambda (e) handler ...)
  (lambda () body ...))
\`\`\`

### \`raise\` - Throw Exception
\`\`\`scheme
(raise (make-error "message"))
\`\`\`

### \`deferror-class\` - Define Error Type
\`\`\`scheme
(deferror-class MyError (BaseError))
\`\`\`

## Resource Management

### \`with-destroy\` - RAII Pattern
\`\`\`scheme
(with-destroy resource-expr
  (lambda (res) body ...))
\`\`\`

### \`unwind-protect\` - Ensure Cleanup
\`\`\`scheme
(unwind-protect
  (lambda () body ...)
  (lambda () cleanup ...))
\`\`\`

## Hash Tables

### Hash Literal Syntax
\`\`\`scheme
(hash ("key1" value1) ("key2" value2))
; or with explicit equality:
(hash-eq (key1 value1) (key2 value2))
(hash-eqv (key1 value1) (key2 value2))
\`\`\`

### Hash Operations
\`\`\`scheme
(hash-ref ht key [default])
(hash-put! ht key value)
(hash-remove! ht key)
(hash-keys ht)
(hash-values ht)
(hash->list ht)
\`\`\`

## Module System

### Package Declaration
\`\`\`scheme
(package myapp)
\`\`\`

### Exports
\`\`\`scheme
(export symbol1 symbol2 ...)
(export #t)  ; Export all
(export (rename old-name new-name))
\`\`\`

### Imports
\`\`\`scheme
(import :std/sugar)
(import (only-in :std/format format printf))
(import (except-in :std/iter for))
(import (rename-in :std/sugar (when if-true)))
\`\`\`

## FFI (Foreign Function Interface)

### \`c-lambda\` - Define Foreign Function
\`\`\`scheme
(c-lambda (type ...) return-type "C-code")

Example:
(def strlen (c-lambda (char-string) int "strlen"))
\`\`\`

### \`c-define\` - Export to C
\`\`\`scheme
(c-define (scheme-func args ...) (arg-types ...) return-type "c_name" body ...)
\`\`\`

### \`c-define-type\` - Foreign Type
\`\`\`scheme
(c-define-type type-name "c-type-name")
\`\`\`

## Special Syntax

### Reader Syntax
- \`#t\` / \`#f\` - Boolean true/false
- \`#!void\` - Void value
- \`#u8(...)\` - Byte vector
- \`#\\c\` - Character
- \`[...]\` - List literal
- \`#(...)\` - Vector literal
- \`'expr\` - Quote
- \`\\\`expr\` - Quasiquote
- \`,expr\` - Unquote
- \`,@expr\` - Unquote-splicing

### Keywords
\`\`\`scheme
name:  ; keyword
(func key: value)  ; keyword argument
\`\`\`
`;

      return {
        contents: [
          {
            uri: 'gerbil://docs/syntax',
            mimeType: 'text/markdown',
            text: syntax,
          },
        ],
      };
    }
  );

  // Register module documentation template
  const moduleDocsTemplate = new ResourceTemplate(
    'gerbil://docs/module/{module_path}',
    {
      list: undefined, // No listing - too many modules
    }
  );

  server.registerResource(
    'gerbil-docs-module',
    moduleDocsTemplate,
    {
      title: 'Gerbil Module Documentation',
      description: 'Documentation for a specific Gerbil module',
      mimeType: 'text/markdown',
    },
    async (uri) => {
      const modulePath = uri.pathname.replace('/docs/module/', '');
      
      // For now, provide a basic template
      // In a full implementation, this could introspect the module
      const docs = `# Module: ${modulePath}

Use the MCP tools to explore this module:
- \`gerbil_module_exports\` - List all exported symbols
- \`gerbil_module_deps\` - Show module dependencies
- \`gerbil_doc\` - Get documentation for specific symbols
- \`gerbil_function_signature\` - Check function signatures

Example:
\`\`\`javascript
{
  "name": "gerbil_module_exports",
  "arguments": {
    "module_path": "${modulePath}"
  }
}
\`\`\`
`;

      return {
        contents: [
          {
            uri: uri.toString(),
            mimeType: 'text/markdown',
            text: docs,
          },
        ],
      };
    }
  );
}
