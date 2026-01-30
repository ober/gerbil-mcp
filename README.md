# gerbil-mcp

An MCP (Model Context Protocol) server that gives AI assistants live access to a Gerbil Scheme environment. It lets Claude (or any MCP client) evaluate expressions, inspect module exports, check syntax, expand macros, compile-check code, trace macro expansion step by step, and maintain persistent REPL sessions — all against a real Gerbil runtime instead of guessing from training data.

## Prerequisites

- **Node.js** >= 18
- **Gerbil Scheme** installed with `gxi` and `gxc` available (tested with v0.19+)

## Install

```sh
git clone https://github.com/ober/gerbil-mcp.git
cd gerbil-mcp
npm install
npm run build
```

## Configure

### Claude Code

Add the MCP server using the CLI (user scope — available across all projects):

```sh
claude mcp add -s user gerbil \
  -e GERBIL_MCP_GXI_PATH=/opt/gerbil/bin/gxi \
  -- node /absolute/path/to/gerbil-mcp/dist/index.js
```

Or for a single project only:

```sh
claude mcp add -s project gerbil \
  -e GERBIL_MCP_GXI_PATH=/opt/gerbil/bin/gxi \
  -- node /absolute/path/to/gerbil-mcp/dist/index.js
```

This writes the config to `~/.claude.json` (user scope) or `.mcp.json` in the project root (project scope). Start a new Claude Code session to pick up the server.

### Other MCP clients

Any MCP-compatible client can connect using the stdio transport. The server reads JSON-RPC from stdin and writes to stdout:

```sh
node /path/to/gerbil-mcp/dist/index.js
```

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `GERBIL_MCP_GXI_PATH` | `gxi` in PATH, then `/opt/gerbil/bin/gxi` | Path to the `gxi` binary |
| `GERBIL_MCP_GXC_PATH` | `gxc` in PATH, then `/opt/gerbil/bin/gxc` | Path to the `gxc` compiler binary |
| `GERBIL_MCP_GXPKG_PATH` | `gxpkg` in PATH, then `/opt/gerbil/bin/gxpkg` | Path to the `gxpkg` package manager binary |
| `GERBIL_HOME` | `/opt/gerbil` | Gerbil installation directory (used for module listing) |

## Tools

### gerbil_eval

Evaluate a Gerbil Scheme expression and return the result.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `expression` | string | yes | Gerbil expression to evaluate |
| `imports` | string[] | no | Modules to import first |

```
expression: "(+ 1 2)"
=> Result: 3

expression: "(json-object->string (hash (\"a\" 1)))"
imports: [":std/text/json"]
=> Result: "{\"a\":1}"
```

### gerbil_module_exports

List all exported symbols from a Gerbil module.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `module_path` | string | yes | Module path (e.g. `:std/text/json`) |

```
module_path: ":std/text/json"
=> Module :std/text/json exports 21 symbol(s):
     read-json
     write-json
     json-object->string
     ...
```

### gerbil_check_syntax

Validate Gerbil code without evaluating it. Uses the Gerbil expander.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `code` | string | yes | Code to check |
| `imports` | string[] | no | Modules for macro context |

```
code: "(if #t 1 2)"
=> Syntax is valid.

code: "(if)"
=> Syntax error: Bad syntax; invalid syntax-case clause
```

### gerbil_expand_macro

Show the core-expanded form of a Gerbil expression.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `expression` | string | yes | Expression to expand |
| `imports` | string[] | no | Modules for macro definitions |

```
expression: "(when #t (displayln \"hi\"))"
=> (%#if (%#quote #t) (%#call (%#ref displayln) (%#quote "hi")) (%#quote #!void))
```

### gerbil_apropos

Search for symbols matching a pattern across all Gambit/Gerbil namespaces.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `pattern` | string | yes | Substring to match |

```
pattern: "hash-get"
=> empty namespace:
     __hash-get, hash-get
```

### gerbil_list_std_modules

List available standard library modules by scanning the Gerbil installation.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `prefix` | string | no | Filter by prefix (e.g. `std/text`) |

```
prefix: "std/text"
=> [std] (16 modules)
     :std/text/base58
     :std/text/base64
     :std/text/csv
     :std/text/json
     ...
```

### gerbil_function_signature

Get arity and type info for exported symbols in a module. Shows whether each export is a procedure (with parameter count), macro/syntax, or value.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `module_path` | string | yes | Module path (e.g. `:std/text/json`) |
| `symbol` | string | no | Specific symbol to inspect; if omitted, inspects all exports |

```
module_path: ":std/text/json"
=> :std/text/json — 21 export(s):
     read-json  procedure  arity:1  (std/text/json/util#read-json)
     write-json  procedure  arity:1  (std/text/json/util#write-json)
     JSON  macro/syntax
     ...

module_path: ":std/text/json", symbol: "read-json"
=> :std/text/json — 1 export(s):
     read-json  procedure  arity:1  (std/text/json/util#read-json)
```

### gerbil_module_deps

Show what modules a given module imports. Supports transitive dependency resolution.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `module_path` | string | yes | Module path (e.g. `:std/text/json`) |
| `transitive` | boolean | no | Recursively resolve all dependencies (default: false) |

```
module_path: ":std/text/json"
=> :std/text/json — 1 direct dependencies:
     :std/text/json/api

module_path: ":std/net/httpd", transitive: true
=> :std/net/httpd — transitive dependency tree (3 unique modules):
     :std/net/httpd
       -> :std/net/httpd/api
     :std/net/httpd/api
       -> :std/net/httpd/handler
     ...
```

### gerbil_load_file

Read a Gerbil source file and extract its top-level definitions. Parses imports, exports, and all defined symbols categorized by type. Does not execute the file.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file_path` | string | yes | Absolute path to a `.ss` or `.scm` file |

```
file_path: "/path/to/server.ss"
=> File: /path/to/server.ss

   Imports (3):
     :std/net/httpd
     :std/text/json
     :std/logger

   Definitions (4):
     start-server  (define)
     config  (defstruct)
     handle-request  (define)
     setup-routes  (define)
```

### gerbil_doc

Look up comprehensive info about a symbol: its type (procedure/macro/value), arity, qualified name, and related symbols.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `symbol` | string | yes | Symbol name to look up |
| `module_path` | string | no | Module to import for context |

```
symbol: "read-json", module_path: ":std/text/json"
=> Symbol: read-json

   Kind: procedure
   Qualified name: std/text/json/util#read-json
   Arity: 1
   Module: :std/text/json

   Related symbols:
   "std/text/json/util#" namespace:
     read-json, read-json__%, read-json__0

symbol: "when-let", module_path: ":std/sugar"
=> Symbol: when-let

   Kind: macro/syntax
   Qualified name: when-let
   Module: :std/sugar
```

### gerbil_compile_check

Run the Gerbil compiler (`gxc -S`) on code to catch compilation errors such as unbound identifiers that syntax checking alone misses. Validates the full compilation pipeline without producing C output.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `code` | string | no | Gerbil source code to check (one of `code` or `file_path` required) |
| `file_path` | string | no | Path to a `.ss` file to check |

```
code: "(import :std/text/json) (define (f x) (read-json x))"
=> Compilation check passed. Code compiled successfully (gxc -S).

code: "(import :std/text/json) (define (f x) (nonexistent-fn x))"
=> Compilation errors found:
   *** ERROR IN "<input>"@1.42-1.56
   --- Syntax Error: Reference to unbound identifier
   ... detail: nonexistent-fn
```

### gerbil_trace_macro

Show step-by-step macro expansion using `core-expand1` iteratively. Each step shows one level of desugaring, from sugar form down to core forms.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `expression` | string | yes | Expression to trace |
| `imports` | string[] | no | Modules for macro definitions |
| `max_steps` | number | no | Maximum expansion steps (default: 10) |

```
expression: "(when #t (displayln 42))"
=> Macro expansion trace for: (when #t (displayln 42))
   (4 steps)

   [input]   (when #t (displayln 42))
   [step 1]  (if #t (begin (displayln 42)) #!void)
   [step 2]  (%#if #t (begin (displayln 42)) #!void)
   [step 3]  (%#if (%#quote #t) (%#call (%#ref displayln) (%#quote 42)) (%#quote #!void))

expression: "(if-let (x 5) (displayln x) (displayln #f))"
imports: [":std/sugar"]
=> Macro expansion trace for: (if-let (x 5) (displayln x) (displayln #f))
   (2 steps)

   [input]   (if-let (x 5) (displayln x) (displayln #f))
   [step 1]  (let (test 5) (if test (let (x test) (displayln x)) (displayln #f)))
```

### gerbil_repl_session

Manage persistent Gerbil REPL sessions. State (definitions, imports, variables) persists across evaluations within a session — unlike `gerbil_eval` which is stateless.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | string | yes | `"create"`, `"eval"`, `"destroy"`, or `"list"` |
| `session_id` | string | no | Session ID (required for `eval` and `destroy`) |
| `expression` | string | no | Expression to evaluate (required for `eval`) |

```
action: "create"
=> Session created: a1b2c3d4

action: "eval", session_id: "a1b2c3d4", expression: "(define x 42)"
=> (void)

action: "eval", session_id: "a1b2c3d4", expression: "(+ x 10)"
=> 52

action: "eval", session_id: "a1b2c3d4", expression: "(import :std/text/json)"
=> (void)

action: "eval", session_id: "a1b2c3d4", expression: "(json-object->string (hash (\"x\" x)))"
=> {"x":42}

action: "list"
=> Active REPL sessions (1):
     a1b2c3d4  (age: 45s, idle: 2s)

action: "destroy", session_id: "a1b2c3d4"
=> Session "a1b2c3d4" destroyed.
```

Sessions auto-expire after 10 minutes of inactivity. Maximum 5 concurrent sessions.

### gerbil_run_tests

Run a Gerbil test file that uses `:std/test` and return structured results. The file should define test suites with `test-suite`/`test-case`/`check-equal?` etc., call `(run-tests!)` and `(test-report-summary!)`.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file_path` | string | yes | Path to a `.ss` test file that uses `:std/test` |

```
file_path: "/path/to/tests.ss"
=> Result: PASSED

   Test Summary:
     math: OK

   Checks: 3 total

   --- Full output ---
   ... check (+ 1 2) is equal? to 3
   ...
```

### gerbil_ffi_inspect

Inspect a Gerbil module's FFI (Foreign Function Interface) bindings. Classifies exports as C constants (UPPERCASE), C-style functions (underscore_names), or Gerbil wrappers. Shows values for constants and arity for functions. Optionally provide a source `file_path` to also extract `begin-foreign` and `extern` declarations.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `module_path` | string | yes | Module path to inspect (e.g. `:std/os/signal`) |
| `file_path` | string | no | Source file for deeper analysis of begin-foreign/extern blocks |

```
module_path: ":std/os/signal"
=> FFI inspection of :std/os/signal:

   C Constants (35):
     SIG_SETMASK = 3
     SIG_BLOCK = 1
     SIGTERM = 15
     SIGKILL = 9
     ...

   C-style Functions (1):
     make_sigset  arity:0

   Gerbil Wrappers (8):
     sigprocmask  procedure arity:3
     kill  procedure arity:2
     ...
```

### gerbil_class_info

Inspect a Gerbil `defclass`/`defstruct` type descriptor. Shows type name, slots, own fields, struct vs class, super type, precedence list (MRO), and constructor.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `type_name` | string | yes | Type name without `::t` suffix (e.g. `"Error"`, `"JSON"`) |
| `module_path` | string | no | Module to import to bring the type in scope |

```
type_name: "Error", module_path: ":std/error"
=> Type: Error (class)

   Slots: continuation message irritants where
   Own fields: continuation message irritants where
   Super: (none)
   Precedence: StackTrace -> Exception -> object -> t
   Constructor: :init!

type_name: "JSON", module_path: ":std/text/json"
=> Type: JSON (class)

   Slots: (none)
   Own fields: (none)
   Super: (none)
   Precedence: object -> t
   Constructor: (none)
```

### gerbil_find_definition

Find where a Gerbil symbol is defined. Returns the qualified name, module file path, source file path (if available), kind, and arity.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `symbol` | string | yes | Symbol name to look up (e.g. `"read-json"`, `"map"`) |
| `module_path` | string | no | Module to import for context (e.g. `:std/text/json`) |

```
symbol: "read-json", module_path: ":std/text/json"
=> Symbol: read-json

   Kind: procedure
   Qualified name: std/text/json/util#read-json
   Arity: 1
   Module: :std/text/json
   Module file: /opt/gerbil/.../std/text/json/util.ssi
   Source file: (not available — compiled module)
```

### gerbil_build_project

Build or clean a Gerbil project directory using gxpkg.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project_path` | string | yes | Directory containing the Gerbil project (with `gerbil.pkg`) |
| `action` | string | no | `"build"` (default) or `"clean"` |
| `release` | boolean | no | Build released (static) executables |
| `optimized` | boolean | no | Build full program optimized executables |
| `debug` | boolean | no | Build with debug symbols |

```
project_path: "/path/to/my-project", action: "build", release: true
=> Build succeeded (release).

   ... compilation output ...
```

### gerbil_package_info

List installed Gerbil packages, search the package directory, or show package metadata.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | string | yes | `"list"`, `"search"`, or `"info"` |
| `query` | string | no | Keywords for search, or package name for info (required for search/info) |

```
action: "list"
=> Installed packages (3):

     github.com/user/pkg1
     github.com/user/pkg2
     ...

action: "search", query: "json"
=> Search results for "json":

     github.com/... json processing library
     ...
```

### gerbil_format

Pretty-print/format Gerbil Scheme expressions using Gambit's `pretty-print`.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `code` | string | yes | Gerbil Scheme code to format |

```
code: "(define (f x y) (cond ((> x y) (+ x 1)) ((< x y) (- y 1)) (else 0)))"
=> (define (f x y)
     (cond ((> x y) (+ x 1))
           ((< x y) (- y 1))
           (else 0)))
```

### gerbil_benchmark

Time a Gerbil expression's execution with detailed performance statistics.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `expression` | string | yes | The Gerbil Scheme expression to benchmark |
| `imports` | string[] | no | Modules to import first |
| `iterations` | number | no | Number of times to run (default: 1) |

```
expression: "(let loop ((i 0) (s 0)) (if (< i 10000) (loop (+ i 1) (+ s i)) s))"
iterations: 3
=> Benchmark: (let loop ...)

   Iterations: 3
   Total wall time: 1.71ms
   Avg wall time: 571.6us
   User time: 1.50ms
   System time: 0.0us
   GC time: 0.0us
   GC count: 0
   Bytes allocated: 2.3 MB

   Result: 49995000
```

### gerbil_error_hierarchy

Show the full Gerbil exception/error class hierarchy as a tree.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `modules` | string[] | no | Additional modules to scan for error types beyond `:std/error` |

```
=> Gerbil Error Type Hierarchy:

   t
   \-- object
       \-- Exception
           |-- StackTrace
           |   |-- Error
           |   |   |-- ContractViolation
           |   |   |-- UnboundKeyError
           |   |   |-- IOError
           |   |   |   |-- PrematureEndOfInput
           |   |   |   \-- Closed
           |   |   |-- Timeout
           |   |   \-- ContextError
           |   \-- RuntimeException

   Precedence lists:
     Error: StackTrace -> Exception -> object -> t
     IOError: Error -> StackTrace -> Exception -> object -> t
     ...
```

## How It Works

Most tools invoke `gxi -e` as a short-lived subprocess — no persistent state between calls. Expressions are wrapped in error-handling code using `with-catch` and output structured markers (`GERBIL-MCP-RESULT:` / `GERBIL-MCP-ERROR:`) that the TypeScript layer parses.

The `gerbil_compile_check` tool uses `gxc -S` (the Gerbil compiler in expand-only mode) to catch errors beyond what the expander alone detects.

The `gerbil_repl_session` tool spawns a persistent `gxi` subprocess with piped stdin/stdout, using a sentinel-based protocol to delimit expression output across multiple evaluations.

The `gerbil_build_project` and `gerbil_package_info` tools invoke `gxpkg` as a subprocess for package management and project building.

User input is injected via `(read (open-input-string ...))` rather than string interpolation, letting Scheme's reader handle all quoting. Subprocess execution uses `execFile` (not `exec`) to avoid shell injection.

## Project Structure

```
src/
  index.ts                Server entry point, tool registration
  gxi.ts                  gxi/gxc/gxpkg subprocess wrapper, REPL session manager
  tools/
    eval.ts               gerbil_eval
    module-exports.ts     gerbil_module_exports
    check-syntax.ts       gerbil_check_syntax
    expand-macro.ts       gerbil_expand_macro
    apropos.ts            gerbil_apropos
    list-modules.ts       gerbil_list_std_modules
    function-signature.ts gerbil_function_signature
    module-deps.ts        gerbil_module_deps
    load-file.ts          gerbil_load_file
    doc.ts                gerbil_doc
    compile-check.ts      gerbil_compile_check
    trace-macro.ts        gerbil_trace_macro
    repl-session.ts       gerbil_repl_session
    run-tests.ts          gerbil_run_tests
    ffi-inspect.ts        gerbil_ffi_inspect
    class-info.ts         gerbil_class_info
    find-definition.ts  gerbil_find_definition
    build-project.ts    gerbil_build_project
    package-info.ts     gerbil_package_info
    format.ts           gerbil_format
    benchmark.ts        gerbil_benchmark
    error-hierarchy.ts  gerbil_error_hierarchy
```

## License

MIT
