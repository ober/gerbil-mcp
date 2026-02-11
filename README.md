# gerbil-mcp

An MCP (Model Context Protocol) server that gives AI assistants live access to a Gerbil Scheme environment. It lets Claude (or any MCP client) evaluate expressions, inspect module exports, check syntax, expand macros, compile-check code, trace macro expansion step by step, maintain persistent REPL sessions, profile function performance, analyze heap memory usage, trace call counts, visualize call graphs, manage packages, scaffold projects and test files, generate module stubs, build projects with structured diagnostics, find symbol references, and suggest imports — all against a real Gerbil runtime instead of guessing from training data.

The server also provides MCP **resources** for browsing the cookbook recipe library, **prompts** for common Gerbil workflows, and **tool annotations** (`readOnlyHint`/`idempotentHint`) so clients can make informed decisions about tool safety.

## Prerequisites

- **Node.js** >= 18
- **Gerbil Scheme** installed with `gxi`, `gxc`, and `gxpkg` available (tested with v0.19+)

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

#### Auto-approve all Gerbil tools

By default Claude Code asks for confirmation each time an MCP tool is called. Since the Gerbil tools are read-only introspection (no filesystem writes, no network access), you can safely auto-approve them all with a wildcard in your project's `.claude/settings.local.json` (local, not checked in) or `~/.claude/settings.json` (user scope):

```json
{
  "permissions": {
    "allow": [
      "mcp__gerbil__*"
    ]
  }
}
```

### Gemini CLI

Add the MCP server using the CLI (user scope — recommended):

```sh
gemini mcp add gerbil node /absolute/path/to/gerbil-mcp/dist/index.js \
  -e GERBIL_MCP_GXI_PATH=/opt/gerbil/bin/gxi \
  --scope user --trust
```

The `--trust` flag auto-approves all tool calls from this server, which is recommended for the Gerbil introspection tools to provide a seamless experience.

You can also use the Makefile to automate this:

```sh
make update
```

### GitHub Copilot (VS Code)

Add a `.vscode/mcp.json` file to your project:

```json
{
  "servers": {
    "gerbil": {
      "type": "stdio",
      "command": "node",
      "args": ["/absolute/path/to/gerbil-mcp/dist/index.js"],
      "env": {
        "GERBIL_MCP_GXI_PATH": "/opt/gerbil/bin/gxi"
      }
    }
  }
}
```

Or add it to your VS Code user settings (`settings.json`) to make it available across all projects:

```json
{
  "mcp": {
    "servers": {
      "gerbil": {
        "type": "stdio",
        "command": "node",
        "args": ["/absolute/path/to/gerbil-mcp/dist/index.js"],
        "env": {
          "GERBIL_MCP_GXI_PATH": "/opt/gerbil/bin/gxi"
        }
      }
    }
  }
}
```

### GitHub Copilot Coding Agent

The [Copilot coding agent](https://docs.github.com/copilot/how-tos/agents/copilot-coding-agent/extending-copilot-coding-agent-with-mcp) runs in a GitHub Actions environment. Configure MCP servers in your repository settings on GitHub.com under **Settings → Copilot → Coding agent**, using this JSON:

```json
{
  "mcpServers": {
    "gerbil": {
      "type": "stdio",
      "command": "node",
      "args": ["/home/runner/gerbil-mcp/dist/index.js"],
      "tools": ["*"],
      "env": {
        "GERBIL_MCP_GXI_PATH": "$COPILOT_MCP_GERBIL_GXI_PATH"
      }
    }
  }
}
```

Since the agent runs on a GitHub Actions runner, Gerbil and this server must be installed during setup. Create `.github/workflows/copilot-setup-steps.yml`:

```yaml
name: "Copilot Setup Steps"
on: workflow_dispatch

jobs:
  copilot-setup-steps:
    runs-on: ubuntu-latest
    permissions:
      contents: read
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: "20"

      # Install Gerbil — adjust to your preferred method
      - name: Install Gerbil
        run: |
          # Example: install from a release or package manager
          # See https://cons.io/guide/install.html
          sudo apt-get update && sudo apt-get install -y gerbil

      # Clone and build gerbil-mcp
      - name: Setup gerbil-mcp
        run: |
          git clone https://github.com/ober/gerbil-mcp.git /home/runner/gerbil-mcp
          cd /home/runner/gerbil-mcp
          npm install
          npm run build
```

Add a `COPILOT_MCP_GERBIL_GXI_PATH` secret in your repository's `copilot` environment (e.g. `/usr/bin/gxi`) pointing to the `gxi` binary.

### GitHub Copilot CLI

Add the server to `~/.copilot/mcp-config.json`:

```json
{
  "mcpServers": {
    "gerbil": {
      "type": "local",
      "command": "node",
      "args": ["/absolute/path/to/gerbil-mcp/dist/index.js"],
      "env": {
        "GERBIL_MCP_GXI_PATH": "/opt/gerbil/bin/gxi"
      },
      "tools": ["*"]
    }
  }
}
```

To auto-load Gerbil-specific instructions for the CLI in your project, copy the example file to `.github/copilot-instructions.md`:

```sh
mkdir -p .github
cp /path/to/gerbil-mcp/copilot-instructions.md.gerbil-example .github/copilot-instructions.md
```

Or for global instructions across all projects, copy to `~/.copilot-instructions.md`:

```sh
cp /path/to/gerbil-mcp/copilot-instructions.md.gerbil-example ~/.copilot-instructions.md
```

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
| `GERBIL_MCP_GERBIL_PATH` | `gerbil` in PATH, then `/opt/gerbil/bin/gerbil` | Path to the `gerbil` CLI binary (used for project-wide testing) |
| `GERBIL_HOME` | `/opt/gerbil` | Gerbil installation directory (used for module listing) |

## Tools

### gerbil_eval

Evaluate a Gerbil Scheme expression and return the result.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `expression` | string | yes | Gerbil expression to evaluate |
| `imports` | string[] | no | Modules to import first |
| `loadpath` | string[] | no | Directories to add to `GERBIL_LOADPATH` for project-local module resolution |

```
expression: "(+ 1 2)"
=> Result: 3

expression: "(json-object->string (hash (\"a\" 1)))"
imports: [":std/text/json"]
=> Result: "{\"a\":1}"

expression: "(config-omit-events (load-config))"
imports: [":myproject/config"]
loadpath: ["/path/to/myproject/.gerbil/lib"]
=> Result: ("event-a" "event-b")
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

Run the Gerbil compiler (`gxc -S`) on code to catch compilation errors such as unbound identifiers that syntax checking alone misses. Validates the full compilation pipeline without producing C output. Enhanced error messages detect known compiler-internal crashes and provide diagnostic hints.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `code` | string | no | Gerbil source code to check (one of `code` or `file_path` required) |
| `file_path` | string | no | Path to a `.ss` file to check |
| `loadpath` | string[] | no | Directories to add to `GERBIL_LOADPATH` for project-local module resolution |

```
code: "(import :std/text/json) (define (f x) (read-json x))"
=> Compilation check passed. Code compiled successfully (gxc -S).

code: "(import :std/text/json) (define (f x) (nonexistent-fn x))"
=> Compilation errors found:
   *** ERROR IN "<input>"@1.42-1.56
   --- Syntax Error: Reference to unbound identifier
   ... detail: nonexistent-fn

file_path: "/path/to/project/config.ss"
loadpath: ["/path/to/project/.gerbil/lib"]
=> Compilation check passed. /path/to/project/config.ss compiled successfully (gxc -S).
```

When gxc hits internal errors (e.g., `stx-car-e`, code generation crashes), the tool appends a hint explaining the likely cause and suggesting workarounds.

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
| `loadpath` | string[] | no | Directories to add to `GERBIL_LOADPATH` (used with `create`) |
| `project_path` | string | no | Project directory for auto-configuring `GERBIL_LOADPATH` from `.gerbil/lib` (used with `create`) |
| `preload_file` | string | no | Path to a `.ss` file whose imports will be loaded into the session (used with `create`) |

```
action: "create"
=> Session created: a1b2c3d4

action: "create", project_path: "/path/to/myproject"
=> Session created: b2c3d4e5
   GERBIL_LOADPATH configured with:
     /path/to/myproject/.gerbil/lib
   Project package: myproject

action: "create", preload_file: "/path/to/myproject/server.ss"
=> Session created: d4e5f6g7

   Preloaded imports from /path/to/myproject/server.ss:
   Loaded (3):
     (import :std/net/httpd)
     (import :std/text/json)
     (import :std/logger)

action: "eval", session_id: "a1b2c3d4", expression: "(define x 42)"
=> (void)

action: "eval", session_id: "a1b2c3d4", expression: "(+ x 10)"
=> 52

action: "list"
=> Active REPL sessions (1):
     a1b2c3d4  (age: 45s, idle: 2s)

action: "destroy", session_id: "a1b2c3d4"
=> Session "a1b2c3d4" destroyed.
```

Sessions auto-expire after 10 minutes of inactivity. Maximum 5 concurrent sessions.

When `project_path` is provided, the session automatically adds `{project_path}/.gerbil/lib` to the load path and reads `gerbil.pkg` for informational display. This lets you interactively import and test project-local modules without fighting import paths.

When `preload_file` is provided, the session reads the file's import forms and evaluates them after creation. This lets you immediately use functions from the file's imported modules without manually importing each one — useful for debugging test files or exploring a module's context.

### gerbil_run_tests

Run Gerbil tests and return structured results. Supports two modes: **single-file mode** (`file_path`) runs a single test file via `gxi`, and **directory mode** (`directory`) runs project-wide tests via `gerbil test` with recursive discovery.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file_path` | string | no | Path to a `.ss` test file (single-file mode). Cannot be used with `directory`. |
| `directory` | string | no | Project directory to run tests in (project-wide mode). Uses `gerbil test <dir>/...`. Cannot be used with `file_path`. |
| `filter` | string | no | Regex pattern to filter test names (directory mode only, maps to `-r` flag) |
| `quiet` | boolean | no | Quiet mode: only show errors (directory mode only, maps to `-q` flag) |
| `timeout` | number | no | Timeout in milliseconds (default: 30000 for file, 120000 for directory) |

One of `file_path` or `directory` is required.

```
file_path: "/path/to/tests.ss"
=> Result: PASSED

   Test Summary:
     math: OK

   Checks: 3 total

   --- Full output ---
   ... check (+ 1 2) is equal? to 3
   ...

directory: "/path/to/project"
=> Result: PASSED

   Test Summary:
     billing: OK
     detection: OK

   Checks: 24 total

   --- Full output ---
   ...

directory: "/path/to/project", filter: "billing"
=> Result: PASSED

   Filter: billing

   Test Summary:
     billing: OK

   Checks: 12 total
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

Find where a Gerbil symbol is defined. Returns the qualified name, module file path, source file path (if available), kind, and arity. Optionally includes a source code preview of the definition.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `symbol` | string | yes | Symbol name to look up (e.g. `"read-json"`, `"map"`) |
| `module_path` | string | no | Module to import for context (e.g. `:std/text/json`) |
| `source_preview` | boolean | no | If true, include a source code preview of the definition |
| `preview_lines` | number | no | Maximum number of source lines to show in preview (default: 30) |

```
symbol: "read-json", module_path: ":std/text/json"
=> Symbol: read-json

   Kind: procedure
   Qualified name: std/text/json/util#read-json
   Arity: 1
   Module: :std/text/json
   Module file: /opt/gerbil/.../std/text/json/util.ssi
   Source file: (not available — compiled module)

symbol: "read-json", module_path: ":std/text/json", source_preview: true
=> Symbol: read-json
   ...
   Source preview:
   ```scheme
   (def (read-json port)
     ...)
   ```
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

### gerbil_version

Report Gerbil and Gambit versions, installation path, and system type. No parameters required.

```
=> Gerbil Environment

   Gerbil version: v0.19-dev
   Gambit version: v4.9.7-6-g64f4d369
   Gerbil home: /opt/gerbil/v0.19-dev/
   System type: (os . "linux")
```

### gerbil_scaffold

Create a new Gerbil project from a template using `gxpkg new`. Generates `gerbil.pkg`, `build.ss`, and initial source files.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project_path` | string | yes | Directory to create the project in (must already exist) |
| `package` | string | no | Package prefix (default: current username) |
| `name` | string | no | Package name (default: directory name) |
| `link` | string | no | Public package link (e.g. `"github.com/user/project"`) |

```
project_path: "/tmp/my-project", package: "myapp", link: "github.com/user/my-project"
=> Project scaffolded in /tmp/my-project.
```

### gerbil_package_manage

Install, update, or uninstall Gerbil packages.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `action` | string | yes | `"install"`, `"update"`, or `"uninstall"` |
| `package` | string | yes | Package name, optionally with `@tag`. Use `"all"` with update. |
| `global_env` | boolean | no | Use global environment even in local package context |
| `force` | boolean | no | Force the action (only applies to uninstall) |
| `cwd` | string | no | Working directory for local package context |

```
action: "install", package: "github.com/mighty-gerbils/gerbil-crypto"
=> Package github.com/mighty-gerbils/gerbil-crypto installed successfully.

action: "update", package: "all"
=> Package all updated successfully.

action: "uninstall", package: "github.com/mighty-gerbils/gerbil-crypto", force: true
=> Package github.com/mighty-gerbils/gerbil-crypto uninstalled successfully.
```

### gerbil_find_callers

Find all files that reference a given symbol. Recursively scans `.ss` files in a directory for occurrences and reports file paths with line numbers.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `symbol` | string | yes | Symbol name to find usages of |
| `directory` | string | yes | Directory to search in (absolute path) |
| `module_path` | string | no | Module the symbol comes from, for import verification |

```
symbol: "read-json", directory: "/path/to/project"
=> References to "read-json" (2 files):

     /path/to/project/server.ss
       lines: 15,28
     /path/to/project/api.ss
       lines: 7

symbol: "read-json", directory: "/path/to/project", module_path: ":std/text/json"
=> References to "read-json" (1 file):

     /path/to/project/server.ss
       lines: 15,28
```

### gerbil_suggest_imports

Find which standard library module exports a given symbol. Scans common `:std/*` modules and reports matching import statements.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `symbol` | string | yes | Symbol to find the import for |

```
symbol: "read-json"
=> Symbol "read-json" is exported by:

     (import :std/text/json)

symbol: "for/collect"
=> Symbol "for/collect" is exported by:

     (import :std/iter)
```

For less common modules, use `gerbil_apropos` + `gerbil_module_exports` as a fallback.

### gerbil_diagnostics

Run `gxc -S` on a file or all `.ss` files in a project and return structured diagnostics with file, line, column, severity, and message.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file_path` | string | no | Path to a single `.ss` file to check |
| `project_path` | string | no | Path to a project directory (checks files from `build.ss` or scans directory) |

```
file_path: "/path/to/module.ss"
=> Compiled cleanly: /path/to/module.ss

project_path: "/path/to/project"
=> Diagnostics for /path/to/project:

   /path/to/project/server.ss:12:5: error: Reference to unbound identifier: foo
   /path/to/project/utils.ss: compiled cleanly
```

### gerbil_document_symbols

List all definitions in a Gerbil source file with name, kind, and line number.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file_path` | string | yes | Absolute path to a Gerbil source file (`.ss` or `.scm`) |

```
file_path: "/path/to/server.ss"
=> Symbols in /path/to/server.ss (5):

     L1   import     (import :std/net/httpd ...)
     L4   procedure  start-server
     L8   struct     config
     L12  procedure  handle-request
     L20  procedure  setup-routes
```

### gerbil_workspace_symbols

Search for symbol definitions across all `.ss` files in a project directory. Returns matching definitions with name, kind, line number, and file path.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `query` | string | yes | Search query (substring match, case-insensitive) |
| `directory` | string | no | Directory to search in (defaults to current working directory) |

```
query: "handle", directory: "/path/to/project"
=> Workspace symbols matching "handle" (3 results):

     handle-request  procedure  L12  server.ss
     handle-error    procedure  L5   errors.ss
     handle-auth     procedure  L18  auth.ss
```

### gerbil_rename_symbol

Rename a symbol across all `.ss` files in a project directory. Default is dry-run mode (showing proposed changes). Set `dry_run` to `false` to apply changes. Uses word-boundary detection to avoid renaming partial matches.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `old_name` | string | yes | Current symbol name to rename |
| `new_name` | string | yes | New symbol name |
| `directory` | string | yes | Project directory to search (absolute path) |
| `dry_run` | boolean | no | If true (default), only show changes without applying them |

```
old_name: "handle-request", new_name: "process-request", directory: "/path/to/project"
=> Dry run: rename "handle-request" → "process-request"

   server.ss:
     L12: (def (handle-request ...) → (def (process-request ...)
     L45: (handle-request ctx) → (process-request ctx)
   api.ss:
     L7: (export handle-request) → (export process-request)

   3 occurrences in 2 files.
```

### gerbil_lint

Static analysis for common Gerbil issues: unused imports, duplicate definitions, style warnings (`define` vs `def`, missing `transparent:`), shadowed standard bindings, hash literal symbol key warnings, and compilation errors via `gxc`.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file_path` | string | yes | Absolute path to a Gerbil source file to lint |

```
file_path: "/path/to/module.ss"
=> Lint: /path/to/module.ss
     1 error(s), 2 warning(s), 1 info

     [WARNING] L3 (possibly-unused-import): Import :std/format may be unused
     [WARNING] L10 (hash-symbol-key): Hash literal uses bare symbol key 'CRITICAL' — this creates a symbol key, not a string. Use ("CRITICAL" ...) for string keys.
     [INFO] L5 (style-prefer-def): Prefer "def" over "define" (supports optional/keyword args)
```

### gerbil_project_info

Single-call summary of a Gerbil project: package name, build targets, source files, and external dependencies. Reads `gerbil.pkg`, `build.ss`, and scans source files.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project_path` | string | yes | Directory containing the Gerbil project (with `gerbil.pkg`) |

```
project_path: "/path/to/myproject"
=> Package: myproject
   Location: /path/to/myproject

   Build Targets:
     [lib] config
     [lib] server
     [exe] main -> myapp

   Source Files (5):
     ./
       config.ss
       server.ss
       main.ss
     utils/
       helpers.ss
       db.ss

   External Dependencies (3):
     :std/net/httpd
     :std/text/json
     :std/db/postgresql
```

### gerbil_project_map

Single call that returns all modules in a project with their exports, key definitions grouped by kind, and import dependencies. More detailed than `gerbil_project_info` — shows the full symbol map.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project_path` | string | yes | Directory containing the Gerbil project (with `gerbil.pkg`) |

```
project_path: "/path/to/myproject"
=> Project: myproject
   Location: /path/to/myproject
   Modules: 3

   Module: config.ss
     Exports: load-config, config-port, config-host
     Structs: config
     Procedures: load-config, config-port, config-host
     External imports: :std/text/json

   Module: server.ss
     Exports: start-server, stop-server
     Procedures: start-server, stop-server, handle-request
     External imports: :std/net/httpd, :std/text/json
     Internal imports: ./config

   Module: main.ss
     Procedures: main
     External imports: :std/getopt
     Internal imports: ./server, ./config
```

### gerbil_check_balance

Check parenthesis/bracket/brace balance in Gerbil Scheme code. Pure delimiter scanner — no subprocess, runs in milliseconds. Reports unclosed delimiters, unexpected closers, and mismatches with positions.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file_path` | string | no | Path to a Gerbil source file to check |
| `code` | string | no | Inline Gerbil code to check (alternative to `file_path`) |

```
file_path: "/path/to/module.ss"
=> Balance OK — 12 top-level forms, 156 delimiters

code: "(def (f x) (+ x 1)"
=> Balance error:

     Unclosed '(' opened at line 1, column 1
```

### gerbil_read_forms

Read a Gerbil source file using the actual Gerbil reader and list all top-level forms with their index, start/end line numbers, and a summary (`car` of list or type). On reader error, reports the error position plus any forms read before the error.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file_path` | string | yes | Path to a Gerbil source file to read |

```
file_path: "/path/to/module.ss"
=> Forms in /path/to/module.ss (4 forms):

     [0]  L1-L3   (import ...)
     [1]  L5-L5   (export ...)
     [2]  L7-L12  (def ...)
     [3]  L14-L20 (defstruct ...)
```

### gerbil_profile

Instrument specific functions with call counting and timing while running an expression. Reports per-function call count, cumulative time, average time, and percentage of wall time. Also reports overall wall time, CPU time, GC time, and bytes allocated. Instruments top-level bindings via `set!`; does not work on lexical bindings.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `expression` | string | yes | The Gerbil Scheme expression to profile |
| `functions` | string[] | yes | Function names to instrument (e.g. `["read-json", "hash-put!"]`) |
| `imports` | string[] | no | Modules to import first |
| `loadpath` | string[] | no | Directories to add to `GERBIL_LOADPATH` for project-local module resolution |

```
expression: "(begin (for-each read-json inputs) 'done)"
functions: ["read-json", "hash-put!"]
imports: [":std/text/json"]
=> Profile: (begin ...)

   Wall time: 4.5s | User: 4.3s | GC: 0.2s | Alloc: 150 MB

   Function              Calls        Time      Avg       %
   read-json              1000     3.200s   3.200ms   71.1%
   hash-put!             50000     1.100s   0.022ms   24.4%

   Result: done
```

### gerbil_heap_profile

Capture GC heap metrics before and after running an expression. Reports heap size, allocation, live objects, movable objects, and still objects. Forces garbage collection before each snapshot for accurate measurements. Uses `:std/debug/heap` `memory-usage` function.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `expression` | string | yes | The Gerbil Scheme expression to profile |
| `imports` | string[] | no | Modules to import first |
| `loadpath` | string[] | no | Directories to add to `GERBIL_LOADPATH` for project-local module resolution |

```
expression: "(make-vector 1000000 0)"
=> Heap Profile: (make-vector 1000000 0)

   Metric             Before        After        Delta
   gc-heap-size      68.0 MB      136.0 MB    +68.0 MB
   gc-alloc          56.2 MB      147.3 MB    +91.1 MB
   gc-live           28.5 MB       41.6 MB    +13.1 MB
   gc-movable        26.6 MB       39.7 MB    +13.1 MB
   gc-still           1.9 MB        1.9 MB     +0.0 MB

   Result: #(0 0 0 ...)
```

### gerbil_trace_calls

Count how many times specified functions are called while running an expression. Lightweight instrumentation with minimal overhead (no timing). Instruments top-level bindings via `set!`; does not work on lexical bindings.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `expression` | string | yes | The Gerbil Scheme expression to run |
| `functions` | string[] | yes | Function names to count calls for (e.g. `["read-json", "hash-put!"]`) |
| `imports` | string[] | no | Modules to import first |
| `loadpath` | string[] | no | Directories to add to `GERBIL_LOADPATH` for project-local module resolution |

```
expression: "(let loop ((i 0)) (if (< i 100) (begin (hash-put! ht i i) (loop (+ i 1))) 'done))"
functions: ["hash-put!"]
=> Call Trace: (let loop ...)

   Function              Calls
   hash-put!               100

   Result: done
```

### gerbil_call_graph

Analyze which functions call which other functions in a Gerbil source file. Uses static analysis (no execution). Shows a tree visualization and flat listing of call relationships between locally defined functions. Optionally filter to show only the tree rooted at a specific function.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file_path` | string | yes | Absolute path to a Gerbil source file (`.ss` or `.scm`) |
| `function` | string | no | If provided, only show the call tree rooted at this function |

```
file_path: "/path/to/server.ss"
=> Call graph: /path/to/server.ss

   main
     +-- helper
     +-- process-data
     |   +-- parse-input
     |   +-- validate
     +-- output-results

   Defined functions: 5
     main (L10) -> helper, process-data, output-results
     process-data (L20) -> parse-input, validate
     parse-input (L30) -> (no local calls)
     validate (L40) -> (no local calls)
     helper (L50) -> (no local calls)

file_path: "/path/to/server.ss", function: "process-data"
=> Call graph: /path/to/server.ss (rooted at process-data)

   process-data
     +-- parse-input
     +-- validate

   ...
```

### gerbil_scaffold_test

Generate a `:std/test` skeleton from a module's exports. Introspects the module to discover exported procedures, macros, and values, then produces a ready-to-fill test file. Does not write to disk — returns the generated text.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `module_path` | string | yes | Module to generate tests for (e.g. `:std/text/json`, `:myproject/handler`) |
| `suite_name` | string | no | Override the test suite name (default: derived from module path) |
| `loadpath` | string[] | no | Directories to add to `GERBIL_LOADPATH` for project-local module resolution |

```
module_path: ":std/text/json"
=> (import :std/test :std/text/json)
   (export json-test)

   (def json-test
     (test-suite "std/text/json"
       (test-case "read-json"
         (check (read-json arg1) => ...))
       (test-case "write-json"
         (check (write-json arg1) => ...))
       ...))

module_path: ":std/text/json", suite_name: "my-json-test"
=> (import :std/test :std/text/json)
   (export my-json-test)

   (def my-json-test
     (test-suite "std/text/json"
       ...))
```

### gerbil_build_and_report

Run `gerbil build` on a project directory and return structured diagnostics. On success, reports a summary. On failure, parses compiler errors into structured file:line:column diagnostics. Uses the modern `gerbil` CLI (not gxpkg).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project_path` | string | yes | Directory containing the Gerbil project (with `gerbil.pkg`) |
| `flags` | string[] | no | Extra build flags: `"--release"`, `"--optimized"`, `"--debug"` |
| `context_lines` | number | no | Lines of source context to show around each error (default: 3). Set to 0 to disable. |
| `loadpath` | string[] | no | Directories to add to `GERBIL_LOADPATH` for project-local module resolution |

```
project_path: "/path/to/myproject"
=> Build succeeded.

   ... compilation output ...

project_path: "/path/to/myproject", flags: ["--optimized"]
=> Build succeeded (--optimized).

project_path: "/path/to/myproject", loadpath: ["~/.gerbil/lib", "/path/to/deps/.gerbil/lib"]
=> Build succeeded.

project_path: "/path/to/broken-project"
=> Build failed: 2 error(s), 0 warning(s)

     [ERROR] server.ss:12:5 — Reference to unbound identifier: foo
     [ERROR] utils.ss:30:10 — Syntax Error: cannot find library module
```

### gerbil_generate_module_stub

Generate a module skeleton by introspecting an existing module's exports and signatures. Produces `(def ...)` stubs for procedures with arity-based argument placeholders, `(defrules ...)` for macros, and `(def ...)` for values. Does not write to disk — returns the generated text.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `module_path` | string | yes | Module to generate a stub from (e.g. `:std/text/json`) |
| `package_prefix` | string | no | Package prefix for the generated module |
| `imports` | string[] | no | Additional import paths to include |
| `loadpath` | string[] | no | Directories to add to `GERBIL_LOADPATH` for project-local module resolution |

```
module_path: ":std/text/json"
=> (import :std/text/json)
   (export read-json write-json json-object->string ...)

   (def (read-json arg1)
     ...)

   (def (write-json arg1)
     ...)

   ...

module_path: ":std/text/json", package_prefix: "myapp", imports: [":std/iter"]
=> (package: myapp)

   (import :std/text/json :std/iter)
   (export read-json write-json ...)

   (def (read-json arg1)
     ...)
   ...
```

### gerbil_check_exports

Static analysis tool that checks export/import consistency across a Gerbil project. Detects symbols exported but not defined in the file, and cross-module import mismatches where file A imports from project module B but uses symbols that B does not export. Pure static analysis — no subprocess, fast.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project_path` | string | yes | Path to the Gerbil project directory |

```
project_path: "/path/to/myproject"
=> Export consistency check: /path/to/myproject

   Found 2 issue(s):

   [ERROR] server.ss:3 — Exported symbol "handle-auth" is not defined in this file
   [WARNING] api.ss:5 — Imports "process-data" from ./server, but ./server does not export it
```

### gerbil_generate_module

Generate a Gerbil module by reading a template file and applying word-boundary-aware string substitutions. Returns the generated text (does NOT write to disk). Useful for creating mechanical variations of an existing module pattern.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `template_path` | string | yes | Path to the template `.ss` file to use as a base |
| `substitutions` | array | yes | Array of `{from, to}` substitution pairs |

```
template_path: "/path/to/handler.ss"
substitutions: [{"from": "user", "to": "admin"}, {"from": "User", "to": "Admin"}]
=> (import :myapp/admin-db)
   (export admin-handler admin-list)

   (def (admin-handler req)
     ...)
   ...
```

### gerbil_howto

Search curated Gerbil Scheme idioms and recipes by keyword. Returns code examples with imports and usage notes. Supports merging in additional recipes from an external JSON cookbook file.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `query` | string | yes | Search keywords (e.g. `"json parse"`, `"file read"`, `"channel thread"`) |
| `cookbook_path` | string | no | Absolute path to a JSON cookbook file with additional recipes to merge |

```
query: "json parse"
=> Found 3 recipe(s) for "json parse":

   ## Parse JSON string
   Imports: :std/text/json
   ```scheme
   (import :std/text/json)
   (def data (call-with-input-string "{\"name\":\"alice\"}" read-json))
   ```
   ...

query: "custom recipe", cookbook_path: "/path/to/project/.claude/cookbooks.json"
=> Found 1 recipe(s) for "custom recipe":

   ## My Custom Recipe
   ...
```

When `cookbook_path` is provided, recipes from the external file are merged with the built-in set. External recipes override built-in ones if they share the same `id`.

### gerbil_howto_add

Append a new Gerbil Scheme recipe to a JSON cookbook file. If a recipe with the same `id` already exists, it is replaced (update semantics). Convention: use `.claude/cookbooks.json` in the project root.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `cookbook_path` | string | yes | Absolute path to the JSON cookbook file |
| `id` | string | yes | Unique recipe identifier in kebab-case |
| `title` | string | yes | Human-readable title |
| `tags` | string[] | yes | Search keywords |
| `imports` | string[] | yes | Gerbil module imports (use `[]` if none needed) |
| `code` | string | yes | Code example |
| `notes` | string | no | Usage notes |
| `related` | string[] | no | Related recipe IDs |

```
cookbook_path: "/path/to/project/.claude/cookbooks.json"
id: "csv-read"
title: "Read a CSV file"
tags: ["csv", "file", "read", "parse"]
imports: [":std/text/csv"]
code: "(import :std/text/csv)\n(call-with-input-file \"data.csv\" read-csv)"
=> Added recipe "csv-read" in /path/to/project/.claude/cookbooks.json (1 total recipes).
```

### gerbil_file_summary

Structural overview of a Gerbil source file without reading the whole file. Shows imports, exports, and definitions grouped by kind. Pure TypeScript — no subprocess, fast.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `file_path` | string | yes | Absolute path to a Gerbil source file (`.ss` or `.scm`) |

```
file_path: "/path/to/server.ss"
=> File: server.ss (1.2 KB)

   Imports:
     :std/net/httpd
     :std/text/json

   Exports:
     start-server, stop-server

   Procedures:
     L4  start-server
     L12 stop-server
     L20 handle-request

   Structs:
     L8  config
```

### gerbil_make

Run a Makefile target in a Gerbil project directory. Returns structured output with exit code, stdout, and stderr.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project_path` | string | yes | Directory containing the Makefile |
| `target` | string | no | Make target to run (e.g. `"build"`, `"clean"`). Defaults to the default target. |
| `timeout` | number | no | Timeout in milliseconds (default: 120000) |

```
project_path: "/path/to/project", target: "build"
=> Make target "build" completed successfully.

   ... build output ...

project_path: "/path/to/project"
=> No Makefile found in /path/to/project.
```

### gerbil_describe

Evaluate a Gerbil Scheme expression and describe the resulting value's type, structure, and key properties. Useful for understanding what a function returns or what a data structure contains.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `expression` | string | yes | The Gerbil Scheme expression to evaluate and describe |
| `imports` | string[] | no | Modules to import first |
| `loadpath` | string[] | no | Directories to add to `GERBIL_LOADPATH` |
| `project_path` | string | no | Project directory for auto-configuring `GERBIL_LOADPATH` |
| `env` | object | no | Environment variables for the subprocess |

```
expression: "(hash (\"name\" \"alice\") (\"age\" 30))"
=> hash-table (2 entries)
     "name" => "alice"
     "age" => 30

expression: "[1 2 3]"
=> list (length 3)
     1
     2
     3

expression: "42"
=> exact integer: 42

expression: "car"
=> procedure
```

## Resources

The server exposes MCP resources for browsing the built-in cookbook recipe library.

### gerbil://cookbooks

Returns a JSON index of all cookbook recipes with `id`, `title`, and `tags` for each entry. Useful for browsing available patterns without fetching full code.

### gerbil://cookbooks/{id}

Returns the full JSON detail for a single recipe, including `code`, `imports`, `notes`, and `related` recipes.

## Prompts

The server provides reusable prompt templates that MCP clients can invoke to get Gerbil-aware instructions.

### explain-code

Explain a piece of Gerbil Scheme code with awareness of Gerbil-specific idioms (bracket list syntax, keyword colons, dot-syntax methods, macros).

| Argument | Type | Required | Description |
|----------|------|----------|-------------|
| `code` | string | yes | The Gerbil Scheme code to explain |

### convert-to-gerbil

Convert code from another language to idiomatic Gerbil Scheme, following Gerbil conventions (`def`, brackets, `:std/iter`, `match`, `chain`, etc.).

| Argument | Type | Required | Description |
|----------|------|----------|-------------|
| `code` | string | yes | The source code to convert |
| `source_language` | string | no | Source language (auto-detected if omitted) |

### generate-tests

Generate comprehensive tests for a Gerbil module using the `:std/test` framework, following naming conventions (`*-test.ss`, `*-test` export).

| Argument | Type | Required | Description |
|----------|------|----------|-------------|
| `module_path` | string | yes | Module path to generate tests for |

### review-code

Review Gerbil Scheme code for issues with a checklist of common Gerbil pitfalls (`define` vs `def`, `#f` vs `'()`, missing `-O`, unsafe declares, etc.).

| Argument | Type | Required | Description |
|----------|------|----------|-------------|
| `code` | string | yes | The Gerbil Scheme code to review |

### write-gerbil-module

Get guidance for writing a new Gerbil Scheme module, following Gerbil conventions (`def`, `defstruct`, keyword args, `:std/iter`, etc.).

| Argument | Type | Required | Description |
|----------|------|----------|-------------|
| `module_name` | string | yes | The module name (e.g. `"myapp/handler"`) |
| `purpose` | string | yes | What the module should do |

### debug-gerbil-error

Get help debugging a Gerbil Scheme error with a structured debugging workflow that leverages the available MCP tools.

| Argument | Type | Required | Description |
|----------|------|----------|-------------|
| `error_message` | string | yes | The error message or stack trace |
| `code` | string | no | The code that produced the error (if available) |

### port-to-gerbil

Port code from another Scheme dialect (Racket, Guile, Chicken, Chez) to idiomatic Gerbil with a mapping of key differences (`hash-ref` → `hash-get`, `#:keyword` → `keyword:`, etc.).

| Argument | Type | Required | Description |
|----------|------|----------|-------------|
| `code` | string | yes | The Scheme code to port |
| `source_dialect` | string | no | Source Scheme dialect (auto-detected if omitted) |

## How It Works

Most tools invoke `gxi -e` as a short-lived subprocess — no persistent state between calls. Expressions are wrapped in error-handling code using `with-catch` and output structured markers (`GERBIL-MCP-RESULT:` / `GERBIL-MCP-ERROR:`) that the TypeScript layer parses.

The `gerbil_compile_check` tool uses `gxc -S` (the Gerbil compiler in expand-only mode) to catch errors beyond what the expander alone detects. When gxc hits internal compiler crashes, the tool post-processes the error output to detect known patterns (e.g., `stx-car-e`, code generation failures) and appends diagnostic hints.

The `gerbil_repl_session` tool spawns a persistent `gxi` subprocess with piped stdin/stdout, using a sentinel-based protocol to delimit expression output across multiple evaluations. Sessions can be created with a `project_path` or explicit `loadpath` to set `GERBIL_LOADPATH` for the subprocess, enabling project-local module imports.

Several tools (`gerbil_eval`, `gerbil_compile_check`, `gerbil_repl_session`, `gerbil_profile`, `gerbil_heap_profile`, `gerbil_trace_calls`) accept a `loadpath` parameter that sets `GERBIL_LOADPATH` on the subprocess environment. This allows operating within a project's build context — for example, importing modules from a project's `.gerbil/lib` directory.

The `gerbil_profile` and `gerbil_trace_calls` tools use Scheme-side instrumentation via `eval`/`set!` to replace top-level function bindings with wrappers that count calls (and measure timing for profile). The `gerbil_heap_profile` tool uses `:std/debug/heap` `memory-usage` to capture GC metrics before and after expression evaluation, forcing garbage collection for accurate snapshots.

The `gerbil_call_graph` tool is pure TypeScript — it reads a source file, parses function definitions using paren-depth tracking, and builds an adjacency list by scanning function bodies for references to other defined functions.

The `gerbil_build_project`, `gerbil_package_info`, `gerbil_scaffold`, and `gerbil_package_manage` tools invoke `gxpkg` as a subprocess for package management, project scaffolding, and building. The `gerbil_build_and_report` tool invokes the modern `gerbil build` CLI and parses its output into structured diagnostics using the same `parseGxcErrors()` parser as `gerbil_diagnostics`.

The `gerbil_scaffold_test` and `gerbil_generate_module_stub` tools use the same Gerbil introspection mechanism as `gerbil_function_signature` — importing a module and iterating its exports to classify each as procedure (with arity), macro, or value — then formatting the results as ready-to-use Scheme source code.

User input is injected via `(read (open-input-string ...))` rather than string interpolation, letting Scheme's reader handle all quoting. Subprocess execution uses `execFile` (not `exec`) to avoid shell injection.

## Project Structure

```
src/
  index.ts                Server entry point, tool & prompt & resource registration
  gxi.ts                  gxi/gxc/gxpkg subprocess wrapper, REPL session manager
  prompts.ts              MCP prompt templates (7 prompts)
  resources.ts            MCP resources (cookbook index and recipe detail)
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
    find-definition.ts    gerbil_find_definition
    build-project.ts      gerbil_build_project
    package-info.ts       gerbil_package_info
    format.ts             gerbil_format
    benchmark.ts          gerbil_benchmark
    error-hierarchy.ts    gerbil_error_hierarchy
    version.ts            gerbil_version
    scaffold.ts           gerbil_scaffold
    package-manage.ts     gerbil_package_manage
    find-callers.ts       gerbil_find_callers
    suggest-imports.ts    gerbil_suggest_imports
    diagnostics.ts        gerbil_diagnostics
    document-symbols.ts   gerbil_document_symbols
    workspace-symbols.ts  gerbil_workspace_symbols
    rename-symbol.ts      gerbil_rename_symbol
    lint.ts               gerbil_lint
    project-info.ts       gerbil_project_info
    project-map.ts        gerbil_project_map
    check-balance.ts      gerbil_check_balance
    read-forms.ts         gerbil_read_forms
    profile.ts            gerbil_profile
    heap-profile.ts       gerbil_heap_profile
    trace-calls.ts        gerbil_trace_calls
    call-graph.ts         gerbil_call_graph
    scaffold-test.ts      gerbil_scaffold_test
    build-and-report.ts   gerbil_build_and_report
    generate-module-stub.ts gerbil_generate_module_stub
    check-exports.ts      gerbil_check_exports
    generate-module.ts    gerbil_generate_module
    howto.ts              gerbil_howto
    howto-add.ts          gerbil_howto_add
    file-summary.ts       gerbil_file_summary
    make.ts               gerbil_make
    describe.ts           gerbil_describe
    parse-utils.ts        Shared parsing utilities
test/
  tools.test.ts           Functional tests for all MCP tools
```

## License

MIT
