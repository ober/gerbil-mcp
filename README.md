# gerbil-mcp

An MCP (Model Context Protocol) server that gives AI assistants live access to a Gerbil Scheme environment. It lets Claude (or any MCP client) evaluate expressions, look up module exports, check syntax, expand macros, and search symbols against a real `gxi` runtime — instead of guessing from training data.

## Prerequisites

- **Node.js** >= 18
- **Gerbil Scheme** installed with `gxi` available (tested with v0.19+)

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
     :std/text/utf8
     ...
```

## How It Works

Each tool invokes `gxi -e` as a subprocess — no persistent REPL, no state between calls. This avoids the complexity of managing gxi's interactive mode (which has banners, prompts, and debug REPL behavior that complicate scripted usage).

Expressions are wrapped in error-handling code that uses `with-catch` to trap exceptions and output structured markers (`GERBIL-MCP-RESULT:` / `GERBIL-MCP-ERROR:`) that the TypeScript layer parses.

User input is injected via `(read (open-input-string ...))` rather than string interpolation, letting Scheme's reader handle all quoting. Subprocess execution uses `execFile` (not `exec`) to avoid shell injection.

## Project Structure

```
src/
  index.ts              Server entry point, tool registration
  gxi.ts                gxi subprocess wrapper (execFile, timeouts, escaping)
  tools/
    eval.ts             gerbil_eval
    module-exports.ts   gerbil_module_exports
    check-syntax.ts     gerbil_check_syntax
    expand-macro.ts     gerbil_expand_macro
    apropos.ts          gerbil_apropos
    list-modules.ts     gerbil_list_std_modules
```

## License

MIT
# gerbil-mcp
