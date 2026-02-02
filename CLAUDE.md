# gerbil-mcp Development Guide

This is the gerbil-mcp project — an MCP server that provides Gerbil Scheme language intelligence to AI assistants.

## Project Structure

```
src/
  index.ts          # Server entry point, tool & prompt registration
  gxi.ts            # gxi/gxc/gxpkg subprocess wrapper, REPL session manager
  prompts.ts        # MCP prompt templates
  tools/            # Individual tool implementations (one file per tool)
test/
  tools.test.ts     # Functional tests for all MCP tools
```

## Build & Test Commands

```sh
npm run build      # Compile TypeScript to dist/
npm run test       # Run the full test suite
npm run test:watch # Run tests in watch mode
npm run dev        # Watch mode for TypeScript compilation
```

## Development Workflow

### MANDATORY: Run Tests After Changes

After adding or modifying any code in this repository, you MUST run the test suite:

```sh
npm run build && npm run test
```

All 40 tests must pass before considering any change complete. The test suite covers:
- Core evaluation tools (eval, syntax checking, compilation)
- Module inspection tools (exports, dependencies, signatures)
- Symbol lookup tools (doc, find definition, suggest imports)
- Macro tools (expand, trace)
- Type inspection tools (class info, error hierarchy)
- File analysis tools (load file, document symbols, lint, diagnostics)
- Balance checking tools (delimiter balance, inline code)
- Read forms tool (top-level form listing, reader errors)
- Project tools (workspace symbols, find callers, rename)
- REPL session tools (create, eval, destroy lifecycle)

### Adding a New Tool

1. Create a new file in `src/tools/` (e.g., `src/tools/my-tool.ts`)
2. Export a `registerMyTool(server: McpServer)` function
3. Import and call the register function in `src/index.ts`
4. Add corresponding test(s) in `test/tools.test.ts`
5. Run `npm run build && npm run test` to verify

### Common Patterns

- Tool implementations use `runGxi()` to invoke Gerbil expressions
- Use `escapeSchemeString()` for user input embedded in Scheme code
- Use markers like `GERBIL-MCP-RESULT:` to delimit output for parsing
- Wrap Scheme code in `(with-catch ...)` for error handling

## Testing Details

The test suite uses Vitest and spawns a real MCP server process for integration testing. Tests communicate via JSON-RPC over stdin/stdout.

Test files are created in a temp directory and cleaned up automatically.

## Prerequisites

- Node.js >= 18
- Gerbil Scheme with `gxi`, `gxc`, `gxpkg` available in PATH or at `/opt/gerbil/bin/`
