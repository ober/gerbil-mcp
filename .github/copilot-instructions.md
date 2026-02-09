# gerbil-mcp Development Guide

This is the gerbil-mcp project — an MCP server that provides Gerbil Scheme language intelligence to AI assistants.

## Project Structure

```
src/
  index.ts          # Server entry point, tool & prompt registration
  gxi.ts            # gxi/gxc/gxpkg subprocess wrapper, REPL session manager
  prompts.ts        # MCP prompt templates
  resources.ts      # MCP resource providers (cookbook access)
  tools/            # Individual tool implementations (one file per tool)
    verify-utils.ts # Shared verification utilities (used by howto-verify + CLI script)
test/
  tools.test.ts     # Functional tests for all MCP tools
scripts/
  test-cookbooks.ts # Cross-version cookbook recipe tester (standalone CLI)
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

All 302 tests must pass before considering any change complete.

### Adding a New Tool

1. Create a new file in `src/tools/` (e.g., `src/tools/my-tool.ts`)
2. Export a `registerMyTool(server: McpServer)` function
3. Import and call the register function in `src/index.ts`
4. Add the tool to the `INSTRUCTIONS` string in `src/index.ts`
5. Add corresponding test(s) in `test/tools.test.ts`
6. Add the tool to both `CLAUDE.md.gerbil-example` and `copilot-instructions.md.gerbil-example` under the appropriate section
7. Update the test count and coverage list in `CLAUDE.md`
8. If the tool was listed in `features.json`, remove that entry (it's now implemented)
9. Run `npm run build && npm run test` to verify

### Common Patterns

- Tool implementations use `runGxi()` to invoke Gerbil expressions
- Use `runGxiFile()` to run a `.ss` file directly (used by `run-tests` single-file mode)
- Use `runGerbilCmd()` to invoke the `gerbil` CLI binary (used by `run-tests` directory mode)
- Use `escapeSchemeString()` for user input embedded in Scheme code
- Use `buildLoadpathEnv()` to construct `GERBIL_LOADPATH` env overlay from a loadpath array
- Use markers like `GERBIL-MCP-RESULT:` to delimit output for parsing
- Wrap Scheme code in `(with-catch ...)` for error handling
- Pass custom env to subprocesses via the `env` field in `GxiOptions` (used by loadpath support)

## Testing Details

The test suite uses Vitest and spawns a real MCP server process for integration testing. Tests communicate via JSON-RPC over stdin/stdout.

Test files are created in a temp directory and cleaned up automatically.

## Prerequisites

- Node.js >= 18
- Gerbil Scheme with `gxi`, `gxc`, `gxpkg` available in PATH or at `/opt/gerbil/bin/`
