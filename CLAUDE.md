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

All 193 tests must pass before considering any change complete. The test suite covers:
- Core evaluation tools (eval, syntax checking, compilation, compile-check error details)
- Module inspection tools (exports, dependencies, signatures)
- Symbol lookup tools (doc, find definition, suggest imports)
- Macro tools (expand, trace)
- Type inspection tools (class info, error hierarchy)
- File analysis tools (load file, document symbols, lint, diagnostics, hash literal lint, channel pattern lint, unquote/dot-in-brackets/missing-export pitfall detection)
- Balance checking tools (delimiter balance, inline code, #! reader directives, vector literals, verification suggestion)
- Read forms tool (top-level form listing, reader errors)
- Project tools (workspace symbols, find callers, rename, project map)
- Run tests tool (single-file, directory mode, filter, validation)
- REPL session tools (create, eval, destroy lifecycle, loadpath, project_path, preload_file)
- Performance and profiling tools (profile, heap profile, trace calls, call graph)
- Code generation tools (scaffold test, generate module stub, generate module from template)
- Build and report tool (structured build diagnostics, source context lines, loadpath support)
- Enhanced find definition (source preview)
- Cross-module export checker (check-exports)
- Diagnostics with loadpath support
- Function signature parameter names (source-level formals extraction, runtime keyword arg detection, loadpath/project_path support)
- Module exports loadpath/project_path support
- Makefile awareness in build-and-report and gerbil_make tool
- Howto cookbook tool (keyword-based recipe search, external cookbook loading, howto_add)
- Howto verify tool (cookbook recipe syntax validation, compile_check mode for gxc compilation, REPL-only pattern detection)
- File summary tool (structural overview)
- Check arity tool (project-wide call-site arity checking)
- Resolve imports tool (bulk unbound identifier resolution)
- Trace eval tool (let*/let/letrec/letrec* binding tracing)
- SXML inspect tool (XML parsing and tree visualization)
- Eval project_path support
- Feature suggestion tools (suggest_feature, list_features)
- Demangle tool (hex decoding, double underscore, prefix recognition, module/symbol split)
- Stale static files tool (no local files, no overlap, size mismatch detection, matching files, extension filtering)
- Module exports declare fallback (compiled .scm artifact scanning when expander fails)
- Build and report auto-loadpath (gerbil.pkg depend: auto-detection, non-executable build.ss detection)
- Balanced replace tool (balance-preserving edits, rejection on break, fix detection, dry-run, ambiguous match, strings with parens)
- Wrap form tool (single-line wrap, multi-line range, auto-detect end_line, invalid wrapper rejection, dry-run)
- Splice form tool (default head removal, explicit keep_children, single child, apply mode, no form found)
- Check test arity tool (test file scanning, arity mismatch detection, no-test-files handling)


### Adding a New Tool

1. Create a new file in `src/tools/` (e.g., `src/tools/my-tool.ts`)
2. Export a `registerMyTool(server: McpServer)` function
3. Import and call the register function in `src/index.ts`
4. Add the tool to the `INSTRUCTIONS` string in `src/index.ts`
5. Add corresponding test(s) in `test/tools.test.ts`
6. Add the tool to `CLAUDE.md.gerbil-example` under the appropriate section
7. Update the test count and coverage list in this file (`CLAUDE.md`)
8. Run `npm run build && npm run test` to verify

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
