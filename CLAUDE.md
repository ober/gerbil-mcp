# gerbil-mcp Development Guide

This is the gerbil-mcp project — an MCP server that provides Gerbil Scheme language intelligence to AI assistants.

## Project Structure

```
src/
  index.ts          # Server entry point, tool & prompt registration
  gxi.ts            # gxi/gxc/gxpkg subprocess wrapper, REPL session manager
  prompts.ts        # MCP prompt templates
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

### Cross-Version Cookbook Testing

Test cookbook recipes against multiple Gerbil installations and update the `valid_for` field:

```sh
# Test against current gxi install (dry-run — no modifications)
npx tsx scripts/test-cookbooks.ts --gxi $(which gxi) --dry-run

# Test against multiple versions
npx tsx scripts/test-cookbooks.ts --gxi /path/to/v18/gxi --gxi /path/to/v19/gxi

# Test a specific recipe
npx tsx scripts/test-cookbooks.ts --gxi $(which gxi) --recipe-id json-parse

# Skip compile checking (syntax-only, faster)
npx tsx scripts/test-cookbooks.ts --gxi $(which gxi) --no-compile
```

The script updates each recipe's `valid_for` field in `cookbooks.json` with the list of version strings where both syntax and compile checks pass. The `valid_for` field enables prefix-based version matching at query time (e.g., `v0.18.1-173` matches `v0.18.2-5`).

## Development Workflow

### MANDATORY: Run Tests After Changes

After adding or modifying any code in this repository, you MUST run the test suite:

```sh
npm run build && npm run test
```

All 351 tests must pass before considering any change complete. The test suite covers:
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
- Feature suggestion tools (suggest_feature, list_features, vote_feature, gerbil_version tagging and filtering)
- Demangle tool (hex decoding, double underscore, prefix recognition, module/symbol split)
- Stale static files tool (no local files, no overlap, size mismatch detection, matching files, extension filtering)
- Module exports declare fallback (compiled .scm artifact scanning when expander fails)
- Build and report auto-loadpath (gerbil.pkg depend: auto-detection, non-executable build.ss detection)
- Balanced replace tool (balance-preserving edits, rejection on break, fix detection, dry-run, ambiguous match, strings with parens)
- Wrap form tool (single-line wrap, multi-line range, auto-detect end_line, invalid wrapper rejection, dry-run)
- Splice form tool (default head removal, explicit keep_children, single child, apply mode, no form found)
- FFI scaffold tool (typedef parsing, function declarations, #define constants, enums, create/destroy pairs, auto-cleanup, include_path, module_name, missing file handling)
- Check test arity tool (test file scanning, arity mismatch detection, no-test-files handling)
- Class info constructor signature (make-X positional arg order with inherited field annotations)
- Project dependency graph tool (dependency tree, external deps listing, gerbil.pkg requirement)
- Test coverage summary tool (module export introspection, test file auto-discovery, coverage percentage)
- Module catalog tool (sugar descriptions, iter descriptions, unknown module error handling)
- Eval stdout capture (parameterize-based output capture, separate stdout from return value, void-with-output)
- Lint re-export awareness (bare module import detection, comment token filtering in exports)
- Balanced replace matching imbalance (net-zero delimiter change detection, non-matching rejection)
- Cookbook correction flag (deprecated/superseded_by fields, deprioritized scoring, supersedes parameter in howto_add)
- Port type mismatch lint (fdopen + char I/O detection, variable tracking, same-line and forward-scan)
- FFI callback debug tool (c-define/extern cross-reference, orphan detection, duplicate C names, begin-foreign checking)
- Example API coverage tool (module export introspection, directory scanning, per-export file references, coverage percentage)
- Validate example imports tool (hybrid static+runtime analysis, builtin symbol filtering, undefined symbol detection)
- Function signature compiled artifact scan (compiled .scm keyword-dispatch pattern extraction, fallback for missing source)
- Bisect crash tool (crash detection, form parsing, binary search bisection, preamble preservation, missing file handling)
- Import conflict checker (local def vs import conflict detection, cross-import conflicts, only-in filter handling, batch runtime resolution, project-wide mode)
- Cookbook version tagging (gerbil_version field in howto_add, version tag display in howto search, explicit version filter excluding mismatched recipes, howto_verify version filtering, per-recipe version tags in verify output)
- Feature version tagging (gerbil_version field in suggest_feature, version tag display in list_features, explicit version filter excluding mismatched features)
- Cross-version cookbook testing (valid_for field in Recipe, prefix matching for version filtering, valid_for storage and preservation in howto_add, valid_for display in howto search and verify, shared verify-utils extraction)
- Security scan tool (shell injection detection, FFI pointer-void mismatch, static global buffer in C, port without unwind-protect, clean file no issues, severity threshold filter, project-wide scan, missing file error, port-open with guard skipped)
- Security pattern add tool (new rules file creation, update semantics, corrupt JSON error)
- Howto compact search (compact mode brief listings, max_results limit, howto_get fetch by id, unknown id error)
- FFI type check tool (declaration extraction, type mismatch detection, no-declarations handling, required params validation)
- Stale linked pkg tool (no symlinks handling, no pkg dir handling)
- Eval with env vars (env parameter passes through to subprocess)
- Cross-module export collision detection (shared exports across sibling modules)
- Run tests auto-loadpath (gerbil.pkg depend: auto-detection)
- Build and report auto-retry (lock error detection, missing exe_c detection, missing C header detection)
- Function signature keyword breakdown (positional vs keyword arg extraction from compiled artifacts)
- Tool annotations (readOnlyHint/idempotentHint on all tools, verified via tools/list)
- Describe tool (hash table, list, number, string, boolean, procedure description, error handling)
- New prompts (write-gerbil-module, debug-gerbil-error, port-to-gerbil)
- Cookbook resources (gerbil://cookbooks index, gerbil://cookbooks/{id} detail, unknown id handling)
- Reference resources (gerbil://reference/* for idioms, pattern-matching, actors, stdlib-map, gambit-interop; listResources inclusion, markdown content verification, unknown URI error handling)
- Smart complete tool (prefix-based symbol completion, module-scoped completion)
- Explain error tool (arity mismatch classification, unbound identifier classification, unknown error fallback, cookbook recipe lookup)
- Diff modules tool (added/removed/shared export comparison, same-module comparison)
- Migration check tool (v0.18→v0.19 pattern detection, clean file no issues)
- Dead code tool (unexported uncalled definition detection, project-wide scan)
- Dependency cycles tool (circular import detection, cycle-free project handling)
- Generate API docs tool (procedure/macro/value classification, custom title)
- New prompts (optimize-gerbil-code, migrate-gerbil-version, design-ffi-bindings, refactor-gerbil-module)
- Prompt improvements (debug-error with describe/explain, review with security scan, write-module with howto, convert with keywords, generate-tests with async, port with quasiquote)
- Multi-module integration tests (check_exports + dep_graph + dependency_cycles agreement)
- Find definition stdlib source lookup (lib/ → src/ path rewrite, module_path fallback, missing source graceful handling)


### Adding a New Tool

1. Create a new file in `src/tools/` (e.g., `src/tools/my-tool.ts`)
2. Export a `registerMyTool(server: McpServer)` function
3. Import and call the register function in `src/index.ts`
4. Add the tool to the `INSTRUCTIONS` string in `src/index.ts`
5. Add corresponding test(s) in `test/tools.test.ts`
6. Add the tool to both `CLAUDE.md.gerbil-example` and `copilot-instructions.md.gerbil-example` under the appropriate section (these are the templates that users copy into their projects — the former for Claude Code, the latter for GitHub Copilot). The copilot version omits Claude-specific sections (skills, hooks, PreCompact) but has the same tool listings.
7. Update the test count and coverage list in this file (`CLAUDE.md`)
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
