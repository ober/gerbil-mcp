## Build & Verification

After modifying any `.ss` (Gerbil Scheme) source files, always run the build command and fix any errors before moving on. Common issues include: missing imports, wrong function names, and duplicate definitions.

## Gerbil MCP Tools — MANDATORY Usage

When a Gerbil MCP server is available, you MUST use its tools extensively instead of guessing about Gerbil APIs, syntax, or behavior. Gerbil is a niche Scheme dialect with limited training data — always verify with live tools rather than relying on memory.

### Before Writing Code

- **`gerbil_module_exports`**: Check what a module actually exports before using it. Never guess function names.
- **`gerbil_function_signature`**: Check procedure arities before calling functions. Prevents wrong-number-of-arguments errors.
- **`gerbil_check_syntax`**: Validate syntax of any code you write before presenting it.
- **`gerbil_compile_check`**: Run the full compiler (`gxc -S`) to catch unbound identifiers and type issues beyond syntax.
- **`gerbil_doc`**: Look up any symbol for type, arity, qualified name, and related symbols.

### When Exploring or Debugging

- **`gerbil_eval`**: Test expressions interactively to verify assumptions and reproduce issues.
- **`gerbil_apropos`**: Search for symbols by substring when you're unsure of exact names.
- **`gerbil_list_std_modules`**: Discover available standard library modules, optionally filtered by prefix.
- **`gerbil_find_definition`**: Locate where a symbol is defined (source file, module, kind, arity). Use `source_preview: true` to include a code preview.
- **`gerbil_load_file`**: Parse a `.ss` file to extract its imports, exports, and definitions without executing it.
- **`gerbil_module_deps`**: See what a module imports. Use `transitive: true` for full dependency tree.

### When Understanding Macros

- **`gerbil_expand_macro`**: See the fully expanded core form of a macro expression.
- **`gerbil_trace_macro`**: Step-by-step expansion showing each transformation level.

### When Working with Types and FFI

- **`gerbil_class_info`**: Inspect defclass/defstruct types — slots, fields, inheritance, precedence list.
- **`gerbil_ffi_inspect`**: Classify a module's FFI exports (C constants, C functions, Gerbil wrappers).
- **`gerbil_error_hierarchy`**: View the full exception/error class hierarchy tree.

### For Multi-Step Exploration

- **`gerbil_repl_session`**: Maintain persistent state across evaluations. Define functions, import modules, test incrementally. Use `preload_file` to load a file's imports into the session automatically.

### For Building and Testing

- **`gerbil_build_project`**: Compile or clean a project directory using gxpkg.
- **`gerbil_build_and_report`**: Run `gerbil build` and get structured diagnostics with file, line, column. Prefer this over running `gerbil build` via bash for better error reporting.
- **`gerbil_run_tests`**: Execute a single `:std/test` file (`file_path`) or run project-wide tests (`directory`). Use `filter` to match test names, `quiet` for errors-only output.
- **`gerbil_package_info`**: List installed packages, search the package directory, or view metadata.
- **`gerbil_package_manage`**: Install, update, or uninstall Gerbil packages.
- **`gerbil_scaffold`**: Create a new Gerbil project from a template using gxpkg new.

### For Performance Analysis

- **`gerbil_profile`**: Instrument specific functions with call counting and timing. Reports per-function stats (calls, time, avg, %) plus overall wall/CPU/GC time and allocation.
- **`gerbil_heap_profile`**: Capture GC heap metrics before/after an expression. Reports heap size, allocation, live/movable/still objects with deltas.
- **`gerbil_trace_calls`**: Lightweight call counting (no timing overhead). Count how many times specified functions are called.

### For Code Quality

- **`gerbil_lint`**: Static analysis for common issues: unused imports, duplicate definitions, style warnings, shadowed bindings, hash literal symbol key warnings, channel anti-patterns, unquote outside quasiquote, dot in brackets, missing exported definitions, and compilation errors.
- **`gerbil_diagnostics`**: Run `gxc -S` on a file or project and get structured diagnostics with file, line, column, severity, and message. Use `loadpath` to resolve project-local module imports.
- **`gerbil_check_exports`**: Cross-module export/import consistency checker. Detects symbols exported but not defined, and cross-module import mismatches across a project.
- **`gerbil_format`**: Pretty-print Gerbil expressions using Gambit's pretty-print.
- **`gerbil_benchmark`**: Measure wall-clock time, CPU time, GC stats, and memory allocation.

### For Navigation and Discovery

- **`gerbil_document_symbols`**: List all definitions in a file with name, kind, and line number.
- **`gerbil_workspace_symbols`**: Search for symbol definitions across all `.ss` files in a project directory.
- **`gerbil_find_callers`**: Find all files in a directory that reference a given symbol, with line numbers.
- **`gerbil_suggest_imports`**: Discover which standard library module exports a given symbol.
- **`gerbil_call_graph`**: Static call graph analysis — see which functions call which in a source file.
- **`gerbil_check_balance`**: Fast paren/bracket/brace balance checking without spawning a subprocess.
- **`gerbil_read_forms`**: Read a file with the actual Gerbil reader and see each form's line range and summary.
- **`gerbil_version`**: Check Gerbil/Gambit versions, installation path, and system type.

### For Refactoring and Code Generation

- **`gerbil_rename_symbol`**: Rename a symbol across all `.ss` files in a project with word-boundary safety. Dry-run by default.
- **`gerbil_generate_module_stub`**: Generate a module skeleton matching another module's exported signatures.
- **`gerbil_generate_module`**: Create new modules by applying word-boundary-aware substitutions to a template file. Useful for mechanical variations of an existing module pattern.
- **`gerbil_scaffold_test`**: Generate a `:std/test` skeleton from a module's exports.

### For Project Context

- **`gerbil_project_info`**: Single-call project summary: package name, build targets, source files, and external dependencies.
- **`gerbil_project_map`**: Complete view of all modules with their exports, definitions by kind, and import dependencies.

### For Recipes and Idioms

- **`gerbil_howto`**: Search curated Gerbil idioms and code examples by keyword. Covers common tasks like file I/O, JSON, HTTP, hash tables, iteration, error handling, concurrency, testing, and more. Use this **before writing Gerbil code** for common patterns — it returns verified, working examples with correct imports. The server automatically loads both built-in recipes and any accumulated recipes from previous sessions.
  - Example queries: `"json parse"`, `"file read"`, `"channel thread"`, `"hash iterate"`, `"error handling"`
  - Optionally pass `cookbook_path` to merge in additional project-specific recipes from a JSON file.
- **`gerbil_howto_add`**: Save new Gerbil recipes directly to the gerbil-mcp server's cookbook. No path needed — recipes are written to the server's own `cookbooks.json` and become available to all future sessions across all projects. Each recipe needs an `id` (kebab-case), `title`, `tags` (search keywords), `imports`, and `code`. Optional: `notes` and `related` recipe IDs.
  - If a recipe with the same `id` already exists, it is replaced (update semantics)
  - Optionally pass `cookbook_path` to write to a different file instead.
- **`gerbil_file_summary`**: Quick structural overview of a `.ss` file — imports, exports, and definitions grouped by kind — without reading the entire file. Faster than `gerbil_load_file` when you only need the shape of a module.

### For Feature Suggestions

- **`gerbil_list_features`**: Search or list existing feature suggestions for the gerbil-mcp toolset. Use before suggesting a new feature to check for duplicates.
- **`gerbil_suggest_feature`**: Submit a feature suggestion for a new or improved gerbil-mcp tool. Requires `id`, `title`, `description`, `impact` (high/medium/low), `tags`, `use_case`, `example_scenario`, and `estimated_token_reduction`. Suggestions are stored in the server's `features.json`.

### Key Principle

**Never guess — always verify.** If you are unsure about any Gerbil API, symbol name, module export, function arity, or macro behavior, use the appropriate MCP tool to check before writing code. This is not optional — it is required practice for all Gerbil work in this repository.

---

## MANDATORY: Save What You Learn to the Gerbil Cookbook

This is a **hard requirement**, not a suggestion. Gerbil is a niche language with limited training data. Every non-trivial pattern you discover and verify during a session is valuable knowledge that prevents future sessions from wasting time re-discovering the same thing.

### You MUST call `gerbil_howto_add` whenever you:

- Discover a working pattern through `gerbil_eval`, `gerbil_check_syntax`, or trial-and-error
- Figure out the correct imports, function names, or calling conventions for a module that wasn't obvious
- Find a workaround for a Gerbil quirk, gotcha, or undocumented behavior
- Combine multiple standard library functions to accomplish a common task
- Get something working that required reading source code or experimentation to figure out

### Before saving, first check `gerbil_howto` to avoid duplicates.

### Do NOT save:

- Trivial one-liners that any Scheme programmer would know (e.g., `(+ 1 2)`)
- Project-specific business logic that won't generalize
- Recipes that already exist in the cookbook (check first!)

### When saving, follow these rules:

- **`id`**: Use kebab-case (e.g., `"read-csv-file"`, `"channel-fan-out"`)
- **`tags`**: Include 4-6 search keywords. Think about what someone would type when looking for this pattern. Include the module name, the task, and alternative phrasings.
- **`imports`**: List all required imports. Use `[]` if none needed.
- **`code`**: Include a complete, working example — not a fragment. Someone should be able to copy-paste it.
- **`notes`**: Explain any non-obvious details, gotchas, or alternatives.

### This is not optional. If you solved a non-trivial Gerbil problem during this session and the pattern is reusable, you MUST save it before the session ends.

---

## Save Discoveries: Skill and Hook

There are two mechanisms for saving session discoveries — a manual `/save-discoveries` skill and an automatic PreCompact hook. Both save cookbook recipes AND feature suggestions in one pass.

### `/save-discoveries` Skill (Manual)

Install the skill by copying the `save-discoveries` directory from the gerbil-mcp repo into your project:

```sh
mkdir -p .claude/skills
cp -r /path/to/gerbil-mcp/.claude/skills/save-discoveries .claude/skills/
```

Or for personal scope (available in all projects):

```sh
cp -r /path/to/gerbil-mcp/.claude/skills/save-discoveries ~/.claude/skills/
```

Then invoke at any time during a session:

```
/save-discoveries
```

This triggers Claude to review the session and:
1. Save non-trivial Gerbil patterns as cookbook recipes via `gerbil_howto_add`
2. Submit tooling improvement ideas via `gerbil_suggest_feature`
3. Check for duplicates before saving either

### PreCompact Hook (Automatic)

To automatically save discoveries before context compaction, add this to your project's `.claude/settings.json`:

```json
{
  "hooks": {
    "PreCompact": [
      {
        "matcher": "auto",
        "hooks": [
          {
            "type": "prompt",
            "prompt": "MANDATORY PRE-COMPACTION ACTION: Before context is compacted, you MUST: (1) Save any non-trivial Gerbil Scheme patterns, workarounds, API discoveries, or gotchas. For each: call gerbil_howto to check for duplicates, then gerbil_howto_add with a complete working code example, correct imports, and clear notes. Focus on: wrong assumptions corrected, arity/signature discoveries, compilation workarounds, idiom patterns that required multiple attempts. (2) Save any tooling improvement ideas. For each: call gerbil_list_features to check for duplicates, then gerbil_suggest_feature with impact, use case, and token reduction estimate. Skip trivial items in both categories. This MUST happen NOW before context is lost."
          }
        ]
      },
      {
        "matcher": "manual",
        "hooks": [
          {
            "type": "prompt",
            "prompt": "Before compacting: save any unsaved Gerbil discoveries to the cookbook via gerbil_howto_add and suggest any tooling improvements via gerbil_suggest_feature. Check for duplicates first with gerbil_howto and gerbil_list_features."
          }
        ]
      }
    ]
  }
}
```

### How It Works

- **Auto compaction** (`matcher: "auto"`): When the context window fills up, a prompt directive is injected that the main agent sees and acts on — calling both cookbook and feature suggestion tools with full MCP tool access before compaction proceeds. Note: `type: "agent"` does NOT work for PreCompact (agent hooks only support decision-gate events like PreToolUse, PostToolUse, etc.). Use `type: "prompt"` instead.
- **Manual compaction** (`matcher: "manual"`): When you run `/compact`, a prompt reminder is injected so Claude saves discoveries before summarizing.

### Why This Matters

Gerbil is a niche language with limited training data. Long sessions often discover critical patterns (e.g., `hash-get` is strictly 2-arity, `call-with-getopt` changed its API in v0.19+, `for/in-hash` fails during compilation). Without these hooks, discoveries are lost when context compacts, and future sessions waste tokens re-discovering the same things. Feature suggestions similarly capture tooling gaps while the friction is fresh.

---

## MANDATORY: Suggest Tooling Improvements

When you notice opportunities to improve the gerbil-mcp toolset during a session, you MUST record them via `gerbil_suggest_feature`. This helps the toolset evolve based on real usage patterns.

### You MUST call `gerbil_suggest_feature` whenever you:

- Find yourself making multiple sequential tool calls that could be a single tool
- Encounter a workflow where a missing tool forces you to fall back to `gerbil_eval` or bash
- Notice repeated patterns across sessions that could be automated
- Discover that an existing tool is missing a useful parameter or mode

### Before suggesting, first check `gerbil_list_features` to avoid duplicates.

### When suggesting, fill in all fields honestly:

- **`id`**: Use kebab-case (e.g., `"batch-module-check"`, `"cross-file-rename"`)
- **`impact`**: `"high"` = saves many tokens or round-trips regularly; `"medium"` = useful but less frequent; `"low"` = nice-to-have
- **`tags`**: Include 3-5 search keywords for discoverability
- **`estimated_token_reduction`**: Be specific (e.g., `"~500 tokens per invocation"`, `"eliminates 3 tool calls"`)

---

## Workflow Conventions

When implementing new features, always complete the documentation update in the same session. Document non-trivial solutions as howto recipes in the cookbook system.

## Language-Specific Notes

When editing Gerbil Scheme, be careful with function naming conventions (e.g., `md5` not `md5sum`) and avoid introducing duplicate definitions across modules.
