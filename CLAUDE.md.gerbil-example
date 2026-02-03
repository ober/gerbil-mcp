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
- **`gerbil_run_tests`**: Execute a single `:std/test` file (`file_path`) or run project-wide tests (`directory`). Use `filter` to match test names, `quiet` for errors-only output.
- **`gerbil_package_info`**: List installed packages, search the package directory, or view metadata.
- **`gerbil_package_manage`**: Install, update, or uninstall Gerbil packages.
- **`gerbil_scaffold`**: Create a new Gerbil project from a template using gxpkg new.

### For Performance Analysis

- **`gerbil_profile`**: Instrument specific functions with call counting and timing. Reports per-function stats (calls, time, avg, %) plus overall wall/CPU/GC time and allocation.
- **`gerbil_heap_profile`**: Capture GC heap metrics before/after an expression. Reports heap size, allocation, live/movable/still objects with deltas.
- **`gerbil_trace_calls`**: Lightweight call counting (no timing overhead). Count how many times specified functions are called.

### For Code Quality

- **`gerbil_lint`**: Static analysis for common issues: unused imports, duplicate definitions, style warnings, shadowed bindings, hash literal symbol key warnings, and compilation errors.
- **`gerbil_diagnostics`**: Run `gxc -S` on a file or project and get structured diagnostics with file, line, column, severity, and message.
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

### For Refactoring

- **`gerbil_rename_symbol`**: Rename a symbol across all `.ss` files in a project with word-boundary safety. Dry-run by default.

### For Project Context

- **`gerbil_project_info`**: Single-call project summary: package name, build targets, source files, and external dependencies.
- **`gerbil_project_map`**: Complete view of all modules with their exports, definitions by kind, and import dependencies.

### Key Principle

**Never guess — always verify.** If you are unsure about any Gerbil API, symbol name, module export, function arity, or macro behavior, use the appropriate MCP tool to check before writing code. This is not optional — it is required practice for all Gerbil work in this repository.
