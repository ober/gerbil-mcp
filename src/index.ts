#!/usr/bin/env node

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { registerEvalTool } from './tools/eval.js';
import { registerModuleExportsTool } from './tools/module-exports.js';
import { registerCheckSyntaxTool } from './tools/check-syntax.js';
import { registerExpandMacroTool } from './tools/expand-macro.js';
import { registerAproposTool } from './tools/apropos.js';
import { registerListModulesTool } from './tools/list-modules.js';
import { registerFunctionSignatureTool } from './tools/function-signature.js';
import { registerModuleDepsTool } from './tools/module-deps.js';
import { registerLoadFileTool } from './tools/load-file.js';
import { registerDocTool } from './tools/doc.js';
import { registerCompileCheckTool } from './tools/compile-check.js';
import { registerTraceMacroTool } from './tools/trace-macro.js';
import { registerReplSessionTool } from './tools/repl-session.js';
import { registerRunTestsTool } from './tools/run-tests.js';
import { registerFfiInspectTool } from './tools/ffi-inspect.js';
import { registerClassInfoTool } from './tools/class-info.js';
import { registerFindDefinitionTool } from './tools/find-definition.js';
import { registerBuildProjectTool } from './tools/build-project.js';
import { registerPackageInfoTool } from './tools/package-info.js';
import { registerFormatTool } from './tools/format.js';
import { registerBenchmarkTool } from './tools/benchmark.js';
import { registerErrorHierarchyTool } from './tools/error-hierarchy.js';
import { registerVersionTool } from './tools/version.js';
import { registerScaffoldTool } from './tools/scaffold.js';
import { registerPackageManageTool } from './tools/package-manage.js';
import { registerFindCallersTool } from './tools/find-callers.js';
import { registerSuggestImportsTool } from './tools/suggest-imports.js';
import { registerDiagnosticsTool } from './tools/diagnostics.js';
import { registerDocumentSymbolsTool } from './tools/document-symbols.js';
import { registerWorkspaceSymbolsTool } from './tools/workspace-symbols.js';
import { registerRenameSymbolTool } from './tools/rename-symbol.js';
import { registerLintTool } from './tools/lint.js';
import { registerProjectInfoTool } from './tools/project-info.js';
import { registerProjectMapTool } from './tools/project-map.js';
import { registerCheckBalanceTool } from './tools/check-balance.js';
import { registerReadFormsTool } from './tools/read-forms.js';
import { registerProfileTool } from './tools/profile.js';
import { registerHeapProfileTool } from './tools/heap-profile.js';
import { registerTraceCallsTool } from './tools/trace-calls.js';
import { registerCallGraphTool } from './tools/call-graph.js';
import { registerScaffoldTestTool } from './tools/scaffold-test.js';
import { registerBuildAndReportTool } from './tools/build-and-report.js';
import { registerGenerateModuleStubTool } from './tools/generate-module-stub.js';
import { registerCheckExportsTool } from './tools/check-exports.js';
import { registerGenerateModuleTool } from './tools/generate-module.js';
import { registerHowtoTool } from './tools/howto.js';
import { registerHowtoAddTool } from './tools/howto-add.js';
import { registerFileSummaryTool } from './tools/file-summary.js';
import { registerMakeTool } from './tools/make.js';
import { registerCheckArityTool } from './tools/check-arity.js';
import { registerCheckTestArityTool } from './tools/check-test-arity.js';
import { registerHowtoVerifyTool } from './tools/howto-verify.js';
import { registerResolveImportsTool } from './tools/resolve-imports.js';
import { registerTraceEvalTool } from './tools/trace-eval.js';
import { registerSxmlInspectTool } from './tools/sxml-inspect.js';
import { registerSuggestFeatureTool } from './tools/suggest-feature.js';
import { registerListFeaturesTool } from './tools/list-features.js';
import { registerVoteFeatureTool } from './tools/vote-feature.js';
import { registerDemangleTool } from './tools/demangle.js';
import { registerStaleStaticTool } from './tools/stale-static.js';
import { registerBalancedReplaceTool } from './tools/balanced-replace.js';
import { registerWrapFormTool } from './tools/wrap-form.js';
import { registerSpliceFormTool } from './tools/splice-form.js';
import { registerFfiScaffoldTool } from './tools/ffi-scaffold.js';
import { registerProjectDepGraphTool } from './tools/project-dep-graph.js';
import { registerTestCoverageTool } from './tools/test-coverage.js';
import { registerModuleCatalogTool } from './tools/module-catalog.js';
import { registerFfiCallbackDebugTool } from './tools/ffi-callback-debug.js';
import { registerExampleApiCoverageTool } from './tools/example-api-coverage.js';
import { registerValidateExampleImportsTool } from './tools/validate-example-imports.js';
import { registerBisectCrashTool } from './tools/bisect-crash.js';
import { registerCheckImportConflictsTool } from './tools/check-import-conflicts.js';
import { registerSecurityScanTool } from './tools/security-scan.js';
import { registerSecurityPatternAddTool } from './tools/security-pattern-add.js';
import { registerHowtoGetTool } from './tools/howto-get.js';
import { registerStaleLinkedPkgTool } from './tools/stale-linked-pkg.js';
import { registerFfiTypeCheckTool } from './tools/ffi-type-check.js';
import { registerDescribeTool } from './tools/describe.js';
import { registerSmartCompleteTool } from './tools/smart-complete.js';
import { registerExplainErrorTool } from './tools/explain-error.js';
import { registerDiffModulesTool } from './tools/diff-modules.js';
import { registerMigrationCheckTool } from './tools/migration-check.js';
import { registerDeadCodeTool } from './tools/dead-code.js';
import { registerDependencyCyclesTool } from './tools/dependency-cycles.js';
import { registerGenerateApiDocsTool } from './tools/generate-api-docs.js';
import { registerFfiNullSafetyTool } from './tools/ffi-null-safety.js';
import { registerMethodDispatchAuditTool } from './tools/method-dispatch-audit.js';
import { registerFfiBufferSizeAuditTool } from './tools/ffi-buffer-size-audit.js';
import { registerFfiUtf8ByteLengthTool } from './tools/ffi-utf8-byte-length.js';
import { registerStackTraceDecodeTool } from './tools/stack-trace-decode.js';
import { registerTailPositionCheckTool } from './tools/tail-position-check.js';
import { registerModuleQuickstartTool } from './tools/module-quickstart.js';
import { registerDynamicReferenceTool } from './tools/dynamic-reference.js';
import { registerProjectHealthCheckTool } from './tools/project-health-check.js';
import { registerInterfaceComplianceCheckTool } from './tools/interface-compliance-check.js';
import { registerReturnTypeAnalysisTool } from './tools/return-type-analysis.js';
import { registerHttpdHandlerScaffoldTool } from './tools/httpd-handler-scaffold.js';
import { registerParserGrammarScaffoldTool } from './tools/parser-grammar-scaffold.js';
import { registerActorEnsembleScaffoldTool } from './tools/actor-ensemble-scaffold.js';
import { registerEventSystemGuideTool } from './tools/event-system-guide.js';
import { registerMacroHygieneCheckTool } from './tools/macro-hygiene-check.js';
import { registerConcurrentPlanValidateTool } from './tools/concurrent-plan-validate.js';
import { registerTestFixtureGenTool } from './tools/test-fixture-gen.js';
import { registerDbPatternScaffoldTool } from './tools/db-pattern-scaffold.js';
import { registerGracefulShutdownScaffoldTool } from './tools/graceful-shutdown-scaffold.js';
import { registerVerifyTool } from './tools/verify.js';
import { registerStdlibSourceTool } from './tools/stdlib-source.js';
import { registerHowtoRunTool } from './tools/howto-run.js';
import { registerFunctionBehaviorTool } from './tools/function-behavior.js';
import { registerTranslateSchemeTool } from './tools/translate-scheme.js';
import { registerProjectTemplateTool } from './tools/project-template.js';
import { registerErrorFixLookupTool, registerErrorFixAddTool } from './tools/error-fix.js';
import { registerCheckDuplicatesTool } from './tools/check-duplicates.js';
import { registerBuildChainTool } from './tools/build-chain.js';
import { registerFFILinkCheckTool } from './tools/ffi-link-check.js';
import { registerBatchSyntaxCheckTool } from './tools/batch-syntax-check.js';
import { registerPreflightCheckTool } from './tools/preflight-check.js';
import { registerBuildLinkageDiagnosticTool } from './tools/build-linkage-diagnostic.js';
import { registerCrossModuleCheckTool } from './tools/cross-module-check.js';
import { registerDetectIfdefStubsTool } from './tools/detect-ifdef-stubs.js';
import { registerQtTestRunnerTool } from './tools/qt-test-runner.js';
import { registerPkgLinkSyncTool } from './tools/pkg-link-sync.js';
import { registerCrossPackageDiffTool } from './tools/cross-package-diff.js';
import { registerDispatchCoverageTool } from './tools/dispatch-coverage.js';
import { registerMacroPatternDetectorTool } from './tools/macro-pattern-detector.js';
import { registerBoilerplateConverterTool } from './tools/boilerplate-converter.js';
import { registerSignalTraceTool } from './tools/signal-trace.js';
import { registerPrompts } from './prompts.js';
import { registerResources } from './resources.js';

const INSTRUCTIONS = `You have access to a live Gerbil Scheme environment via this MCP server. Use these tools proactively when working with Gerbil Scheme code:

## Essential Tools (Always Use)

- BEFORE writing ANY Gerbil code: FIRST use gerbil_howto to search the cookbook for relevant patterns. The cookbook contains 394+ verified, working examples with correct imports, arities, and keyword conventions — accumulated from real debugging sessions. Many bugs (wrong arity, missing parent arg, keyword vs positional) are already documented here. Search with the widget/module/task name (e.g. "dialog create", "layout parent", "hash iterate", "json parse"). Skipping this step has repeatedly caused bugs that were already solved in the cookbook.
- BEFORE finalizing Gerbil code involving FFI, shell commands, file I/O, or C shims: run gerbil_security_scan on the file or project. It checks for known vulnerability patterns (shell injection, FFI type mismatches, resource leaks, unsafe C patterns) and reports findings with severity and remediation. Skipping this step risks shipping code with known vulnerability patterns.
- BEFORE writing Gerbil code: use gerbil_module_exports to check what a module actually exports, rather than guessing function names or signatures. Use loadpath or project_path to resolve project-local dependency modules.
- BEFORE suggesting Gerbil code: use gerbil_check_syntax to verify your code is syntactically valid. Use gerbil_batch_syntax_check to verify multiple snippets in one call.
- BEFORE calling Gerbil functions: use gerbil_function_signature to check procedure arities and keyword arguments, avoiding wrong number of arguments errors. Use loadpath or project_path for dependency modules.
- When UNSURE about Gerbil behavior: use gerbil_eval to test expressions and verify your assumptions. Use loadpath or project_path to import project-local modules. Use env parameter for FFI library paths (e.g. DYLD_LIBRARY_PATH).
- To catch compilation errors: use gerbil_compile_check to run gxc and detect unbound identifiers and type issues. Use loadpath for project context. Combines stdout/stderr for complete error output. Enhanced error messages help diagnose internal compiler crashes.
- To build with diagnostics: use gerbil_build_and_report to run \`gerbil build\` and get structured error diagnostics with file, line, column. Prefer this over running \`gerbil build\` via bash for better error reporting. Auto-detects external dependencies from gerbil.pkg depend: entries and sets GERBIL_LOADPATH automatically. Auto-retries with clean on lock errors or missing exe C files. Use modules_only: true to skip exe linking targets and only compile library modules (dramatically faster when iterating on code).
- To run test suites: use gerbil_run_tests to execute a single :std/test file (file_path) or run project-wide tests (directory). Use filter to match test names, quiet for errors-only output. Auto-detects GERBIL_LOADPATH from gerbil.pkg depend: entries. Use env parameter for FFI library paths.
- To analyze test coverage for command sequences: use gerbil_dispatch_coverage_analysis to detect gaps in functional test suites. Identifies commands tested individually but never in combination, helping catch state management bugs.
- To detect repetitive code patterns: use gerbil_macro_pattern_detector to find boilerplate that could be replaced with macros. Identifies repeated functions, hash accessors, and method wrappers.
- To convert boilerplate to macros: use gerbil_boilerplate_converter with 2+ similar expressions to generate a macro definition automatically. Extracts the pattern and creates defrule with invocations.
- To debug signal handling: use gerbil_signal_trace to generate instrumentation code for tracing signal delivery and trap execution. Logs when signals are received, handlers execute, and exceptions occur.
- When looking up any symbol: use gerbil_doc to get type, arity, qualified name, and related symbols.
- To describe a value: use gerbil_describe to evaluate an expression and get a detailed description of the resulting value's type, structure, and contents. Useful for understanding what functions return or what data structures contain.

## Common Tools (Use Frequently)

- To get smart completions: use gerbil_smart_complete with a partial symbol prefix and optional module list. Returns valid completions scoped to imported modules, dramatically reducing guessing.
- To debug errors: use gerbil_explain_error with an error message for automated classification, cause analysis, relevant cookbook recipes, and suggested tool calls to investigate.
- When debugging Gerbil code: use gerbil_eval to reproduce and isolate issues. Use loadpath or project_path for project context. Use env parameter for FFI library paths.
- To trace let bindings: use gerbil_trace_eval to step through let*/let/letrec/letrec* bindings, showing each variable's name, type, and value as it is bound.
- To inspect SXML trees: use gerbil_sxml_inspect to parse XML text or evaluate an SXML expression and display the tree structure with labeled node types (DOCUMENT, PI, ELEMENT, ATTR, TEXT).
- When exploring unfamiliar Gerbil APIs: use gerbil_apropos to search for relevant symbols, gerbil_module_exports to see what's available, and gerbil_list_std_modules to discover modules.
- When understanding macros: use gerbil_expand_macro to see what sugar forms expand to.
- When understanding module structure: use gerbil_module_deps to see what a module imports and depends on.
- When analyzing user code: use gerbil_load_file to extract definitions from Gerbil source files.
- To understand complex macros: use gerbil_trace_macro for step-by-step expansion showing each transformation.
- For multi-step exploration: use gerbil_repl_session to maintain persistent state across evaluations. Use project_path or loadpath on create to work within a project's build context. Use env parameter for FFI library paths.
- To examine C bindings: use gerbil_ffi_inspect to classify a module's FFI exports (constants, C functions, wrappers).
- To inspect types: use gerbil_class_info to examine defclass/defstruct types (slots, fields, inheritance, precedence).
- To find where a symbol is defined: use gerbil_find_definition to locate the source file and module for any symbol. Set source_preview: true to include the actual source code. Automatically resolves standard library source files via the Gerbil installation's src/ tree (lib/ → src/ path rewrite).
- To lint code: use gerbil_lint for static analysis (unused imports, duplicates, style, hash literal symbol keys, channel anti-patterns, unquote outside quasiquote, dot in brackets, missing exported definitions with re-export awareness, SRFI-19 time->seconds shadow, unsafe mutex-lock!/unlock! without unwind-protect, byte/char port type mismatch, compilation errors).
- To get a structural file overview: use gerbil_file_summary for imports, exports, and definitions grouped by kind — without reading the entire file.
- To get project overview: use gerbil_project_info for package name, build targets, source files, and dependencies.
- To find imports: use gerbil_suggest_imports to discover which module exports a given symbol.
- To find code recipes: use gerbil_howto to search curated Gerbil idioms and code examples by keyword (e.g. "json parse", "file read", "channel thread"). Use compact: true for a brief listing, then gerbil_howto_get to fetch full recipe by id.
- To fetch a recipe by id: use gerbil_howto_get with a recipe id to retrieve the full code, imports, and notes.
- To save a new Gerbil recipe: use gerbil_howto_add to append idioms discovered during a session. Use gerbil_version to tag version-specific recipes.
- To get a module catalog: use gerbil_module_catalog for a compact reference of all exports from a module with kind, arity, and brief descriptions. Has curated descriptions for :std/sugar, :std/iter. Replaces multiple gerbil_doc calls.

## Specialized Tools

- To compare module exports: use gerbil_diff_modules to see added/removed/shared exports between two modules. Critical for version migration.
- To compare function signatures across packages: use gerbil_cross_package_diff to see arity/kind differences for shared exports, symbols only in module A, and symbols only in module B. Critical for debugging wrapper/wrapped function mismatches (e.g. a shim with different parameter defaults).
- To check migration compatibility: use gerbil_migration_check to scan a file for v0.18 patterns that need updating for v0.19.
- To detect dead code: use gerbil_dead_code to find unexported, uncalled definitions across a project.
- To detect circular dependencies: use gerbil_dependency_cycles to find circular module imports that cause compilation errors.
- To generate API docs: use gerbil_generate_api_docs to produce markdown documentation from a module's exports with arities and types.
- To build a Gerbil project: use gerbil_build_project to compile or clean a project directory using gxpkg.
- To explore packages: use gerbil_package_info to list installed packages, search the package directory, or view package metadata.
- To format Gerbil code: use gerbil_format to pretty-print expressions using Gambit's pretty-print.
- To benchmark expressions: use gerbil_benchmark to measure wall-clock time, CPU time, GC stats, and memory allocation.
- To understand error types: use gerbil_error_hierarchy to see the full exception/error class hierarchy tree.
- To check environment: use gerbil_version to see Gerbil/Gambit versions, home directory, and system info.
- To diagnose MCP issues: use gerbil_preflight_check to verify server prerequisites (gxi/gxc availability, Gerbil version, GERBIL_HOME/PATH, dist/ folder, basic eval). Use when tools are not working.
- To create a new project: use gerbil_scaffold to generate a project template with gerbil.pkg and build.ss.
- To manage packages: use gerbil_package_manage to install, update, or uninstall Gerbil packages.
- To find symbol usages: use gerbil_find_callers to search a directory for files that reference a given symbol.
- To get structured diagnostics: use gerbil_diagnostics for file/project compilation diagnostics with file, line, column, severity.
- To list symbols in a file: use gerbil_document_symbols for all definitions with name, kind, and line number.
- To search project symbols: use gerbil_workspace_symbols to find definitions matching a query across all project files.
- To rename a symbol: use gerbil_rename_symbol for project-wide rename with dry-run safety (default). Supports single-file mode via file_path parameter for local renames with word-boundary safety.
- To map project exports: use gerbil_project_map for a complete view of all modules with their exports, definitions by kind, and import dependencies.
- To check delimiter balance: use gerbil_check_balance for fast paren/bracket/brace balance checking without spawning a subprocess.
- To list top-level forms: use gerbil_read_forms to read a file with the actual Gerbil reader and see each form's line range and summary.
- To profile function performance: use gerbil_profile to instrument specific functions with call counting and timing.
- To analyze memory usage: use gerbil_heap_profile to capture GC heap metrics before and after running an expression.
- To count function calls: use gerbil_trace_calls for lightweight call counting without timing overhead.
- To visualize call relationships: use gerbil_call_graph to see which functions call which in a source file (static analysis).
- To scaffold tests: use gerbil_scaffold_test to generate a :std/test skeleton from a module's exports.
- To generate module stubs: use gerbil_generate_module_stub to create a module skeleton matching another module's exported signatures.
- To check export consistency: use gerbil_check_exports to verify that exports match definitions and cross-module imports are consistent.
- To generate modules from templates: use gerbil_generate_module to create new modules by applying substitutions to a template.
- To run Makefile targets: use gerbil_make to run make targets in a Gerbil project directory.
- To check call-site arity: use gerbil_check_arity to detect functions called with the wrong number of arguments.
- To find tests affected by signature changes: use gerbil_check_test_arity to scan *-test.ss files for calls to a specific function.
- To verify cookbook recipes: use gerbil_howto_verify to check that cookbook recipes have valid syntax and imports.
- To resolve missing imports: use gerbil_resolve_imports to analyze a file for unbound identifiers and generate a suggested import block.
- To suggest a new feature: use gerbil_suggest_feature to write a feature suggestion for future consideration.
- To check existing feature suggestions: use gerbil_list_features to search or list existing feature suggestions.
- To vote for a feature: use gerbil_vote_feature to increment the vote count for a feature you could have used.
- To decode mangled C symbols: use gerbil_demangle to convert Gambit-mangled C identifiers back to readable module/function paths.
- To detect stale build artifacts: use gerbil_stale_static to compare global vs project-local static files.
- For balance-safe editing: use gerbil_balanced_replace instead of string replace. Validates delimiter balance before and after.
- To wrap code in a form: use gerbil_wrap_form to wrap lines in a new Scheme form with guaranteed matching parentheses.
- To unwrap/splice a form: use gerbil_splice_form to remove a wrapper form while keeping selected children.
- To generate FFI bindings: use gerbil_ffi_scaffold to parse a C header file and generate Gambit FFI binding code.
- To visualize project dependencies: use gerbil_project_dep_graph to see module dependency tree.
- To check test coverage: use gerbil_test_coverage to compare a module's exports against its test file.
- To debug FFI callbacks: use gerbil_ffi_callback_debug to analyze c-define/extern linkage.
- To check example API coverage: use gerbil_example_api_coverage to see which exports are referenced in example files.
- To validate example imports: use gerbil_validate_example_imports to check that imports match used symbols.
- To bisect crashes: use gerbil_bisect_crash to binary-search a crashing file for minimal reproducing forms.
- To detect import conflicts: use gerbil_check_import_conflicts to find clashing symbol definitions before building.
- To scan for security issues: use gerbil_security_scan for vulnerability pattern detection in Gerbil and C code.
- To add security patterns: use gerbil_security_pattern_add to contribute new detection rules.
- To detect stale linked packages: use gerbil_stale_linked_pkg to check if linked packages need rebuilding.
- To check FFI type safety: use gerbil_ffi_type_check to detect type mismatches in c-lambda declarations.
- To check FFI link symbols: use gerbil_ffi_link_check to cross-reference C function calls in c-declare blocks against symbols in linked .a static libraries via nm. Catches missing library links before build-test cycle.
- To audit FFI null safety: use gerbil_ffi_null_safety to find c-lambda pointer dereferences without null checks.
- To audit FFI buffer sizes: use gerbil_ffi_buffer_size_audit to detect buffer overflows in FFI bindings.
- To audit method dispatch: use gerbil_method_dispatch_audit to find {method obj} calls that may fail at runtime.
- To decode stack traces: use gerbil_stack_trace_decode to parse GDB/Gambit backtraces into readable form.
- To check tail positions: use gerbil_tail_position_check to verify recursive calls are in tail position.
- To explore a module: use gerbil_module_quickstart to generate a working example for any stdlib module.
- To generate module docs: use gerbil_dynamic_reference for auto-generated API reference for any module.
- To audit project health: use gerbil_project_health_check for a composite quality audit in one call.
- To check interfaces: use gerbil_interface_compliance_check to verify struct/class implements required methods.
- To analyze return types: use gerbil_return_type_analysis to detect gotcha return values (void, hash-ref, when).
- To scaffold HTTP servers: use gerbil_httpd_handler_scaffold to generate :std/net/httpd server code.
- To scaffold parsers: use gerbil_parser_grammar_scaffold to generate :std/parser lexer and grammar code.
- To scaffold actor systems: use gerbil_actor_ensemble_scaffold to generate distributed actor projects.
- To explore events: use gerbil_event_system_guide for sync/select/choice patterns from :std/event.
- To check macro hygiene: use gerbil_macro_hygiene_check to detect variable capture in macro definitions.
- To validate DAG plans: use gerbil_concurrent_plan_validate for :std/misc/concurrent-plan DAG validation.
- To generate test fixtures: use gerbil_test_fixture_gen to create mock modules and test setup with parameterize.
- To scaffold database access: use gerbil_db_pattern_scaffold to generate CRUD with connection pooling and transactions.
- To scaffold graceful shutdown: use gerbil_graceful_shutdown_scaffold for signal handling and cleanup patterns.
- To verify code in one pass: use gerbil_verify to run syntax check, compile check, lint, and arity check in a single call. Replaces sequential check_syntax → compile_check → lint → check_arity workflow. Now includes duplicate definition detection.
- To check for duplicate definitions: use gerbil_check_duplicates for fast pre-build scanning of duplicate top-level defs (def, defmethod, defrule, etc.). Reports line numbers for both original and duplicate. Catches "Bad binding; rebind conflict" before compilation.
- To build multi-project dependencies: use gerbil_build_chain to build a chain of dependent Gerbil projects in dependency order. Reads gerbil.pkg depend: and GERBIL_LOADPATH from Makefile to find upstream projects, checks if they need rebuilding, and builds them before the target. Use dry_run to preview.
- To read stdlib source: use gerbil_stdlib_source to read the full source code of any standard library module by its module path.
- To test a cookbook recipe: use gerbil_howto_run to compile-check and optionally execute a recipe in the current environment.
- To probe function behavior: use gerbil_function_behavior to generate a behavior card showing return values for normal and edge cases (missing keys, empty lists, out of bounds, etc.).
- To translate Scheme to Gerbil: use gerbil_translate_scheme to mechanically convert R7RS or Racket code to idiomatic Gerbil with semantic warnings.
- To generate project structure: use gerbil_project_template to create a complete multi-file project from a template (cli, http-api, library, actor-service, db-crud, parser, ffi-wrapper, test-project).
- To look up error fixes: use gerbil_error_fix_lookup for instant fix lookup from a database of ~20 common error→fix mappings. Much faster than explain_error for known errors.
- To add error fixes: use gerbil_error_fix_add to record new error→fix mappings discovered during a session.
- To diagnose exe link failures: use gerbil_build_linkage_diagnostic to trace transitive FFI link dependencies in build.ss exe targets. Detects missing C libraries that would cause silent link failures.
- To check cross-module symbols: use gerbil_cross_module_check to detect unbound symbol references across project files before compilation. Critical when splitting large modules into sub-modules.
- To find #ifdef stubs: use gerbil_detect_ifdef_stubs to scan c-declare blocks for #ifdef/#else stub patterns (NULL/0 returns) that cause segfaults in cross-project builds.
- To run Qt FFI tests: use gerbil_qt_test_runner to build, patchelf, and run a Qt exe test in one step with QT_QPA_PLATFORM=offscreen.
- To sync linked package artifacts: use gerbil_pkg_link_sync to detect and copy stale .ssi/.so/.scm files from a linked package's local build to the global ~/.gerbil/lib/.

## Common Workflows

- **Debug a segfault**: gerbil_stale_static → gerbil_bisect_crash → gerbil_demangle → gerbil_ffi_type_check
- **Add a feature**: gerbil_howto → write code → gerbil_check_syntax → gerbil_compile_check → gerbil_build_and_report
- **Understand unfamiliar code**: gerbil_file_summary → gerbil_document_symbols → gerbil_call_graph → gerbil_module_deps
- **Port from another Scheme**: gerbil_howto → gerbil_suggest_imports → gerbil_module_exports → gerbil_check_syntax
- **Refactor a module**: gerbil_check_exports → gerbil_find_callers → gerbil_rename_symbol → gerbil_check_import_conflicts
- **Migrate between versions**: gerbil_migration_check → gerbil_diff_modules → gerbil_howto "v0.19" → gerbil_compile_check
- **Debug an error**: gerbil_explain_error → follow suggested tools → gerbil_howto for fix patterns
- **Write a new module**: gerbil_howto → gerbil_module_exports (check APIs) → write code → gerbil_lint → gerbil_security_scan
- **Audit FFI safety**: gerbil_ffi_null_safety → gerbil_ffi_buffer_size_audit → gerbil_ffi_type_check → gerbil_security_scan
- **Explore unknown module**: gerbil_module_quickstart → gerbil_dynamic_reference → gerbil_howto
- **Build a service**: gerbil_httpd_handler_scaffold → gerbil_db_pattern_scaffold → gerbil_graceful_shutdown_scaffold
- **Project quality audit**: gerbil_project_health_check → fix issues → gerbil_security_scan
- **Quick verify**: gerbil_verify → fix all issues at once → gerbil_verify again to confirm clean
- **Port from Racket**: gerbil_translate_scheme → gerbil_verify → gerbil_suggest_imports → manual review
- **Fix common error**: gerbil_error_fix_lookup → apply fix → gerbil_verify
- **Start new project**: gerbil_project_template → gerbil build → make test
- **Diagnose exe build issues**: gerbil_build_linkage_diagnostic → gerbil_detect_ifdef_stubs → gerbil_ffi_link_check → gerbil_stale_static
- **Split a module**: gerbil_cross_module_check → fix missing imports → gerbil_check_import_conflicts → gerbil_verify

## Important Guidance

- Don't use gerbil_eval for syntax checking — use gerbil_check_syntax instead
- Don't guess function names — use gerbil_module_exports to verify
- Don't assume arity — use gerbil_function_signature to check
- Don't skip the cookbook — use gerbil_howto before writing code

## Troubleshooting

- Tool returns empty results → check loadpath, check module path spelling, ensure module is installed
- Compile check passes but build fails → check for stale artifacts with gerbil_stale_static
- REPL session hangs → destroy and recreate, check for infinite loops
- Module not found → check GERBIL_LOADPATH, use gerbil_list_std_modules to discover available modules

Gerbil is a niche Scheme dialect — your training data is limited. Always verify with these tools rather than guessing.`;

const server = new McpServer(
  { name: 'gerbil-mcp', version: '1.0.0' },
  { instructions: INSTRUCTIONS },
);

registerEvalTool(server);
registerModuleExportsTool(server);
registerCheckSyntaxTool(server);
registerExpandMacroTool(server);
registerAproposTool(server);
registerListModulesTool(server);
registerFunctionSignatureTool(server);
registerModuleDepsTool(server);
registerLoadFileTool(server);
registerDocTool(server);
registerCompileCheckTool(server);
registerTraceMacroTool(server);
registerReplSessionTool(server);
registerRunTestsTool(server);
registerFfiInspectTool(server);
registerClassInfoTool(server);
registerFindDefinitionTool(server);
registerBuildProjectTool(server);
registerPackageInfoTool(server);
registerFormatTool(server);
registerBenchmarkTool(server);
registerErrorHierarchyTool(server);
registerVersionTool(server);
registerScaffoldTool(server);
registerPackageManageTool(server);
registerFindCallersTool(server);
registerSuggestImportsTool(server);
registerDiagnosticsTool(server);
registerDocumentSymbolsTool(server);
registerWorkspaceSymbolsTool(server);
registerRenameSymbolTool(server);
registerLintTool(server);
registerProjectInfoTool(server);
registerProjectMapTool(server);
registerCheckBalanceTool(server);
registerReadFormsTool(server);
registerProfileTool(server);
registerHeapProfileTool(server);
registerTraceCallsTool(server);
registerCallGraphTool(server);
registerScaffoldTestTool(server);
registerBuildAndReportTool(server);
registerGenerateModuleStubTool(server);
registerCheckExportsTool(server);
registerGenerateModuleTool(server);
registerHowtoTool(server);
registerHowtoAddTool(server);
registerFileSummaryTool(server);
registerMakeTool(server);
registerCheckArityTool(server);
registerCheckTestArityTool(server);
registerHowtoVerifyTool(server);
registerResolveImportsTool(server);
registerTraceEvalTool(server);
registerSxmlInspectTool(server);
registerSuggestFeatureTool(server);
registerListFeaturesTool(server);
registerVoteFeatureTool(server);
registerDemangleTool(server);
registerStaleStaticTool(server);
registerBalancedReplaceTool(server);
registerWrapFormTool(server);
registerSpliceFormTool(server);
registerFfiScaffoldTool(server);
registerProjectDepGraphTool(server);
registerTestCoverageTool(server);
registerModuleCatalogTool(server);
registerFfiCallbackDebugTool(server);
registerExampleApiCoverageTool(server);
registerValidateExampleImportsTool(server);
registerBisectCrashTool(server);
registerCheckImportConflictsTool(server);
registerSecurityScanTool(server);
registerSecurityPatternAddTool(server);
registerHowtoGetTool(server);
registerStaleLinkedPkgTool(server);
registerFfiTypeCheckTool(server);
registerDescribeTool(server);
registerSmartCompleteTool(server);
registerExplainErrorTool(server);
registerDiffModulesTool(server);
registerMigrationCheckTool(server);
registerDeadCodeTool(server);
registerDependencyCyclesTool(server);
registerGenerateApiDocsTool(server);
registerFfiNullSafetyTool(server);
registerMethodDispatchAuditTool(server);
registerFfiBufferSizeAuditTool(server);
registerFfiUtf8ByteLengthTool(server);
registerStackTraceDecodeTool(server);
registerTailPositionCheckTool(server);
registerModuleQuickstartTool(server);
registerDynamicReferenceTool(server);
registerProjectHealthCheckTool(server);
registerInterfaceComplianceCheckTool(server);
registerReturnTypeAnalysisTool(server);
registerHttpdHandlerScaffoldTool(server);
registerParserGrammarScaffoldTool(server);
registerActorEnsembleScaffoldTool(server);
registerEventSystemGuideTool(server);
registerMacroHygieneCheckTool(server);
registerConcurrentPlanValidateTool(server);
registerTestFixtureGenTool(server);
registerDbPatternScaffoldTool(server);
registerGracefulShutdownScaffoldTool(server);
registerVerifyTool(server);
registerStdlibSourceTool(server);
registerHowtoRunTool(server);
registerFunctionBehaviorTool(server);
registerTranslateSchemeTool(server);
registerProjectTemplateTool(server);
registerErrorFixLookupTool(server);
registerErrorFixAddTool(server);
registerCheckDuplicatesTool(server);
registerBuildChainTool(server);
registerFFILinkCheckTool(server);
registerBatchSyntaxCheckTool(server);
registerPreflightCheckTool(server);
registerBuildLinkageDiagnosticTool(server);
registerCrossModuleCheckTool(server);
registerDetectIfdefStubsTool(server);
registerQtTestRunnerTool(server);
registerPkgLinkSyncTool(server);
registerCrossPackageDiffTool(server);
registerDispatchCoverageTool(server);
registerMacroPatternDetectorTool(server);
registerBoilerplateConverterTool(server);
registerSignalTraceTool(server);

registerPrompts(server);
registerResources(server);

const transport = new StdioServerTransport();
await server.connect(transport);

process.stderr.write('gerbil-mcp server started\n');
