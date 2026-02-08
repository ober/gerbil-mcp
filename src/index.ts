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
import { registerPrompts } from './prompts.js';

const INSTRUCTIONS = `You have access to a live Gerbil Scheme environment via this MCP server. Use these tools proactively when working with Gerbil Scheme code:

- BEFORE writing ANY Gerbil code: FIRST use gerbil_howto to search the cookbook for relevant patterns. The cookbook contains 240+ verified, working examples with correct imports, arities, and keyword conventions — accumulated from real debugging sessions. Many bugs (wrong arity, missing parent arg, keyword vs positional) are already documented here. Search with the widget/module/task name (e.g. "dialog create", "layout parent", "hash iterate", "json parse"). Skipping this step has repeatedly caused bugs that were already solved in the cookbook.
- BEFORE writing Gerbil code: use gerbil_module_exports to check what a module actually exports, rather than guessing function names or signatures. Use loadpath or project_path to resolve project-local dependency modules.
- BEFORE suggesting Gerbil code: use gerbil_check_syntax to verify your code is syntactically valid.
- When UNSURE about Gerbil behavior: use gerbil_eval to test expressions and verify your assumptions. Use loadpath or project_path to import project-local modules.
- When debugging Gerbil code: use gerbil_eval to reproduce and isolate issues. Use loadpath or project_path for project context.
- To trace let bindings: use gerbil_trace_eval to step through let*/let/letrec/letrec* bindings, showing each variable's name, type, and value as it is bound.
- To inspect SXML trees: use gerbil_sxml_inspect to parse XML text or evaluate an SXML expression and display the tree structure with labeled node types (DOCUMENT, PI, ELEMENT, ATTR, TEXT).
- When exploring unfamiliar Gerbil APIs: use gerbil_apropos to search for relevant symbols, gerbil_module_exports to see what's available, and gerbil_list_std_modules to discover modules.
- When understanding macros: use gerbil_expand_macro to see what sugar forms expand to.
- BEFORE calling Gerbil functions: use gerbil_function_signature to check procedure arities and keyword arguments, avoiding wrong number of arguments errors. Use loadpath or project_path for dependency modules.
- When understanding module structure: use gerbil_module_deps to see what a module imports and depends on.
- When analyzing user code: use gerbil_load_file to extract definitions from Gerbil source files.
- When looking up any symbol: use gerbil_doc to get type, arity, qualified name, and related symbols.
- To catch compilation errors: use gerbil_compile_check to run gxc and detect unbound identifiers and type issues. Use loadpath for project context. Combines stdout/stderr for complete error output. Enhanced error messages help diagnose internal compiler crashes.
- To understand complex macros: use gerbil_trace_macro for step-by-step expansion showing each transformation.
- For multi-step exploration: use gerbil_repl_session to maintain persistent state across evaluations. Use project_path or loadpath on create to work within a project's build context.
- To run test suites: use gerbil_run_tests to execute a single :std/test file (file_path) or run project-wide tests (directory). Use filter to match test names, quiet for errors-only output.
- To examine C bindings: use gerbil_ffi_inspect to classify a module's FFI exports (constants, C functions, wrappers).
- To inspect types: use gerbil_class_info to examine defclass/defstruct types (slots, fields, inheritance, precedence).
- To find where a symbol is defined: use gerbil_find_definition to locate the source file and module for any symbol. Set source_preview: true to include the actual source code.
- To build a Gerbil project: use gerbil_build_project to compile or clean a project directory using gxpkg.
- To explore packages: use gerbil_package_info to list installed packages, search the package directory, or view package metadata.
- To format Gerbil code: use gerbil_format to pretty-print expressions using Gambit's pretty-print.
- To benchmark expressions: use gerbil_benchmark to measure wall-clock time, CPU time, GC stats, and memory allocation.
- To understand error types: use gerbil_error_hierarchy to see the full exception/error class hierarchy tree.
- To check environment: use gerbil_version to see Gerbil/Gambit versions, home directory, and system info.
- To create a new project: use gerbil_scaffold to generate a project template with gerbil.pkg and build.ss.
- To manage packages: use gerbil_package_manage to install, update, or uninstall Gerbil packages.
- To find symbol usages: use gerbil_find_callers to search a directory for files that reference a given symbol.
- To find imports: use gerbil_suggest_imports to discover which module exports a given symbol.
- To get structured diagnostics: use gerbil_diagnostics for file/project compilation diagnostics with file, line, column, severity.
- To list symbols in a file: use gerbil_document_symbols for all definitions with name, kind, and line number.
- To search project symbols: use gerbil_workspace_symbols to find definitions matching a query across all project files.
- To rename a symbol: use gerbil_rename_symbol for project-wide rename with dry-run safety (default).
- To lint code: use gerbil_lint for static analysis (unused imports, duplicates, style, hash literal symbol keys, channel anti-patterns, unquote outside quasiquote, dot in brackets, missing exported definitions with re-export awareness, SRFI-19 time->seconds shadow, unsafe mutex-lock!/unlock! without unwind-protect, byte/char port type mismatch, compilation errors).
- To get project overview: use gerbil_project_info for package name, build targets, source files, and dependencies.
- To map project exports: use gerbil_project_map for a complete view of all modules with their exports, definitions by kind, and import dependencies.
- To check delimiter balance: use gerbil_check_balance for fast paren/bracket/brace balance checking without spawning a subprocess. Handles #! reader directives and suggests cross-checking with gerbil_read_forms on errors.
- To list top-level forms: use gerbil_read_forms to read a file with the actual Gerbil reader and see each form's line range and summary.
- To profile function performance: use gerbil_profile to instrument specific functions with call counting and timing while running an expression. Shows per-function call count, time, and percentage.
- To analyze memory usage: use gerbil_heap_profile to capture GC heap metrics (heap size, allocation, live objects) before and after running an expression.
- To count function calls: use gerbil_trace_calls for lightweight call counting without timing overhead. Useful for finding hot functions.
- To visualize call relationships: use gerbil_call_graph to see which functions call which other functions in a source file (static analysis).
- To scaffold tests: use gerbil_scaffold_test to generate a :std/test skeleton from a module's exports. Saves time writing boilerplate test files.
- To build with diagnostics: use gerbil_build_and_report to run \`gerbil build\` and get structured error diagnostics with file, line, column. Prefer this over running \`gerbil build\` via bash for better error reporting. Auto-detects external dependencies from gerbil.pkg depend: entries and sets GERBIL_LOADPATH automatically. Use \`loadpath\` to override.
- To generate module stubs: use gerbil_generate_module_stub to create a module skeleton matching another module's exported signatures.
- To check export consistency: use gerbil_check_exports to verify that exports match definitions and cross-module imports are consistent across a project.
- To generate modules from templates: use gerbil_generate_module to create new modules by applying substitutions to an existing template file.
- To find code recipes: use gerbil_howto to search curated Gerbil idioms and code examples by keyword (e.g. "json parse", "file read", "channel thread"). Use cookbook_path to merge in additional recipes from a JSON file.
- To save a new Gerbil recipe: use gerbil_howto_add to append idioms discovered during a session to a cookbook JSON file (convention: \`.claude/cookbooks.json\` in the project root). Use gerbil_version to tag version-specific recipes (e.g. "v0.18", "v0.19"). Use valid_for to record confirmed-working versions (e.g. ["v0.18.1-173", "v0.19.0-42"]).
- To get a structural file overview: use gerbil_file_summary for imports, exports, and definitions grouped by kind — without reading the entire file.
- To run Makefile targets: use gerbil_make to run make targets in a Gerbil project directory.
- To check call-site arity: use gerbil_check_arity to detect functions called with the wrong number of arguments across a project or single file. Reports mismatches between call sites and known arities.
- To find tests affected by signature changes: use gerbil_check_test_arity to scan *-test.ss files for calls to a specific function and compare against its current arity. Quickly identifies which tests need updating after a refactor.
- To verify cookbook recipes: use gerbil_howto_verify to check that cookbook recipes have valid syntax and imports. Reports pass/fail for each recipe. Use compile_check: true to also run gxc compilation, which catches unbound identifiers and REPL-only patterns. Shows valid_for (tested versions) per recipe.
- To resolve missing imports: use gerbil_resolve_imports to analyze a file for unbound identifiers, search standard library modules, and generate a suggested import block.
- To suggest a new feature: use gerbil_suggest_feature to write a feature suggestion to the features file for future consideration. Use gerbil_version to tag version-specific features (e.g. "v0.18", "v0.19").
- To check existing feature suggestions: use gerbil_list_features to search or list existing feature suggestions and check for duplicates before suggesting new ones. Use gerbil_version to filter by Gerbil version.
- To vote for a feature: use gerbil_vote_feature to increment the vote count for a feature you could have used. Votes help prioritize which features to implement next.
- To decode mangled C symbols: use gerbil_demangle to convert Gambit-mangled C identifiers from GDB/core dumps back to readable Gerbil module/function paths.
- To detect stale build artifacts: use gerbil_stale_static to compare global vs project-local static files and find stale copies that could shadow the current build.
- For balance-safe editing: use gerbil_balanced_replace instead of string replace. It validates delimiter balance before and after the edit, rejecting changes that break balance. Dry-run by default.
- To wrap code in a form: use gerbil_wrap_form to wrap lines in a new Scheme form (e.g. when, let, begin) with guaranteed matching parentheses. Auto-detects form boundaries. Dry-run by default.
- To unwrap/splice a form: use gerbil_splice_form to remove a wrapper form while keeping selected children. The inverse of wrap. Dry-run by default.
- To generate FFI bindings: use gerbil_ffi_scaffold to parse a C header file and generate Gambit FFI binding code with c-define-type, c-lambda, and define-const declarations. Recognizes create/destroy pairs for automatic GC cleanup.
- To visualize project dependencies: use gerbil_project_dep_graph to see which project modules import from which other modules as an ASCII dependency tree. Lists external dependencies separately.
- To check test coverage: use gerbil_test_coverage to compare a module's exports against its test file and identify untested symbols. Auto-discovers *-test.ss files.
- To get a module catalog: use gerbil_module_catalog for a compact reference of all exports from a module with kind, arity, and brief descriptions. Has curated descriptions for :std/sugar, :std/iter. Replaces multiple gerbil_doc calls.
- To debug FFI callbacks: use gerbil_ffi_callback_debug to analyze c-define/extern linkage in a .ss file. Detects orphan callbacks, missing externs, duplicate C names, and callbacks outside begin-foreign.
- To check example API coverage: use gerbil_example_api_coverage to see which module exports are referenced in example/doc files. Scans .ss files in a directory or explicit file list.
- To validate example imports: use gerbil_validate_example_imports to check that imported modules actually export the symbols used in a file. Detects potentially undefined symbols.
- To bisect crashes: use gerbil_bisect_crash to binary-search a crashing Gerbil file and find the minimal set of top-level forms that reproduce the crash. Keeps preamble (imports, exports, declarations) and bisects body forms. Useful for isolating segfaults and uncaught exceptions in FFI code.
- To detect import conflicts: use gerbil_check_import_conflicts to find local definitions that clash with imported module exports before building. Catches "Bad binding; import conflict" errors early with clear messages showing which symbol conflicts with which module. Also detects cross-import conflicts where multiple imports provide the same symbol.

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

registerPrompts(server);

const transport = new StdioServerTransport();
await server.connect(transport);

process.stderr.write('gerbil-mcp server started\n');
