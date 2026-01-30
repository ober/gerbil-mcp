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

const INSTRUCTIONS = `You have access to a live Gerbil Scheme environment via this MCP server. Use these tools proactively when working with Gerbil Scheme code:

- BEFORE writing Gerbil code: use gerbil_module_exports to check what a module actually exports, rather than guessing function names or signatures.
- BEFORE suggesting Gerbil code: use gerbil_check_syntax to verify your code is syntactically valid.
- When UNSURE about Gerbil behavior: use gerbil_eval to test expressions and verify your assumptions.
- When debugging Gerbil code: use gerbil_eval to reproduce and isolate issues.
- When exploring unfamiliar Gerbil APIs: use gerbil_apropos to search for relevant symbols, gerbil_module_exports to see what's available, and gerbil_list_std_modules to discover modules.
- When understanding macros: use gerbil_expand_macro to see what sugar forms expand to.
- BEFORE calling Gerbil functions: use gerbil_function_signature to check procedure arities, avoiding wrong number of arguments errors.
- When understanding module structure: use gerbil_module_deps to see what a module imports and depends on.
- When analyzing user code: use gerbil_load_file to extract definitions from Gerbil source files.
- When looking up any symbol: use gerbil_doc to get type, arity, qualified name, and related symbols.
- To catch compilation errors: use gerbil_compile_check to run gxc and detect unbound identifiers and type issues beyond syntax checking.
- To understand complex macros: use gerbil_trace_macro for step-by-step expansion showing each transformation.
- For multi-step exploration: use gerbil_repl_session to maintain persistent state across evaluations (define functions, import modules, test incrementally).
- To run test suites: use gerbil_run_tests to execute :std/test files and see pass/fail results with failure details.
- To examine C bindings: use gerbil_ffi_inspect to classify a module's FFI exports (constants, C functions, wrappers).
- To inspect types: use gerbil_class_info to examine defclass/defstruct types (slots, fields, inheritance, precedence).
- To find where a symbol is defined: use gerbil_find_definition to locate the source file and module for any symbol.
- To build a Gerbil project: use gerbil_build_project to compile or clean a project directory using gxpkg.
- To explore packages: use gerbil_package_info to list installed packages, search the package directory, or view package metadata.
- To format Gerbil code: use gerbil_format to pretty-print expressions using Gambit's pretty-print.
- To benchmark expressions: use gerbil_benchmark to measure wall-clock time, CPU time, GC stats, and memory allocation.
- To understand error types: use gerbil_error_hierarchy to see the full exception/error class hierarchy tree.

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

const transport = new StdioServerTransport();
await server.connect(transport);

process.stderr.write('gerbil-mcp server started\n');
