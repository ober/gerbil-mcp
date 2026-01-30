#!/usr/bin/env node

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { registerEvalTool } from './tools/eval.js';
import { registerModuleExportsTool } from './tools/module-exports.js';
import { registerCheckSyntaxTool } from './tools/check-syntax.js';
import { registerExpandMacroTool } from './tools/expand-macro.js';
import { registerAproposTool } from './tools/apropos.js';
import { registerListModulesTool } from './tools/list-modules.js';

const INSTRUCTIONS = `You have access to a live Gerbil Scheme environment via this MCP server. Use these tools proactively when working with Gerbil Scheme code:

- BEFORE writing Gerbil code: use gerbil_module_exports to check what a module actually exports, rather than guessing function names or signatures.
- BEFORE suggesting Gerbil code: use gerbil_check_syntax to verify your code is syntactically valid.
- When UNSURE about Gerbil behavior: use gerbil_eval to test expressions and verify your assumptions.
- When debugging Gerbil code: use gerbil_eval to reproduce and isolate issues.
- When exploring unfamiliar Gerbil APIs: use gerbil_apropos to search for relevant symbols, gerbil_module_exports to see what's available, and gerbil_list_std_modules to discover modules.
- When understanding macros: use gerbil_expand_macro to see what sugar forms expand to.

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

const transport = new StdioServerTransport();
await server.connect(transport);

process.stderr.write('gerbil-mcp server started\n');
