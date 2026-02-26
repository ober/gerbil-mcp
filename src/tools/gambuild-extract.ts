import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, stat } from 'node:fs/promises';
import { join } from 'node:path';
import { homedir } from 'node:os';
import { runGxi } from '../gxi.js';

/**
 * Known gambuild-C environment variables and their purposes.
 */
const ENV_VARS: Record<string, string> = {
  'BUILD_OBJ_CC': 'C compiler command for object file compilation',
  'BUILD_OBJ_CC_PARAM': 'Compiler flags for object file compilation',
  'BUILD_DYN_CC': 'C compiler command for dynamic library compilation',
  'BUILD_DYN_CC_PARAM': 'Compiler flags for dynamic library (includes -shared -fPIC)',
  'BUILD_EXE_CC': 'C compiler command for executable linking',
  'BUILD_EXE_CC_PARAM': 'Compiler/linker flags for executable linking',
  'BUILD_LIB_CC': 'C compiler command for static library creation',
  'BUILD_LIB_CC_PARAM': 'Flags for static library (ar command)',
  'GAMBITDIR_INCLUDE': 'Path to Gambit header files (gambit.h)',
  'GAMBITDIR_LIB': 'Path to Gambit library files (libgambit.a)',
  'C_COMPILER': 'The C compiler to use (gcc, clang, etc.)',
  'C_PREPROC': 'C preprocessor command',
  'FLAGS_OBJ': 'Additional flags for object compilation',
  'FLAGS_DYN': 'Additional flags for dynamic library',
  'FLAGS_LIB': 'Additional flags for static library',
  'FLAGS_EXE': 'Additional flags for executable',
  'LIBS': 'Libraries to link against',
  'GAMBITDIR': 'Root Gambit installation directory',
};

export function registerGambuildExtractTool(server: McpServer): void {
  server.registerTool(
    'gerbil_gambuild_extract',
    {
      title: 'Extract Gambuild-C Templates',
      description:
        'Read and explain the installed gambuild-C script, extracting operation templates ' +
        '(dyn, obj, exe) and environment variables. The gambuild-C script is the critical ' +
        'bridge between Gambit\'s compile-file and the system C compiler. Shows the exact ' +
        'gcc invocation for each operation, lists all environment variables with their purposes, ' +
        'and shows actual values from the current Gerbil/Gambit installation.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        operation: z
          .string()
          .optional()
          .describe(
            'Specific operation to show (dyn, obj, exe, lib). If omitted, shows all.',
          ),
      },
    },
    async ({ operation }) => {
      // Find gambuild-C script
      let gambuildPath = '';
      const candidates = [
        process.env.GERBIL_HOME
          ? join(process.env.GERBIL_HOME, 'bin', 'gambuild-C')
          : '',
        '/opt/gerbil/bin/gambuild-C',
        join(homedir(), '.gerbil', 'bin', 'gambuild-C'),
      ].filter(Boolean);

      // Also try to get it from gxi
      const gxiResult = await runGxi(
        ['(displayln (path-expand "~~bin/gambuild-C"))'],
        { timeout: 5_000 },
      );
      if (!gxiResult.timedOut && gxiResult.stdout.trim()) {
        candidates.unshift(gxiResult.stdout.trim());
      }

      for (const path of candidates) {
        try {
          const st = await stat(path);
          if (st.isFile()) {
            gambuildPath = path;
            break;
          }
        } catch {
          continue;
        }
      }

      if (!gambuildPath) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Could not find gambuild-C script. Searched:\n' +
                candidates.map((c) => `  ${c}`).join('\n'),
            },
          ],
          isError: true,
        };
      }

      let content: string;
      try {
        content = await readFile(gambuildPath, 'utf-8');
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            { type: 'text' as const, text: `Failed to read ${gambuildPath}: ${msg}` },
          ],
          isError: true,
        };
      }

      const sections: string[] = [
        `Gambuild-C Template Analysis: ${gambuildPath}`,
        `Script size: ${content.length} bytes, ${content.split('\n').length} lines`,
        '',
      ];

      // Extract operations (case blocks)
      const operations = extractOperations(content);

      if (operation) {
        const op = operations.get(operation);
        if (!op) {
          sections.push(`Operation "${operation}" not found.`);
          sections.push(`Available operations: ${[...operations.keys()].join(', ')}`);
        } else {
          sections.push(`Operation: ${operation}`);
          sections.push('');
          sections.push(op);
        }
      } else {
        sections.push(`Operations found: ${[...operations.keys()].join(', ')}`);
        sections.push('');

        for (const [name, body] of operations) {
          sections.push(`━━━ Operation: ${name} ━━━`);
          sections.push('');
          sections.push(body);
          sections.push('');
        }
      }

      // List environment variables found in the script
      sections.push('━━━ Environment Variables ━━━');
      sections.push('');

      const foundVars = new Set<string>();
      for (const varName of Object.keys(ENV_VARS)) {
        if (content.includes(varName) || content.includes(`$${varName}`) || content.includes(`\${${varName}}`)) {
          foundVars.add(varName);
        }
      }

      // Also find vars not in our known list
      const envPattern = /\$\{?([A-Z_][A-Z0-9_]*)\}?/g;
      let match;
      while ((match = envPattern.exec(content)) !== null) {
        const varName = match[1];
        if (!['HOME', 'PATH', 'PWD', 'TMPDIR', 'USER', 'SHELL'].includes(varName)) {
          foundVars.add(varName);
        }
      }

      for (const varName of [...foundVars].sort()) {
        const desc = ENV_VARS[varName] || '(undocumented)';
        sections.push(`  ${varName}: ${desc}`);
      }

      // Get actual values from the current installation
      sections.push('');
      sections.push('━━━ Current Installation Values ━━━');
      sections.push('');

      const valResult = await runGxi(
        [
          '(display "GAMBITDIR:") (displayln (path-expand "~~"))',
          '(display "INCLUDE:") (displayln (path-expand "~~include"))',
          '(display "LIB:") (displayln (path-expand "~~lib"))',
        ],
        { timeout: 5_000 },
      );

      if (!valResult.timedOut) {
        for (const line of valResult.stdout.split('\n')) {
          const trimmed = line.trim();
          if (trimmed.startsWith('GAMBITDIR:')) {
            sections.push(`  GAMBITDIR = ${trimmed.slice('GAMBITDIR:'.length)}`);
          } else if (trimmed.startsWith('INCLUDE:')) {
            sections.push(`  GAMBITDIR_INCLUDE = ${trimmed.slice('INCLUDE:'.length)}`);
          } else if (trimmed.startsWith('LIB:')) {
            sections.push(`  GAMBITDIR_LIB = ${trimmed.slice('LIB:'.length)}`);
          }
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}

/**
 * Extract operation blocks from the gambuild-C script.
 * Looks for case patterns like: "dyn") ... ;; or function-based dispatching.
 */
function extractOperations(content: string): Map<string, string> {
  const operations = new Map<string, string>();
  const lines = content.split('\n');

  let currentOp: string | null = null;
  let currentBody: string[] = [];

  for (const line of lines) {
    // Match case-style patterns: "dyn") or dyn)
    const caseMatch = line.match(/^\s*"?(\w+)"?\s*\)/);
    if (caseMatch) {
      // Save previous op
      if (currentOp) {
        operations.set(currentOp, currentBody.join('\n'));
      }
      currentOp = caseMatch[1];
      currentBody = [line];
      continue;
    }

    // Match function-style: gambuild_dyn() or build_dyn()
    const funcMatch = line.match(/^\s*(?:gambuild|build)_(\w+)\s*\(\)/);
    if (funcMatch) {
      if (currentOp) {
        operations.set(currentOp, currentBody.join('\n'));
      }
      currentOp = funcMatch[1];
      currentBody = [line];
      continue;
    }

    // End of case block
    if (currentOp && line.match(/^\s*;;/)) {
      currentBody.push(line);
      operations.set(currentOp, currentBody.join('\n'));
      currentOp = null;
      currentBody = [];
      continue;
    }

    if (currentOp) {
      currentBody.push(line);
    }
  }

  if (currentOp) {
    operations.set(currentOp, currentBody.join('\n'));
  }

  return operations;
}
