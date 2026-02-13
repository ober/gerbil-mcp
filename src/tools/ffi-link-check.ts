import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { execFile } from 'node:child_process';
import { join, resolve, dirname, basename } from 'node:path';
import { z } from 'zod';

/**
 * Extract C function calls from c-declare blocks in a Gerbil .ss file.
 * Returns function names that are called in shim code (not declared/defined).
 */
function extractCDeclareCallees(source: string): string[] {
  const callees = new Set<string>();
  // Match c-declare blocks: (c-declare #<<END-C ... END-C) or (c-declare "...")
  const cDeclarePattern = /\(c-declare\s+(?:#<<(\S+)\s+([\s\S]*?)^\1|"((?:[^"\\]|\\.)*)"\s*)\)/gm;
  let match;
  while ((match = cDeclarePattern.exec(source)) !== null) {
    const cCode = match[2] || match[3] || '';
    extractCallsFromCCode(cCode, callees);
  }

  // Also check for inline c-declare strings
  const inlinePattern = /\(c-declare\s+"((?:[^"\\]|\\.)*)"\s*\)/g;
  while ((match = inlinePattern.exec(source)) !== null) {
    extractCallsFromCCode(match[1], callees);
  }

  return [...callees];
}

/**
 * Extract function calls from C code (function names followed by '(').
 * Filters out common C keywords and standard library functions.
 */
function extractCallsFromCCode(cCode: string, callees: Set<string>): void {
  // Remove C comments
  const noComments = cCode.replace(/\/\*[\s\S]*?\*\//g, '').replace(/\/\/.*$/gm, '');
  // Remove string literals
  const noStrings = noComments.replace(/"(?:[^"\\]|\\.)*"/g, '""');
  // Remove preprocessor directives
  const noPreprocessor = noStrings.replace(/^\s*#.*$/gm, '');

  // Match function calls: identifier(
  const callPattern = /\b([a-zA-Z_][a-zA-Z0-9_]*)\s*\(/g;
  let match;
  while ((match = callPattern.exec(noPreprocessor)) !== null) {
    const name = match[1];
    if (!C_KEYWORDS.has(name) && !C_STDLIB.has(name)) {
      callees.add(name);
    }
  }

  // Also match ___return(FunctionName(...)) patterns
  const returnPattern = /___return\s*\(\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*\(/g;
  while ((match = returnPattern.exec(noPreprocessor)) !== null) {
    callees.add(match[1]);
  }
}

const C_KEYWORDS = new Set([
  'if', 'else', 'while', 'for', 'do', 'switch', 'case', 'break', 'continue',
  'return', 'sizeof', 'typeof', 'typedef', 'struct', 'union', 'enum', 'void',
  'int', 'char', 'float', 'double', 'long', 'short', 'unsigned', 'signed',
  'static', 'extern', 'const', 'volatile', 'register', 'auto', 'inline',
  '___return', '___BEGIN_CFUN', '___END_CFUN',
]);

const C_STDLIB = new Set([
  'malloc', 'free', 'calloc', 'realloc', 'memcpy', 'memset', 'memmove',
  'strcmp', 'strncmp', 'strlen', 'strcpy', 'strncpy', 'strcat', 'strncat',
  'printf', 'fprintf', 'sprintf', 'snprintf', 'sscanf', 'fscanf',
  'fopen', 'fclose', 'fread', 'fwrite', 'fgets', 'fputs',
  'atoi', 'atol', 'atof', 'strtol', 'strtoul', 'strtod',
  'abs', 'labs', 'exit', 'abort', 'assert',
]);

/**
 * Extract .a library paths from build.ss ld-options.
 */
async function extractLdLibraries(buildSsPath: string): Promise<string[]> {
  try {
    const content = await readFile(buildSsPath, 'utf-8');
    const libs: string[] = [];
    // Match paths ending in .a
    const libPattern = /["']([^"']*\.a)["']/g;
    let match;
    while ((match = libPattern.exec(content)) !== null) {
      libs.push(match[1]);
    }
    return libs;
  } catch {
    return [];
  }
}

/**
 * Run nm on a .a file and extract defined symbols (T/t/D/d/B/b types).
 */
function runNm(libPath: string): Promise<Set<string>> {
  return new Promise((resolve) => {
    execFile('nm', ['--defined-only', '-g', libPath], { timeout: 10_000 }, (error, stdout) => {
      const symbols = new Set<string>();
      if (!error && stdout) {
        for (const line of stdout.split('\n')) {
          // nm output: address type name
          const parts = line.trim().split(/\s+/);
          if (parts.length >= 3) {
            symbols.add(parts[2]);
          } else if (parts.length === 2) {
            // Some formats: type name
            symbols.add(parts[1]);
          }
        }
      }
      resolve(symbols);
    });
  });
}

export function registerFFILinkCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_ffi_link_check',
    {
      title: 'FFI Link Symbol Check',
      description:
        'Cross-reference C function calls in c-declare blocks against symbols exported ' +
        'by linked static libraries (.a files). Uses `nm` to extract symbols from archives ' +
        'and reports undefined symbols that are called but not found in any linked library. ' +
        'Catches missing library links before a full build-test cycle.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        ffi_file: z
          .string()
          .describe('Path to the Gerbil .ss file containing c-declare blocks'),
        build_file: z
          .string()
          .optional()
          .describe('Path to build.ss to extract ld-options library paths (default: sibling build.ss)'),
        libraries: z
          .array(z.string())
          .optional()
          .describe('Explicit list of .a library file paths to check against'),
      },
    },
    async ({ ffi_file, build_file, libraries }) => {
      // Read the FFI source file
      let ffiSource: string;
      try {
        ffiSource = await readFile(ffi_file, 'utf-8');
      } catch (err) {
        return {
          content: [{
            type: 'text' as const,
            text: `Error reading ${ffi_file}: ${err instanceof Error ? err.message : String(err)}`,
          }],
          isError: true,
        };
      }

      // Extract C function calls from c-declare blocks
      const callees = extractCDeclareCallees(ffiSource);
      if (callees.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: 'No C function calls found in c-declare blocks.',
          }],
        };
      }

      // Gather library paths
      const libPaths: string[] = [...(libraries || [])];
      const buildSs = build_file || join(dirname(ffi_file), 'build.ss');
      if (existsSync(buildSs)) {
        const buildLibs = await extractLdLibraries(buildSs);
        const projectDir = dirname(buildSs);
        for (const lib of buildLibs) {
          const resolved = lib.startsWith('/') ? lib : resolve(projectDir, lib);
          if (!libPaths.includes(resolved)) {
            libPaths.push(resolved);
          }
        }
      }

      if (libPaths.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: `Found ${callees.length} C function call(s) but no .a libraries to check against.\n` +
              `Provide library paths via the libraries parameter or ensure build.ss has ld-options.`,
          }],
          isError: true,
        };
      }

      // Run nm on each library
      const libSymbols = new Map<string, Set<string>>();
      const missingLibs: string[] = [];
      for (const lib of libPaths) {
        if (!existsSync(lib)) {
          missingLibs.push(lib);
          continue;
        }
        const symbols = await runNm(lib);
        libSymbols.set(lib, symbols);
      }

      // Cross-reference callees against library symbols
      const allSymbols = new Set<string>();
      for (const symbols of libSymbols.values()) {
        for (const s of symbols) allSymbols.add(s);
      }

      const resolved: Array<{ name: string; foundIn: string }> = [];
      const unresolved: string[] = [];

      for (const callee of callees) {
        let found = false;
        for (const [lib, symbols] of libSymbols) {
          if (symbols.has(callee) || symbols.has(`_${callee}`)) {
            resolved.push({ name: callee, foundIn: basename(lib) });
            found = true;
            break;
          }
        }
        if (!found) {
          unresolved.push(callee);
        }
      }

      // Report results
      const sections: string[] = [];
      sections.push(`Checked ${callees.length} C function call(s) against ${libPaths.length} library/libraries.`);
      sections.push('');

      if (missingLibs.length > 0) {
        sections.push('Missing libraries (not found on disk):');
        for (const lib of missingLibs) {
          sections.push(`  ${lib}`);
        }
        sections.push('');
      }

      if (unresolved.length > 0) {
        sections.push(`Undefined symbols (${unresolved.length}):`)
        for (const name of unresolved) {
          sections.push(`  ${name}: not found in any linked library`);
        }
        sections.push('');
      }

      if (resolved.length > 0) {
        sections.push(`Resolved symbols (${resolved.length}):`)
        for (const { name, foundIn } of resolved) {
          sections.push(`  ${name}: found in ${foundIn}`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: unresolved.length > 0,
      };
    },
  );
}
