import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';

// Gambit C name mangling rules:
// - Non-alphanumeric, non-underscore chars → _XX_ where XX is lowercase hex
// - Underscore → __ (double underscore)
// - Alphanumeric chars → pass through unchanged
//
// Known prefixes in compiled output:
//   ___H_    module init (header) function
//   ___G_    global variable
//   ___PRM_  module primitive
//   ___LBL_  label

interface DemangleResult {
  original: string;
  prefix?: string;
  prefixMeaning?: string;
  demangled: string;
}

const KNOWN_PREFIXES: [string, string][] = [
  ['___H_', 'module init'],
  ['___G_', 'global'],
  ['___PRM_', 'module primitive'],
  ['___LBL_', 'label'],
];

/**
 * Demangle a Gambit-mangled C identifier back to a Gerbil symbol name.
 *
 * Encoding rules (reverse):
 *   _XX_  → character with hex code XX (case-insensitive)
 *   __    → literal underscore
 *   other → pass through
 */
export function demangleSymbol(mangled: string): string {
  let result = '';
  let i = 0;
  while (i < mangled.length) {
    if (mangled[i] === '_') {
      // Check for _XX_ hex encoding (2 hex digits followed by _)
      if (
        i + 3 < mangled.length &&
        isHexDigit(mangled[i + 1]) &&
        isHexDigit(mangled[i + 2]) &&
        mangled[i + 3] === '_'
      ) {
        const code = parseInt(mangled.slice(i + 1, i + 3), 16);
        result += String.fromCharCode(code);
        i += 4;
      } else if (i + 1 < mangled.length && mangled[i + 1] === '_') {
        // __ → literal underscore
        result += '_';
        i += 2;
      } else {
        // Lone underscore — pass through (shouldn't happen in well-formed mangled names)
        result += '_';
        i += 1;
      }
    } else {
      result += mangled[i];
      i += 1;
    }
  }
  return result;
}

function isHexDigit(ch: string): boolean {
  return /^[0-9a-fA-F]$/.test(ch);
}

function parseOne(input: string): DemangleResult {
  const trimmed = input.trim();

  // Strip known prefix
  for (const [prefix, meaning] of KNOWN_PREFIXES) {
    if (trimmed.startsWith(prefix)) {
      const body = trimmed.slice(prefix.length);
      return {
        original: trimmed,
        prefix,
        prefixMeaning: meaning,
        demangled: demangleSymbol(body),
      };
    }
  }

  return {
    original: trimmed,
    demangled: demangleSymbol(trimmed),
  };
}

function formatResult(r: DemangleResult): string {
  const lines: string[] = [];
  lines.push(`${r.original}`);
  if (r.prefix) {
    lines.push(`  prefix:    ${r.prefix} (${r.prefixMeaning})`);
  }
  lines.push(`  demangled: ${r.demangled}`);

  // If the demangled name contains '#', show the module/symbol split
  const hashIdx = r.demangled.indexOf('#');
  if (hashIdx !== -1) {
    const mod = r.demangled.slice(0, hashIdx);
    const sym = r.demangled.slice(hashIdx + 1);
    if (mod) lines.push(`  module:    ${mod}`);
    if (sym) lines.push(`  symbol:    ${sym}`);
  }

  return lines.join('\n');
}

export function registerDemangleTool(server: McpServer): void {
  server.registerTool(
    'gerbil_demangle',
    {
      title: 'Demangle Gambit C Symbol',
      description:
        'Decode Gambit-mangled C symbol names back to readable Gerbil module/function paths. ' +
        'Useful when debugging crashes via GDB, reading core dumps, or inspecting compiled C output. ' +
        'Accepts one or more mangled names (newline-separated). ' +
        'Recognizes prefixes like ___H_ (module init), ___G_ (global), ___PRM_ (primitive).',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        symbols: z
          .string()
          .describe(
            'One or more Gambit-mangled C symbol names, newline-separated ' +
              '(e.g. "___H_gerbil_2d_aws_2f_ec2_2f_api" or "___G_hash_2d_ref")',
          ),
      },
    },
    async ({ symbols }) => {
      const inputs = symbols
        .split('\n')
        .map((s) => s.trim())
        .filter((s) => s.length > 0);

      if (inputs.length === 0) {
        return {
          content: [{ type: 'text' as const, text: 'No symbols provided.' }],
          isError: true,
        };
      }

      const results = inputs.map(parseOne);
      const output = results.map(formatResult).join('\n\n');

      return {
        content: [{ type: 'text' as const, text: output }],
      };
    },
  );
}
