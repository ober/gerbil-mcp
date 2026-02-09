import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, readdir, stat } from 'node:fs/promises';
import { join, relative } from 'node:path';

/**
 * Recursively find all .ss files in a directory.
 */
async function findSsFiles(dir: string): Promise<string[]> {
  const files: string[] = [];
  try {
    const entries = await readdir(dir, { withFileTypes: true });
    for (const entry of entries) {
      const fullPath = join(dir, entry.name);
      if (entry.isDirectory() && !entry.name.startsWith('.') && entry.name !== 'node_modules') {
        files.push(...await findSsFiles(fullPath));
      } else if (entry.isFile() && entry.name.endsWith('.ss') && !entry.name.endsWith('-test.ss')) {
        files.push(fullPath);
      }
    }
  } catch {
    // Skip directories we can't read
  }
  return files;
}

/**
 * Extract all top-level definitions from a Gerbil source file.
 */
function extractDefinitions(content: string): string[] {
  const defs: string[] = [];
  const defPattern = /^\s*\((?:def|define|defmethod|defstruct|defclass|defrule|defrules|defsyntax)\s+\(?([a-zA-Z_!?*+/<>=.-][a-zA-Z0-9_!?*+/<>=.-]*)/gm;
  let match: RegExpExecArray | null;
  while ((match = defPattern.exec(content)) !== null) {
    defs.push(match[1]);
  }
  return defs;
}

/**
 * Extract exported symbols from a Gerbil source file.
 */
function extractExports(content: string): Set<string> {
  const exports = new Set<string>();
  const exportPattern = /\(export\s+([^)]+)\)/g;
  let match: RegExpExecArray | null;
  while ((match = exportPattern.exec(content)) !== null) {
    const body = match[1];
    // Parse individual symbols from export form
    const symPattern = /([a-zA-Z_!?*+/<>=.-][a-zA-Z0-9_!?*+/<>=.-]*)/g;
    let symMatch: RegExpExecArray | null;
    while ((symMatch = symPattern.exec(body)) !== null) {
      const sym = symMatch[1];
      // Skip export keywords
      if (sym !== '#t' && sym !== 'except' && sym !== 'only' && sym !== 'rename' && sym !== 'prefix') {
        exports.add(sym);
      }
    }
  }
  return exports;
}

/**
 * Check if a symbol is referenced in a file (excluding its own definition).
 */
function isSymbolUsed(symbol: string, content: string, definitionFile: boolean): boolean {
  // Escape special regex characters in the symbol
  const escaped = symbol.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  // Word boundary detection for Scheme identifiers
  const pattern = new RegExp(`(?<![a-zA-Z0-9_!?*+/<>=.-])${escaped}(?![a-zA-Z0-9_!?*+/<>=.-])`, 'g');

  const matches = content.match(pattern);
  if (!matches) return false;

  if (definitionFile) {
    // In the defining file, the symbol appears at least once (in its definition).
    // It's "used" if it appears more than in just the definition + export.
    return matches.length > 1;
  }

  return matches.length > 0;
}

export function registerDeadCodeTool(server: McpServer): void {
  server.registerTool(
    'gerbil_dead_code',
    {
      title: 'Detect Dead Code',
      description:
        'Detect unexported, uncalled definitions in a Gerbil project. ' +
        'Static analysis that finds definitions that are neither exported nor ' +
        'referenced by any other file in the project. Reports potentially unused code.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        project_path: z.string().describe('Project directory to scan for dead code'),
      },
    },
    async ({ project_path }) => {
      // Verify directory exists
      try {
        const s = await stat(project_path);
        if (!s.isDirectory()) {
          return {
            content: [{ type: 'text' as const, text: `Not a directory: ${project_path}` }],
            isError: true,
          };
        }
      } catch {
        return {
          content: [{ type: 'text' as const, text: `Directory not found: ${project_path}` }],
          isError: true,
        };
      }

      const ssFiles = await findSsFiles(project_path);
      if (ssFiles.length === 0) {
        return {
          content: [{ type: 'text' as const, text: `No .ss files found in ${project_path}` }],
        };
      }

      // Read all files
      const fileContents = new Map<string, string>();
      for (const file of ssFiles) {
        try {
          fileContents.set(file, await readFile(file, 'utf-8'));
        } catch {
          // Skip unreadable files
        }
      }

      // For each file, extract definitions and exports
      const deadSymbols: Array<{
        file: string;
        symbol: string;
      }> = [];

      for (const [file, content] of fileContents) {
        const defs = extractDefinitions(content);
        const exports = extractExports(content);

        for (const def of defs) {
          // If it's exported, it's not dead (from this file's perspective)
          if (exports.has(def)) continue;

          // Check if it's used in any other file
          let usedElsewhere = false;
          for (const [otherFile, otherContent] of fileContents) {
            if (otherFile === file) {
              // In the same file, check for internal usage beyond definition
              if (isSymbolUsed(def, content, true)) {
                usedElsewhere = true;
                break;
              }
            } else {
              if (isSymbolUsed(def, otherContent, false)) {
                usedElsewhere = true;
                break;
              }
            }
          }

          if (!usedElsewhere) {
            deadSymbols.push({ file, symbol: def });
          }
        }
      }

      if (deadSymbols.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: `No dead code detected in ${ssFiles.length} file(s) in ${project_path}.`,
          }],
        };
      }

      // Group by file
      const byFile = new Map<string, string[]>();
      for (const { file, symbol } of deadSymbols) {
        const relPath = relative(project_path, file);
        if (!byFile.has(relPath)) {
          byFile.set(relPath, []);
        }
        byFile.get(relPath)!.push(symbol);
      }

      const sections: string[] = [];
      sections.push(
        `## Dead Code Report: ${project_path}\n` +
        `Found ${deadSymbols.length} potentially unused definition(s) in ${ssFiles.length} file(s):\n`,
      );

      for (const [file, symbols] of byFile) {
        sections.push(`### ${file}`);
        sections.push(symbols.map((s) => `  - ${s}`).join('\n'));
        sections.push('');
      }

      sections.push(
        '\n**Note**: This is static analysis and may produce false positives for:\n' +
        '- Symbols used via dynamic dispatch or eval\n' +
        '- Symbols referenced from test files\n' +
        '- Symbols used in macro-generated code\n' +
        '- Entry points used by build.ss\n',
      );

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
