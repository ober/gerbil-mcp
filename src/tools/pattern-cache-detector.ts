import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile, readdir, stat } from 'node:fs/promises';
import { join } from 'node:path';

interface AntiPattern {
  file: string;
  line: number;
  pattern: string;
  code: string;
  severity: 'high' | 'medium' | 'low';
  suggestion: string;
}

/**
 * Detect regex/pattern compilation anti-patterns in Gerbil source code.
 *
 * Patterns detected:
 * 1. pregexp/pregexp-match inside def that could be hoisted
 * 2. pcre2-compile inside def that could be hoisted
 * 3. Pattern compilation inside loops (for, for-each, map, let loop)
 * 4. Repeated identical pattern strings compiled multiple times
 * 5. string-append used to build regex patterns (dynamic, can't cache)
 */
function detectAntiPatterns(
  content: string,
  filePath: string,
): AntiPattern[] {
  const patterns: AntiPattern[] = [];
  const lines = content.split('\n');

  // Track pattern strings seen for duplicate detection
  const patternStrings = new Map<string, number[]>();

  // Track if we're inside a loop or function body
  let loopDepth = 0;
  let defDepth = 0;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const trimmed = line.trim();
    const lineNum = i + 1;

    // Skip comments
    if (trimmed.startsWith(';') || trimmed.startsWith('#|')) continue;

    // Track loop context (simplified — look for loop-starting forms)
    if (
      /\((?:for|for-each|for\/|map|let\s+loop|do\s)\b/.test(trimmed)
    ) {
      loopDepth++;
    }

    // Track def context — only function definitions (def (name args...) body)
    // Skip simple value bindings like (def name value) which are module-level constants
    if (/^\(def[a-z]*\s+\(/.test(trimmed)) {
      defDepth++;
    }

    // 1. pregexp compilation in function body
    const pregexpMatch = trimmed.match(
      /\(pregexp\s+"([^"]+)"/,
    );
    if (pregexpMatch) {
      const patStr = pregexpMatch[1];
      // Track for duplicate detection
      if (!patternStrings.has(patStr)) {
        patternStrings.set(patStr, []);
      }
      patternStrings.get(patStr)!.push(lineNum);

      if (loopDepth > 0) {
        patterns.push({
          file: filePath,
          line: lineNum,
          pattern: 'pregexp-in-loop',
          code: trimmed,
          severity: 'high',
          suggestion:
            'Move (pregexp ...) out of the loop. Compile the pattern once at module level or use pregexp-match with a string pattern to leverage automatic caching.',
        });
      } else if (defDepth > 0) {
        patterns.push({
          file: filePath,
          line: lineNum,
          pattern: 'pregexp-in-def',
          code: trimmed,
          severity: 'medium',
          suggestion:
            'Consider hoisting (pregexp ...) to module level so it is compiled once. Alternatively, use pregexp-match with a string argument to leverage LRU caching.',
        });
      }
    }

    // 2. pcre2-compile in function body or loop
    const pcreMatch = trimmed.match(
      /\(pcre2-compile\s+"([^"]+)"/,
    );
    if (pcreMatch) {
      const patStr = pcreMatch[1];
      if (!patternStrings.has(patStr)) {
        patternStrings.set(patStr, []);
      }
      patternStrings.get(patStr)!.push(lineNum);

      if (loopDepth > 0) {
        patterns.push({
          file: filePath,
          line: lineNum,
          pattern: 'pcre2-compile-in-loop',
          code: trimmed,
          severity: 'high',
          suggestion:
            'Move (pcre2-compile ...) out of the loop. Compile the pattern once at module level and reuse the compiled regex object.',
        });
      } else if (defDepth > 0) {
        patterns.push({
          file: filePath,
          line: lineNum,
          pattern: 'pcre2-compile-in-def',
          code: trimmed,
          severity: 'medium',
          suggestion:
            'Consider hoisting (pcre2-compile ...) to module level so it is compiled once per program lifetime instead of once per call.',
        });
      }
    }

    // 3. Dynamic pattern building with string-append for regex
    if (
      /\((?:pregexp|pcre2-compile)\s+\(string-append\b/.test(trimmed) ||
      /\((?:pregexp-match|pcre2-match)\s+\(string-append\b/.test(trimmed)
    ) {
      patterns.push({
        file: filePath,
        line: lineNum,
        pattern: 'dynamic-pattern-build',
        code: trimmed,
        severity: 'medium',
        suggestion:
          'Dynamic pattern construction via string-append defeats caching. If the pattern components are constant, pre-build the full pattern string at module level.',
      });
    }

    // 4. pregexp-match with compiled pattern (could use string instead for caching)
    if (/\(pregexp-match\s+\(pregexp\s/.test(trimmed)) {
      patterns.push({
        file: filePath,
        line: lineNum,
        pattern: 'redundant-pregexp-compile',
        code: trimmed,
        severity: 'medium',
        suggestion:
          'pregexp-match already accepts string patterns and caches compiled regexes internally. Remove the explicit (pregexp ...) wrapper to benefit from automatic LRU caching.',
      });
    }

    // Rough tracking of closing parens for loop/def context
    // This is a simplification — proper s-expression parsing would be better
    // but this catches the most common patterns
    const opens = (line.match(/\(/g) || []).length;
    const closes = (line.match(/\)/g) || []).length;
    const netClose = closes - opens;
    if (netClose > 0) {
      for (let j = 0; j < netClose; j++) {
        if (loopDepth > 0) loopDepth--;
        else if (defDepth > 0) defDepth--;
      }
    }
  }

  // 5. Detect duplicate pattern strings compiled in multiple places
  for (const [patStr, locations] of patternStrings) {
    if (locations.length >= 2) {
      patterns.push({
        file: filePath,
        line: locations[0],
        pattern: 'duplicate-pattern-compile',
        code: `"${patStr}" compiled at lines: ${locations.join(', ')}`,
        severity: 'medium',
        suggestion:
          `Pattern "${patStr}" is compiled ${locations.length} times. Define it once at module level: (def my-pattern (pregexp "${patStr}")) and reuse.`,
      });
    }
  }

  return patterns;
}

/**
 * Scan all .ss files in a directory recursively.
 */
async function scanDirectory(dirPath: string): Promise<AntiPattern[]> {
  const allPatterns: AntiPattern[] = [];
  const entries = await readdir(dirPath, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = join(dirPath, entry.name);
    if (entry.isDirectory() && !entry.name.startsWith('.')) {
      const sub = await scanDirectory(fullPath);
      allPatterns.push(...sub);
    } else if (entry.isFile() && entry.name.endsWith('.ss')) {
      const content = await readFile(fullPath, 'utf-8');
      const found = detectAntiPatterns(content, fullPath);
      allPatterns.push(...found);
    }
  }

  return allPatterns;
}

export function registerPatternCacheDetectorTool(
  server: McpServer,
): void {
  server.registerTool(
    'gerbil_pattern_cache_check',
    {
      title: 'Detect Pattern Caching Anti-Patterns',
      description:
        'Scan Gerbil code for regex/pattern compilation anti-patterns that hurt performance. ' +
        'Detects: (1) pregexp/pcre2-compile inside function bodies (should hoist to module level), ' +
        '(2) pattern compilation inside loops (for, for-each, map, let loop), ' +
        '(3) dynamic pattern building via string-append (defeats caching), ' +
        '(4) redundant explicit pregexp wrapping in pregexp-match calls, ' +
        '(5) duplicate identical pattern strings compiled in multiple places. ' +
        'Reports each finding with severity, line number, and suggested fix.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Single .ss file to scan'),
        project_path: z
          .string()
          .optional()
          .describe(
            'Project directory to scan recursively (all .ss files)',
          ),
      },
    },
    async ({ file_path, project_path }) => {
      if (!file_path && !project_path) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Either file_path or project_path is required.',
            },
          ],
          isError: true,
        };
      }

      let allPatterns: AntiPattern[] = [];

      if (file_path) {
        try {
          const content = await readFile(file_path, 'utf-8');
          allPatterns = detectAntiPatterns(content, file_path);
        } catch {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Cannot read file: ${file_path}`,
              },
            ],
            isError: true,
          };
        }
      } else if (project_path) {
        try {
          await stat(project_path);
        } catch {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Directory not found: ${project_path}`,
              },
            ],
            isError: true,
          };
        }
        allPatterns = await scanDirectory(project_path);
      }

      if (allPatterns.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'No pattern caching anti-patterns detected.',
            },
          ],
        };
      }

      // Group by severity
      const high = allPatterns.filter((p) => p.severity === 'high');
      const medium = allPatterns.filter(
        (p) => p.severity === 'medium',
      );
      const low = allPatterns.filter((p) => p.severity === 'low');

      const sections: string[] = [];
      sections.push(
        `Pattern Caching Anti-Patterns: ${allPatterns.length} found`,
      );
      sections.push('');

      const formatGroup = (
        label: string,
        items: AntiPattern[],
      ) => {
        if (items.length === 0) return;
        sections.push(`${label} (${items.length}):`);
        for (const item of items) {
          sections.push(
            `  ${item.file}:${item.line} [${item.pattern}]`,
          );
          sections.push(`    ${item.code}`);
          sections.push(`    → ${item.suggestion}`);
          sections.push('');
        }
      };

      formatGroup('HIGH', high);
      formatGroup('MEDIUM', medium);
      formatGroup('LOW', low);

      return {
        content: [
          { type: 'text' as const, text: sections.join('\n') },
        ],
        isError: high.length > 0,
      };
    },
  );
}
