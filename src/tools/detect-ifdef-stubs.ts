import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFile } from 'node:fs/promises';
import { relative, basename } from 'node:path';
import { z } from 'zod';
import { scanSchemeFiles } from './parse-utils.js';

interface IfdefStub {
  file: string;
  line: number;
  condition: string;
  stubPattern: string;
  severity: 'critical' | 'medium';
  remediation: string;
}

/** Patterns that indicate a stub returning a sentinel value */
const CRITICAL_STUB_PATTERNS: Array<{ pattern: RegExp; label: string }> = [
  { pattern: /return\s+NULL\s*;/, label: 'return NULL' },
  { pattern: /return\s+nullptr\s*;/, label: 'return nullptr' },
  { pattern: /return\s+0\s*;/, label: 'return 0' },
  { pattern: /return\s+\(void\s*\*\)\s*0\s*;/, label: 'return (void*)0' },
  { pattern: /return\s+\(\s*void\s*\*\s*\)\s*NULL\s*;/, label: 'return (void*)NULL' },
  { pattern: /___return\s*\(\s*NULL\s*\)/, label: '___return(NULL)' },
  { pattern: /___return\s*\(\s*0\s*\)/, label: '___return(0)' },
  { pattern: /___return\s*\(\s*\(\s*void\s*\*\s*\)\s*0\s*\)/, label: '___return((void*)0)' },
];

const MEDIUM_STUB_PATTERNS: Array<{ pattern: RegExp; label: string }> = [
  { pattern: /\{\s*\}/, label: 'empty function body {}' },
  { pattern: /return\s*;/, label: 'return; (void return)' },
];

/**
 * Extract c-declare blocks from Gerbil source code.
 * Handles both heredoc (#<<MARKER ... MARKER) and inline string ("...") forms.
 * Returns array of { cCode, startLine } objects.
 */
function extractCDeclareBlocks(
  content: string,
): Array<{ cCode: string; startLine: number }> {
  const blocks: Array<{ cCode: string; startLine: number }> = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trimStart();

    // Check for (c-declare #<<MARKER
    const heredocMatch = trimmed.match(/\(c-declare\s+#<<(\S+)/);
    if (heredocMatch) {
      const marker = heredocMatch[1];
      const startLine = i + 1;
      const cLines: string[] = [];
      let j = i + 1;
      while (j < lines.length) {
        if (lines[j].trimEnd() === marker) break;
        cLines.push(lines[j]);
        j++;
      }
      blocks.push({ cCode: cLines.join('\n'), startLine: startLine + 1 });
      i = j;
      continue;
    }

    // Check for (c-declare "...")
    const inlineMatch = trimmed.match(/\(c-declare\s+"((?:[^"\\]|\\.)*)"\s*\)/);
    if (inlineMatch) {
      // Unescape the string
      const cCode = inlineMatch[1]
        .replace(/\\n/g, '\n')
        .replace(/\\t/g, '\t')
        .replace(/\\"/g, '"')
        .replace(/\\\\/g, '\\');
      blocks.push({ cCode, startLine: i + 1 });
    }
  }

  return blocks;
}

/**
 * Scan a C code block for #ifdef/#else stubs.
 * Only checks top-level #else branches (not nested).
 */
function findIfdefStubs(
  cCode: string,
  baseLineOffset: number,
  filePath: string,
): IfdefStub[] {
  const stubs: IfdefStub[] = [];
  const lines = cCode.split('\n');

  let depth = 0;
  let currentCondition = '';
  let inElseBranch = false;
  let elseStartLine = 0;
  let elseBranchLines: string[] = [];

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trim();
    const absoluteLine = baseLineOffset + i;

    // Track #ifdef/#ifndef/#if depth
    if (/^#\s*if(?:def|ndef|\s+defined)/.test(trimmed) || /^#\s*if\s/.test(trimmed)) {
      depth++;
      if (depth === 1) {
        // Extract condition
        const condMatch = trimmed.match(
          /^#\s*(?:ifdef|ifndef)\s+(\S+)|^#\s*if\s+defined\s*\(\s*(\S+)\s*\)|^#\s*if\s+(.+)/,
        );
        currentCondition = condMatch
          ? (condMatch[1] || condMatch[2] || condMatch[3] || '').trim()
          : trimmed;
        inElseBranch = false;
        elseBranchLines = [];
      }
      continue;
    }

    if (/^#\s*else/.test(trimmed)) {
      if (depth === 1) {
        inElseBranch = true;
        elseStartLine = absoluteLine;
        elseBranchLines = [];
      }
      continue;
    }

    if (/^#\s*elif/.test(trimmed)) {
      if (depth === 1) {
        // Flush current else branch if any
        if (inElseBranch && elseBranchLines.length > 0) {
          checkBranchForStubs(
            elseBranchLines.join('\n'),
            elseStartLine,
            filePath,
            currentCondition,
            stubs,
          );
        }
        inElseBranch = false;
        elseBranchLines = [];
      }
      continue;
    }

    if (/^#\s*endif/.test(trimmed)) {
      if (depth === 1 && inElseBranch && elseBranchLines.length > 0) {
        checkBranchForStubs(
          elseBranchLines.join('\n'),
          elseStartLine,
          filePath,
          currentCondition,
          stubs,
        );
      }
      if (depth > 0) depth--;
      if (depth === 0) {
        inElseBranch = false;
        currentCondition = '';
        elseBranchLines = [];
      }
      continue;
    }

    if (depth === 1 && inElseBranch) {
      elseBranchLines.push(lines[i]);
    }
  }

  return stubs;
}

function checkBranchForStubs(
  branchCode: string,
  startLine: number,
  filePath: string,
  condition: string,
  stubs: IfdefStub[],
): void {
  for (const { pattern, label } of CRITICAL_STUB_PATTERNS) {
    if (pattern.test(branchCode)) {
      stubs.push({
        file: filePath,
        line: startLine,
        condition,
        stubPattern: label,
        severity: 'critical',
        remediation:
          `Remove the #ifdef guard and the #else stub. Instead, ensure the required ` +
          `library is always linked by adding it to build.ss ld-options. ` +
          `Stubs that return NULL/0 cause segfaults when the caller dereferences the result.`,
      });
      return; // One match per branch is enough
    }
  }

  for (const { pattern, label } of MEDIUM_STUB_PATTERNS) {
    if (pattern.test(branchCode)) {
      stubs.push({
        file: filePath,
        line: startLine,
        condition,
        stubPattern: label,
        severity: 'medium',
        remediation:
          `The #else branch has a no-op stub. This may silently disable functionality. ` +
          `Consider making the dependency mandatory or reporting an error at runtime.`,
      });
      return;
    }
  }
}

export function registerDetectIfdefStubsTool(server: McpServer): void {
  server.registerTool(
    'gerbil_detect_ifdef_stubs',
    {
      title: 'Detect #ifdef Stubs',
      description:
        'Static analysis to find #ifdef/#else blocks in c-declare that return NULL/0 — ' +
        'a common cause of segfaults in cross-project exe builds. Gambit\'s exe builder ' +
        'doesn\'t propagate per-module cc-options, so #else stub branches compile instead ' +
        'of real implementations. Scans for both heredoc and inline c-declare forms.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Path to a single .ss file to scan'),
        project_path: z
          .string()
          .optional()
          .describe('Path to a project directory to scan all .ss files'),
      },
    },
    async ({ file_path, project_path }) => {
      if (!file_path && !project_path) {
        return {
          content: [{
            type: 'text' as const,
            text: 'Either file_path or project_path is required.',
          }],
          isError: true,
        };
      }

      let filesToScan: string[];
      const baseDir = project_path || '';

      if (file_path) {
        filesToScan = [file_path];
      } else {
        filesToScan = await scanSchemeFiles(project_path!);
        filesToScan = filesToScan.filter(f => f.endsWith('.ss'));
      }

      if (filesToScan.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: 'No .ss files found to scan.',
          }],
        };
      }

      const allStubs: IfdefStub[] = [];

      for (const file of filesToScan) {
        let content: string;
        try {
          content = await readFile(file, 'utf-8');
        } catch {
          continue;
        }

        // Quick check: skip files without c-declare
        if (!content.includes('c-declare')) continue;

        const blocks = extractCDeclareBlocks(content);
        for (const block of blocks) {
          const stubs = findIfdefStubs(block.cCode, block.startLine, file);
          allStubs.push(...stubs);
        }
      }

      // Report results
      const sections: string[] = [];
      sections.push(`Scanned ${filesToScan.length} file(s) for #ifdef stubs in c-declare blocks.`);
      sections.push('');

      if (allStubs.length === 0) {
        sections.push('No #ifdef stubs found. Code is clean.');
        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      }

      sections.push(`Found ${allStubs.length} #ifdef stub(s):`);
      sections.push('');

      for (const stub of allStubs) {
        const relPath = baseDir
          ? relative(baseDir, stub.file)
          : basename(stub.file);
        sections.push(`  [${stub.severity.toUpperCase()}] ${relPath}:${stub.line}`);
        sections.push(`    Condition: #ifdef ${stub.condition}`);
        sections.push(`    Stub: ${stub.stubPattern}`);
        sections.push(`    Fix: ${stub.remediation}`);
        sections.push('');
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: allStubs.some(s => s.severity === 'critical'),
      };
    },
  );
}
