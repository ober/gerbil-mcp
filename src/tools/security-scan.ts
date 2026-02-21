/**
 * gerbil_security_scan — Static security scanner for Gerbil code.
 * Scans .ss and .c/.h files for known vulnerability patterns.
 * Pure TypeScript, no subprocess.
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, resolve, join } from 'node:path';
import { readdir, stat } from 'node:fs/promises';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
export const REPO_SECURITY_RULES_PATH = resolve(__dirname, '..', '..', 'security-rules.json');

export interface SecurityRule {
  id: string;
  title: string;
  severity: 'critical' | 'high' | 'medium' | 'low';
  scope: 'scheme' | 'c-shim' | 'ffi-boundary';
  pattern: string;
  message: string;
  remediation: string;
  related_recipe?: string;
  tags?: string[];
}

interface SecurityFinding {
  file: string;
  line: number;
  severity: 'critical' | 'high' | 'medium' | 'low';
  ruleId: string;
  message: string;
  remediation: string;
  lineText: string;
  suppressed?: boolean;
}

/**
 * Check if a line has an inline suppression comment for a specific rule.
 * Supports:
 *   ; gerbil-security: suppress <rule-id>     (Scheme)
 *   ; gerbil-security: suppress-all            (Scheme)
 *   // gerbil-security: suppress <rule-id>     (C)
 *   // gerbil-security: suppress-all           (C)
 * Checks both the current line and the preceding line.
 */
function hasSuppressionComment(
  lines: string[],
  lineIdx: number,
  ruleId: string,
): boolean {
  // Check current line and preceding line
  for (let offset = 0; offset >= -1; offset--) {
    const idx = lineIdx + offset;
    if (idx < 0 || idx >= lines.length) continue;
    const line = lines[idx];
    // Match: ; gerbil-security: suppress <rule-id-or-all>
    // Also match: // gerbil-security: suppress <rule-id-or-all>
    const match = line.match(
      /(?:;+|\/\/)\s*gerbil-security:\s*suppress(?:-all|[\s]+(\S+))/,
    );
    if (match) {
      if (line.includes('suppress-all')) return true;
      if (match[1] === ruleId) return true;
    }
  }
  return false;
}

const SEVERITY_ORDER: Record<string, number> = {
  critical: 0,
  high: 1,
  medium: 2,
  low: 3,
};

const SKIP_DIRS = new Set([
  '.git', '.svn', 'node_modules', '.gerbil', '__pycache__', 'dist',
]);

function loadSecurityRules(path: string): SecurityRule[] {
  try {
    const raw = readFileSync(path, 'utf-8');
    const parsed = JSON.parse(raw);
    if (!Array.isArray(parsed)) return [];
    return parsed;
  } catch {
    return [];
  }
}

/**
 * Check if a match position is inside a comment or string.
 * For Scheme files: ; starts a line comment, " toggles strings.
 * For C files: // and /* ... *​/ comments, " toggles strings.
 */
function isInCommentOrString(line: string, matchIndex: number, isC: boolean): boolean {
  if (isC) {
    // C-style: check for // comment before match
    const slashSlash = line.indexOf('//');
    if (slashSlash >= 0 && slashSlash < matchIndex) return true;

    // Heuristic string check: count unescaped " before matchIndex
    let inString = false;
    for (let i = 0; i < matchIndex; i++) {
      if (line[i] === '"' && (i === 0 || line[i - 1] !== '\\')) {
        inString = !inString;
      }
    }
    return inString;
  }

  // Scheme: ; starts a line comment
  const semicolon = line.indexOf(';');
  if (semicolon >= 0 && semicolon < matchIndex) return true;

  // Heuristic string check
  const before = line.substring(0, matchIndex);
  const quoteCount = (before.match(/"/g) || []).length;
  return quoteCount % 2 !== 0;
}

/**
 * Check if a port-open pattern has unwind-protect on nearby lines.
 */
function hasUnwindProtect(lines: string[], lineIdx: number): boolean {
  // Search surrounding context (5 lines up, 10 lines down)
  const start = Math.max(0, lineIdx - 5);
  const end = Math.min(lines.length, lineIdx + 10);
  for (let i = start; i < end; i++) {
    const t = lines[i];
    if (t.includes('unwind-protect') || t.includes('call-with-input-file') ||
        t.includes('call-with-output-file') || t.includes('call-with-port') ||
        t.includes('with-input-from-file') || t.includes('with-output-to-file')) {
      return true;
    }
  }
  return false;
}

function scanFileContent(
  filePath: string,
  content: string,
  rules: SecurityRule[],
  severityThreshold: number,
): SecurityFinding[] {
  const findings: SecurityFinding[] = [];
  const lines = content.split('\n');
  const isC = filePath.endsWith('.c') || filePath.endsWith('.h');
  const isSS = filePath.endsWith('.ss') || filePath.endsWith('.scm');

  for (const rule of rules) {
    // Skip rules that don't apply to this file type
    if (rule.scope === 'c-shim' && !isC) continue;
    if (rule.scope === 'scheme' && !isSS) continue;
    // ffi-boundary rules apply to .ss files (they contain c-lambda declarations)
    if (rule.scope === 'ffi-boundary' && !isSS) continue;

    // Skip rules below severity threshold
    if (SEVERITY_ORDER[rule.severity] > severityThreshold) continue;

    let regex: RegExp;
    try {
      regex = new RegExp(rule.pattern);
    } catch {
      continue; // Skip invalid regex patterns
    }

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const match = regex.exec(line);
      if (!match) continue;

      // Skip matches inside comments or strings
      if (isInCommentOrString(line, match.index, isC)) continue;

      // Special handling for port-open rule: skip if unwind-protect is nearby
      if (rule.id === 'port-open-no-unwind-protect' && hasUnwindProtect(lines, i)) {
        continue;
      }

      // Check for inline suppression comments
      const suppressed = hasSuppressionComment(lines, i, rule.id);

      findings.push({
        file: filePath,
        line: i + 1,
        severity: rule.severity,
        ruleId: rule.id,
        message: rule.message,
        remediation: rule.remediation,
        lineText: line.trimStart(),
        suppressed,
      });
    }
  }

  return findings;
}

async function scanDirectory(directory: string): Promise<string[]> {
  const results: string[] = [];
  await scanDirRecursive(directory, results);
  return results.sort();
}

async function scanDirRecursive(dir: string, results: string[]): Promise<void> {
  let entries: string[];
  try {
    entries = await readdir(dir);
  } catch {
    return;
  }

  for (const entry of entries) {
    if (entry.startsWith('.') || SKIP_DIRS.has(entry)) continue;

    const fullPath = join(dir, entry);
    try {
      const info = await stat(fullPath);
      if (info.isDirectory()) {
        await scanDirRecursive(fullPath, results);
      } else if (
        entry.endsWith('.ss') || entry.endsWith('.scm') ||
        entry.endsWith('.c') || entry.endsWith('.h')
      ) {
        results.push(fullPath);
      }
    } catch {
      // skip inaccessible entries
    }
  }
}

export function registerSecurityScanTool(server: McpServer): void {
  server.registerTool(
    'gerbil_security_scan',
    {
      title: 'Security Scanner',
      description:
        'Static security scanner for Gerbil code. Analyzes .ss and .c/.h files for ' +
        'known vulnerability patterns (shell injection, FFI type mismatches, resource leaks, ' +
        'unsafe C patterns). Reports findings with severity, line, and remediation guidance.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Single file to scan (.ss, .c, or .h)'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory to scan all .ss and .c/.h files recursively'),
        rules_path: z
          .string()
          .optional()
          .describe('Path to additional security-rules.json to merge with built-in rules'),
        severity_threshold: z
          .enum(['critical', 'high', 'medium', 'low'])
          .optional()
          .describe('Minimum severity to report (default: "low" — report everything)'),
      },
    },
    async ({ file_path, project_path, rules_path, severity_threshold }) => {
      if (!file_path && !project_path) {
        return {
          content: [{
            type: 'text' as const,
            text: 'Error: provide either file_path or project_path.',
          }],
          isError: true,
        };
      }

      // Load rules
      const builtinRules = loadSecurityRules(REPO_SECURITY_RULES_PATH);
      let rules = [...builtinRules];

      if (rules_path) {
        const extraRules = loadSecurityRules(rules_path);
        // Merge: extra rules with same ID replace built-in
        const ruleMap = new Map<string, SecurityRule>();
        for (const r of rules) ruleMap.set(r.id, r);
        for (const r of extraRules) ruleMap.set(r.id, r);
        rules = Array.from(ruleMap.values());
      }

      if (rules.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: 'No security rules loaded. Check that security-rules.json exists.',
          }],
          isError: true,
        };
      }

      const threshold = SEVERITY_ORDER[severity_threshold || 'low'];

      // Collect files to scan
      let filesToScan: string[] = [];
      if (file_path) {
        filesToScan = [file_path];
      } else if (project_path) {
        filesToScan = await scanDirectory(project_path);
      }

      // Scan each file
      const allFindings: SecurityFinding[] = [];
      for (const fp of filesToScan) {
        let content: string;
        try {
          content = await readFile(fp, 'utf-8');
        } catch (err) {
          const msg = err instanceof Error ? err.message : 'Unknown error';
          if (file_path) {
            return {
              content: [{
                type: 'text' as const,
                text: `Failed to read file: ${msg}`,
              }],
              isError: true,
            };
          }
          continue; // Skip unreadable files in project scan
        }
        const findings = scanFileContent(fp, content, rules, threshold);
        allFindings.push(...findings);
      }

      // Sort findings: by severity (critical first), then by file, then by line
      allFindings.sort((a, b) => {
        const sev = SEVERITY_ORDER[a.severity] - SEVERITY_ORDER[b.severity];
        if (sev !== 0) return sev;
        const fp = a.file.localeCompare(b.file);
        if (fp !== 0) return fp;
        return a.line - b.line;
      });

      // Format output
      const sections: string[] = [];
      if (project_path) {
        sections.push(`Security Scan: ${project_path}`);
        sections.push(`Files scanned: ${filesToScan.length}`);
      } else {
        sections.push(`Security Scan: ${file_path}`);
      }
      sections.push(`Rules loaded: ${rules.length}`);
      sections.push('');

      // Separate active and suppressed findings
      const activeFindings = allFindings.filter((f) => !f.suppressed);
      const suppressedFindings = allFindings.filter((f) => f.suppressed);

      if (activeFindings.length === 0 && suppressedFindings.length === 0) {
        sections.push('No security issues found.');
      } else if (activeFindings.length === 0) {
        sections.push(`No active security issues (${suppressedFindings.length} suppressed).`);
      } else {
        // Summary by severity (active only)
        const counts: Record<string, number> = { critical: 0, high: 0, medium: 0, low: 0 };
        for (const f of activeFindings) counts[f.severity]++;
        const summaryParts: string[] = [];
        for (const sev of ['critical', 'high', 'medium', 'low']) {
          if (counts[sev] > 0) summaryParts.push(`${counts[sev]} ${sev}`);
        }
        let findingsSummary = `Findings: ${activeFindings.length} (${summaryParts.join(', ')})`;
        if (suppressedFindings.length > 0) {
          findingsSummary += ` + ${suppressedFindings.length} suppressed`;
        }
        sections.push(findingsSummary);
        sections.push('');

        for (const f of activeFindings) {
          const sevTag = `[${f.severity.toUpperCase()}]`;
          const shortFile = project_path
            ? f.file.replace(project_path + '/', '')
            : f.file;
          sections.push(`${sevTag} ${shortFile}:${f.line} (${f.ruleId})`);
          sections.push(`  ${f.lineText}`);
          sections.push(`  → ${f.message}`);
          sections.push(`  Fix: ${f.remediation}`);
          sections.push('');
        }
      }

      // Report suppressed findings in a separate section
      if (suppressedFindings.length > 0) {
        sections.push(`Suppressed findings (${suppressedFindings.length}):`);
        for (const f of suppressedFindings) {
          const shortFile = project_path
            ? f.file.replace(project_path + '/', '')
            : f.file;
          sections.push(`  ${shortFile}:${f.line} (${f.ruleId}) — suppressed`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n').trimEnd() }],
        isError: activeFindings.some((f) => f.severity === 'critical'),
      };
    },
  );
}
