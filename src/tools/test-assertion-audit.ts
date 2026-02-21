import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { relative } from 'node:path';
import { scanSchemeFiles } from './parse-utils.js';

interface AssertionIssue {
  file: string;
  line: number;
  code: string;
  message: string;
  severity: 'warning' | 'error';
  lineText: string;
}

/**
 * Check patterns for common :std/test assertion mistakes:
 * 1. (check x ? #f) — predicate #f always fails silently
 * 2. (check x ? values) — tests truthiness, doesn't verify value
 * 3. (check expr => expected) where types likely mismatch
 * 4. Suspicious check patterns that silently pass
 */
function auditCheckExpressions(
  content: string,
  filePath: string,
): AssertionIssue[] {
  const issues: AssertionIssue[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const trimmed = line.trim();
    const lineNum = i + 1;

    // Skip comments
    if (trimmed.startsWith(';') || trimmed.startsWith('#;')) continue;

    // Pattern 1: (check X ? #f) — predicate #f is always false, silently warns
    const checkPredFalse = trimmed.match(
      /\(check\s+.+\s+\?\s+#f\s*\)/,
    );
    if (checkPredFalse) {
      issues.push({
        file: filePath,
        line: lineNum,
        code: 'check-pred-false',
        message:
          '(check X ? #f) — #f as predicate always returns false. ' +
          'Did you mean (check X => #f)?',
        severity: 'error',
        lineText: trimmed,
      });
    }

    // Pattern 2: (check X ? values) — tests truthiness only, doesn't verify value
    const checkPredValues = trimmed.match(
      /\(check\s+.+\s+\?\s+values\s*\)/,
    );
    if (checkPredValues) {
      issues.push({
        file: filePath,
        line: lineNum,
        code: 'check-pred-values',
        message:
          '(check X ? values) — "values" as predicate always returns true for non-#f values. ' +
          'Consider using (check X => expected) for specific value comparison.',
        severity: 'warning',
        lineText: trimmed,
      });
    }

    // Pattern 3: (check #f ? values) — the result is #f which makes (? values)
    // return #t since values is truthy, but the check is meaningless
    const checkFalsePred = trimmed.match(
      /\(check\s+#f\s+\?\s+\w+\s*\)/,
    );
    if (checkFalsePred && !checkPredFalse) {
      issues.push({
        file: filePath,
        line: lineNum,
        code: 'check-false-pred',
        message:
          'Checking #f with a predicate — the expression evaluated to #f ' +
          'which means the operation likely failed. The predicate test is meaningless.',
        severity: 'warning',
        lineText: trimmed,
      });
    }

    // Pattern 4: (check X => 'Y) where X likely returns a struct/object
    // Common when comparing match results to expected strings
    // Detect: check (some-match-fn ...) => '("a" "b") where fn returns struct
    const matchFnCheck = trimmed.match(
      /\(check\s+\((pcre2?-match|regexp-match|pregexp-match)\s/,
    );
    if (matchFnCheck) {
      // If comparing against a list literal, it's likely wrong (match returns struct/vector)
      if (trimmed.match(/=>\s+'\(/)) {
        issues.push({
          file: filePath,
          line: lineNum,
          code: 'check-match-type-mismatch',
          message:
            `${matchFnCheck[1]} returns a match object, not a list. ` +
            'Comparing with => against a list will silently fail.',
          severity: 'warning',
          lineText: trimmed,
        });
      }
    }

    // Pattern 5: (check (vector? X) ? values) where X might not be a vector
    // — if X is a pair, vector? returns #f, and (? values) on #f warns but doesn't fail
    const checkTypeTestPred = trimmed.match(
      /\(check\s+\((vector\?|list\?|string\?|number\?|pair\?|hash-table\?)\s+.+\)\s+\?\s+values\s*\)/,
    );
    if (checkTypeTestPred) {
      issues.push({
        file: filePath,
        line: lineNum,
        code: 'check-type-pred-values',
        message:
          `(check (${checkTypeTestPred[1]} ...) ? values) — if the type check returns #f, ` +
          'this silently warns but does not fail the test suite. ' +
          'Use (check (${checkTypeTestPred[1]} ...) => #t) instead.',
        severity: 'error',
        lineText: trimmed,
      });
    }

    // Pattern 6: (check expr) with no operator — missing => or ?
    const checkBare = trimmed.match(
      /^\(check\s+[^)]+\)\s*$/,
    );
    if (checkBare && !trimmed.includes('=>') && !trimmed.includes(' ? ')) {
      // Make sure it's not check-equal?, check-expect, etc.
      if (
        trimmed.startsWith('(check ') &&
        !trimmed.startsWith('(check-')
      ) {
        issues.push({
          file: filePath,
          line: lineNum,
          code: 'check-missing-operator',
          message:
            'check expression without => or ? operator. ' +
            'This only tests that the expression doesn\'t throw, not its value.',
          severity: 'warning',
          lineText: trimmed,
        });
      }
    }

    // Pattern 7: check with => comparing against wrong type — string vs number
    // e.g. (check (string-length "abc") => "3") — comparing number to string
    const stringLenCheck = trimmed.match(
      /\(check\s+\(string-length\s+.+\)\s+=>\s+"(\d+)"\s*\)/,
    );
    if (stringLenCheck) {
      issues.push({
        file: filePath,
        line: lineNum,
        code: 'check-string-number-mismatch',
        message:
          `string-length returns a number but compared to string "${stringLenCheck[1]}". ` +
          `Use => ${stringLenCheck[1]} (without quotes) instead.`,
        severity: 'error',
        lineText: trimmed,
      });
    }
  }

  return issues;
}

export function registerTestAssertionAuditTool(server: McpServer): void {
  server.registerTool(
    'gerbil_test_assertion_audit',
    {
      title: 'Audit Test Assertions',
      description:
        'Statically analyze :std/test files for common assertion mistakes that silently pass. ' +
        'Detects: (check X ? #f) where #f predicate always fails, ' +
        '(check X ? values) which only tests truthiness, ' +
        'type mismatches in => comparisons, and missing operators. ' +
        'Scans a single file or all *-test.ss files in a directory.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z
          .string()
          .optional()
          .describe('Path to a single test file to audit'),
        directory: z
          .string()
          .optional()
          .describe(
            'Directory to scan for *-test.ss files. ' +
            'Cannot be used with file_path.',
          ),
      },
    },
    async ({ file_path, directory }) => {
      if (file_path && directory) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Cannot specify both file_path and directory.',
            },
          ],
          isError: true,
        };
      }

      if (!file_path && !directory) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Either file_path or directory is required.',
            },
          ],
          isError: true,
        };
      }

      const filesToAudit: string[] = [];

      if (file_path) {
        filesToAudit.push(file_path);
      } else if (directory) {
        const allFiles = await scanSchemeFiles(directory);
        const testFiles = allFiles.filter((f) => f.endsWith('-test.ss'));
        if (testFiles.length === 0) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `No *-test.ss files found in ${directory}.`,
              },
            ],
          };
        }
        filesToAudit.push(...testFiles);
      }

      const allIssues: AssertionIssue[] = [];

      for (const f of filesToAudit) {
        let content: string;
        try {
          content = await readFile(f, 'utf-8');
        } catch {
          continue;
        }
        const issues = auditCheckExpressions(content, f);
        allIssues.push(...issues);
      }

      if (allIssues.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Assertion audit: ${filesToAudit.length} file(s) checked, no issues found.`,
            },
          ],
        };
      }

      const baseDir = directory || '';
      const sections: string[] = [
        `Assertion audit: ${allIssues.length} issue(s) in ${filesToAudit.length} file(s)`,
        '',
      ];

      for (const issue of allIssues) {
        const relFile = baseDir
          ? relative(baseDir, issue.file)
          : issue.file;
        const sev = issue.severity === 'error' ? 'ERROR' : 'WARNING';
        sections.push(
          `  [${sev}] ${relFile}:${issue.line} (${issue.code})`,
        );
        sections.push(`    ${issue.message}`);
        sections.push(`    > ${issue.lineText}`);
        sections.push('');
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: allIssues.some((i) => i.severity === 'error'),
      };
    },
  );
}
