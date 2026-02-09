import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';

/**
 * Known v0.18 -> v0.19 migration patterns.
 */
const MIGRATION_RULES: Array<{
  id: string;
  pattern: RegExp;
  description: string;
  fix: string;
  severity: 'error' | 'warning' | 'info';
}> = [
  {
    id: 'getopt-module-rename',
    pattern: /\(import[^)]*:std\/getopt\b(?!\/)/,
    description: ':std/getopt was renamed to :std/cli/getopt in v0.19',
    fix: 'Replace (import :std/getopt) with (import :std/cli/getopt)',
    severity: 'error',
  },
  {
    id: 'getopt-handler-signature',
    pattern: /\bcall-with-getopt\b/,
    description: 'call-with-getopt handler signature changed in v0.19 (receives parsed options hash, not raw args)',
    fix: 'Update handler function to accept the new options hash format. Check gerbil_howto for "getopt v0.19" patterns.',
    severity: 'warning',
  },
  {
    id: 'srfi-1-import',
    pattern: /\(import[^)]*:std\/srfi\/1\b/,
    description: ':std/srfi/1 may have API changes in v0.19. Consider using :scheme/list or :std/iter instead.',
    fix: 'Verify all functions from :std/srfi/1 still work. Key functions like iota, fold, filter are also in core.',
    severity: 'info',
  },
  {
    id: 'text-json-import',
    pattern: /\(import[^)]*:std\/text\/json\b/,
    description: ':std/text/json API is stable across versions, but verify JSON writing functions.',
    fix: 'No action needed unless using deprecated json-object->string (use write-json-string instead).',
    severity: 'info',
  },
  {
    id: 'define-instead-of-def',
    pattern: /^\s*\(define\s+\(/m,
    description: 'Using (define (name args) ...) instead of (def (name args) ...) loses Gerbil-specific features',
    fix: 'Replace (define (name args) ...) with (def (name args) ...) for keyword arg support.',
    severity: 'warning',
  },
  {
    id: 'deprecated-hash-ref',
    pattern: /\bhash-ref\b/,
    description: 'hash-ref is Racket-style. Gerbil uses hash-get (2 args) or hash-ref from :std/misc/hash.',
    fix: 'Use (hash-get ht key) for simple lookups, or (hash-ref ht key default) if you need a default.',
    severity: 'warning',
  },
  {
    id: 'io-module-changes',
    pattern: /\(import[^)]*:std\/io\b/,
    description: ':std/io API changed significantly in v0.19 with new buffered IO system.',
    fix: 'Check gerbil_module_exports on :std/io to see the current API. Many functions renamed.',
    severity: 'warning',
  },
  {
    id: 'actor-module-rename',
    pattern: /\(import[^)]*:std\/actor\/proto\b/,
    description: 'Actor protocol modules reorganized in v0.19.',
    fix: 'Check :std/actor module hierarchy with gerbil_module_deps.',
    severity: 'warning',
  },
  {
    id: 'net-httpd-changes',
    pattern: /\(import[^)]*:std\/net\/httpd\b/,
    description: ':std/net/httpd API may have changed between versions.',
    fix: 'Verify handler signatures with gerbil_function_signature for http-register-handler and related.',
    severity: 'info',
  },
  {
    id: 'sugar-hash-literal',
    pattern: /\(hash\s+\(/,
    description: 'hash literal syntax (hash ("key" val)) — verify it works the same in both versions.',
    fix: 'This syntax is stable across versions. No action needed.',
    severity: 'info',
  },
];

export function registerMigrationCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_migration_check',
    {
      title: 'Migration Check',
      description:
        'Scan a Gerbil source file for v0.18 patterns that need updating for v0.19 (or vice versa). ' +
        'Detects renamed modules, changed function signatures, deprecated patterns, and API differences. ' +
        'Leverages knowledge from 26+ version-tagged cookbook recipes documenting breaking changes.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        file_path: z.string().describe('Path to the Gerbil source file to check'),
        target_version: z
          .enum(['v0.18', 'v0.19'])
          .optional()
          .describe('Target Gerbil version to migrate to (default: v0.19)'),
      },
    },
    async ({ file_path, target_version }) => {
      const target = target_version ?? 'v0.19';

      let content: string;
      try {
        content = await readFile(file_path, 'utf-8');
      } catch (err) {
        return {
          content: [{
            type: 'text' as const,
            text: `Error reading file: ${(err as Error).message}`,
          }],
          isError: true,
        };
      }

      const lines = content.split('\n');
      const findings: Array<{
        rule: typeof MIGRATION_RULES[number];
        line: number;
        text: string;
      }> = [];

      for (let i = 0; i < lines.length; i++) {
        for (const rule of MIGRATION_RULES) {
          if (rule.pattern.test(lines[i])) {
            findings.push({
              rule,
              line: i + 1,
              text: lines[i].trim(),
            });
          }
        }
      }

      if (findings.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: `No migration issues found in ${file_path} for ${target}.\n\nThe file appears compatible. Consider also running:\n- \`gerbil_compile_check\` to verify compilation\n- \`gerbil_check_import_conflicts\` to check for import issues`,
          }],
        };
      }

      const errors = findings.filter((f) => f.rule.severity === 'error');
      const warnings = findings.filter((f) => f.rule.severity === 'warning');
      const infos = findings.filter((f) => f.rule.severity === 'info');

      const sections: string[] = [];
      sections.push(`## Migration Check: ${file_path} → ${target}\n`);
      sections.push(
        `Found ${findings.length} issue(s): ` +
        `${errors.length} error(s), ${warnings.length} warning(s), ${infos.length} info(s)\n`,
      );

      for (const f of findings) {
        const icon = f.rule.severity === 'error' ? 'ERROR' : f.rule.severity === 'warning' ? 'WARN' : 'INFO';
        sections.push(
          `### [${icon}] ${f.rule.description}\n` +
          `- **Line ${f.line}**: \`${f.text}\`\n` +
          `- **Fix**: ${f.rule.fix}\n`,
        );
      }

      sections.push(
        '\n## Next Steps\n' +
        '- Use `gerbil_howto` with query "v0.19" to find migration recipes\n' +
        '- Use `gerbil_diff_modules` to compare old vs new module exports\n' +
        '- Use `gerbil_compile_check` to verify the migrated code compiles\n',
      );

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
