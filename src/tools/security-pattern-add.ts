/**
 * gerbil_security_pattern_add — Add security rules to the scanner.
 * Follows the same pattern as howto-add.ts for cookbook recipes.
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { dirname } from 'node:path';
import { z } from 'zod';
import type { SecurityRule } from './security-scan.js';
import { REPO_SECURITY_RULES_PATH } from './security-scan.js';

export function registerSecurityPatternAddTool(server: McpServer): void {
  server.registerTool(
    'gerbil_security_pattern_add',
    {
      title: 'Add Security Pattern',
      description:
        'Add a new security detection pattern to the scanner. ' +
        'If a rule with the same id already exists, it is replaced (update semantics). ' +
        'By default writes to the gerbil-mcp repo security-rules.json. ' +
        'Optionally specify rules_path to write to a different file.',
      inputSchema: {
        rules_path: z
          .string()
          .optional()
          .describe(
            'Absolute path to a JSON security rules file. If omitted, writes to the gerbil-mcp repo security-rules.json.',
          ),
        id: z
          .string()
          .describe('Unique rule identifier in kebab-case (e.g. "shell-injection-string-concat")'),
        title: z
          .string()
          .describe('Human-readable title (e.g. "Shell injection via string concatenation")'),
        severity: z
          .enum(['critical', 'high', 'medium', 'low'])
          .describe('Severity level of the vulnerability'),
        scope: z
          .enum(['scheme', 'c-shim', 'ffi-boundary'])
          .describe('Where the pattern applies: scheme (.ss), c-shim (.c/.h), or ffi-boundary (.ss c-lambda)'),
        pattern: z
          .string()
          .describe('Regex detection pattern to match in source lines'),
        message: z
          .string()
          .describe('Explanation of the vulnerability'),
        remediation: z
          .string()
          .describe('How to fix the issue'),
        related_recipe: z
          .string()
          .optional()
          .describe('Cookbook recipe ID with the fix pattern'),
        tags: z
          .array(z.string())
          .optional()
          .describe('Search keywords'),
      },
    },
    async ({ rules_path: explicitPath, id, title, severity, scope, pattern, message, remediation, related_recipe, tags }) => {
      const rulesPath = explicitPath || REPO_SECURITY_RULES_PATH;

      // Validate the regex pattern
      try {
        new RegExp(pattern);
      } catch (e: unknown) {
        return {
          content: [{
            type: 'text' as const,
            text: `Error: invalid regex pattern: ${e instanceof Error ? e.message : String(e)}`,
          }],
          isError: true,
        };
      }

      // Read existing rules or start fresh
      let rules: SecurityRule[] = [];
      try {
        const raw = readFileSync(rulesPath, 'utf-8');
        const parsed = JSON.parse(raw);
        if (!Array.isArray(parsed)) {
          return {
            content: [{
              type: 'text' as const,
              text: `Error: ${rulesPath} does not contain a JSON array.`,
            }],
            isError: true,
          };
        }
        rules = parsed;
      } catch (e: unknown) {
        if ((e as NodeJS.ErrnoException).code === 'ENOENT') {
          rules = [];
        } else {
          return {
            content: [{
              type: 'text' as const,
              text: `Error reading ${rulesPath}: ${e instanceof Error ? e.message : String(e)}`,
            }],
            isError: true,
          };
        }
      }

      // Build the new rule
      const rule: SecurityRule = { id, title, severity, scope, pattern, message, remediation };
      if (related_recipe) rule.related_recipe = related_recipe;
      if (tags && tags.length > 0) rule.tags = tags;

      // Replace existing rule with same id, or append
      const existingIdx = rules.findIndex((r) => r.id === id);
      if (existingIdx >= 0) {
        rules[existingIdx] = rule;
      } else {
        rules.push(rule);
      }

      // Write back
      try {
        mkdirSync(dirname(rulesPath), { recursive: true });
        writeFileSync(rulesPath, JSON.stringify(rules, null, 2) + '\n');
      } catch (e: unknown) {
        return {
          content: [{
            type: 'text' as const,
            text: `Error writing ${rulesPath}: ${e instanceof Error ? e.message : String(e)}`,
          }],
          isError: true,
        };
      }

      const action = existingIdx >= 0 ? 'Updated' : 'Added';
      return {
        content: [{
          type: 'text' as const,
          text: `${action} security rule "${id}" [${severity}] in ${rulesPath} (${rules.length} total rules).`,
        }],
      };
    },
  );
}
