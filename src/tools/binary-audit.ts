import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { stat } from 'node:fs/promises';

const execFileAsync = promisify(execFile);

interface LeakEntry {
  category: string;
  severity: 'high' | 'medium' | 'low';
  count: number;
  examples: string[];
  remediation: string;
}

/**
 * Classify a string as a specific leak category.
 */
function classifyString(s: string): { category: string; severity: 'high' | 'medium' | 'low' } | null {
  // File paths
  if (s.match(/\/home\/|\/usr\/|\/opt\/|\/tmp\/|\/var\//) && s.length > 5) {
    return { category: 'file-path', severity: 'high' };
  }
  // Gerbil/Gambit module paths (containing # or /)
  if (s.match(/^[a-zA-Z][a-zA-Z0-9_-]*\/[a-zA-Z]/) && s.includes('#')) {
    return { category: 'scheme-symbol', severity: 'high' };
  }
  // Module-style paths without # (e.g., std/text/json)
  if (s.match(/^[a-zA-Z][a-zA-Z0-9_-]*\/[a-zA-Z][a-zA-Z0-9_/-]*$/) && s.length > 5) {
    return { category: 'module-path', severity: 'medium' };
  }
  // Gambit version strings
  if (s.match(/v\d+\.\d+\.\d+.*gambit|Gambit/i)) {
    return { category: 'version-string', severity: 'medium' };
  }
  // Gerbil version strings
  if (s.match(/gerbil|Gerbil/) && s.length > 3) {
    return { category: 'version-string', severity: 'medium' };
  }
  // Configure commands
  if (s.match(/--prefix=|--enable-|--disable-|--with-|configure\s/)) {
    return { category: 'configure-command', severity: 'high' };
  }
  // GCC/compiler info
  if (s.match(/gcc|g\+\+|clang/) && s.match(/\d+\.\d+/)) {
    return { category: 'compiler-info', severity: 'low' };
  }
  // Scheme-style identifiers (containing ? ! or ->)
  if (s.match(/[a-z]+-[a-z]+[?!]$/) || s.match(/->/) || s.match(/^make-[a-z]/)) {
    if (s.length > 4) {
      return { category: 'scheme-symbol', severity: 'high' };
    }
  }
  return null;
}

export function registerBinaryAuditTool(server: McpServer): void {
  server.registerTool(
    'gerbil_binary_audit',
    {
      title: 'Binary Information Leak Audit',
      description:
        'Audit a compiled Gerbil/Gambit ELF binary for information leaks. Scans for ' +
        'embedded symbol names, module paths, file paths, version strings, configure ' +
        'commands, and compiler metadata using `strings` and `readelf`. Reports findings ' +
        'categorized by type and severity, with specific remediation steps (strip, objcopy, ' +
        'link file obfuscation) for each category.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        binary_path: z
          .string()
          .describe('Path to the compiled ELF binary to audit'),
        min_length: z
          .number()
          .optional()
          .describe('Minimum string length to scan (default: 4)'),
      },
    },
    async ({ binary_path, min_length }) => {
      if (!binary_path) {
        return {
          content: [
            { type: 'text' as const, text: 'The "binary_path" parameter is required.' },
          ],
          isError: true,
        };
      }

      // Verify file exists
      try {
        const st = await stat(binary_path);
        if (!st.isFile()) {
          return {
            content: [
              { type: 'text' as const, text: `Not a file: ${binary_path}` },
            ],
            isError: true,
          };
        }
      } catch {
        return {
          content: [
            { type: 'text' as const, text: `File not found: ${binary_path}` },
          ],
          isError: true,
        };
      }

      const minLen = min_length ?? 4;

      // Run strings to extract readable strings
      let stringsOutput: string;
      try {
        const result = await execFileAsync('strings', ['-n', String(minLen), binary_path], {
          maxBuffer: 10 * 1024 * 1024, // 10MB
          timeout: 30000,
        });
        stringsOutput = result.stdout;
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            { type: 'text' as const, text: `Failed to run strings: ${msg}` },
          ],
          isError: true,
        };
      }

      // Check if file is stripped
      let isStripped = false;
      try {
        const result = await execFileAsync('file', [binary_path], { timeout: 5000 });
        isStripped = result.stdout.includes('stripped') && !result.stdout.includes('not stripped');
      } catch {
        // ignore
      }

      // Classify all strings
      const leaks = new Map<string, LeakEntry>();
      const allStrings = stringsOutput.split('\n').filter((s) => s.trim().length >= minLen);

      for (const s of allStrings) {
        const trimmed = s.trim();
        const classification = classifyString(trimmed);
        if (!classification) continue;

        const { category, severity } = classification;
        if (!leaks.has(category)) {
          leaks.set(category, {
            category,
            severity,
            count: 0,
            examples: [],
            remediation: getRemediation(category),
          });
        }
        const entry = leaks.get(category)!;
        entry.count++;
        // Upgrade severity if needed
        const severityOrder = { high: 0, medium: 1, low: 2 };
        if (severityOrder[severity] < severityOrder[entry.severity]) {
          entry.severity = severity;
        }
        if (entry.examples.length < 5) {
          entry.examples.push(trimmed.length > 80 ? trimmed.slice(0, 77) + '...' : trimmed);
        }
      }

      // Count Gambit ___sym/___glo/___sup name arrays specifically
      const symNameCount = allStrings.filter((s) =>
        s.match(/___DEF_SYM|___DEF_GLO|___DEF_SUP/) ||
        s.match(/___sym_names|___glo_names|___sup_names/)
      ).length;

      if (leaks.size === 0 && symNameCount === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Binary audit: ${binary_path}\n` +
                `No significant information leaks detected.\n` +
                `Stripped: ${isStripped ? 'yes' : 'no'}\n` +
                `Total strings scanned: ${allStrings.length}`,
            },
          ],
        };
      }

      // Build report
      const sections: string[] = [
        `Binary Information Leak Audit: ${binary_path}`,
        `Stripped: ${isStripped ? 'yes' : 'no'}`,
        `Total strings scanned: ${allStrings.length}`,
        '',
      ];

      // Sort by severity
      const sortedLeaks = [...leaks.values()].sort((a, b) => {
        const order = { high: 0, medium: 1, low: 2 };
        return order[a.severity] - order[b.severity];
      });

      const highCount = sortedLeaks.filter((l) => l.severity === 'high').length;
      const medCount = sortedLeaks.filter((l) => l.severity === 'medium').length;
      const lowCount = sortedLeaks.filter((l) => l.severity === 'low').length;
      const totalLeaks = sortedLeaks.reduce((sum, l) => sum + l.count, 0);

      sections.push(
        `Found ${totalLeaks} potential leaks in ${sortedLeaks.length} categories:`,
        `  ${highCount} HIGH, ${medCount} MEDIUM, ${lowCount} LOW`,
        '',
      );

      for (const leak of sortedLeaks) {
        sections.push(
          `[${leak.severity.toUpperCase()}] ${leak.category} (${leak.count} occurrence${leak.count === 1 ? '' : 's'})`,
        );
        for (const ex of leak.examples) {
          sections.push(`  • ${ex}`);
        }
        sections.push(`  Remediation: ${leak.remediation}`);
        sections.push('');
      }

      if (!isStripped) {
        sections.push('General recommendation: strip -s ' + binary_path);
        sections.push('');
      }

      sections.push('For maximum obfuscation of Scheme symbol names:');
      sections.push('1. Post-process the link file (*__exe_.c) to replace ___DEF_SYM strings');
      sections.push('2. Use strip -s to remove ELF symbol table');
      sections.push('3. Use objcopy --remove-section=.comment --remove-section=.note.* to remove metadata');

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: highCount > 0,
      };
    },
  );
}

function getRemediation(category: string): string {
  switch (category) {
    case 'file-path':
      return 'Build in a clean directory or use objcopy to patch path strings. Remove -g debug flags.';
    case 'scheme-symbol':
      return 'Obfuscate the link file (*__exe_.c) before gcc compilation to replace symbol name strings.';
    case 'module-path':
      return 'Obfuscate the link file — module paths are embedded in ___sym_names/___glo_names arrays.';
    case 'version-string':
      return 'Strip with: strip -s <binary>. Remove .comment section: objcopy --remove-section=.comment';
    case 'configure-command':
      return 'Remove .comment section: objcopy --remove-section=.comment --remove-section=.note.*';
    case 'compiler-info':
      return 'Remove .comment section: objcopy --remove-section=.comment';
    default:
      return 'Review and consider stripping or obfuscating.';
  }
}
