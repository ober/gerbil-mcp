import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { join } from 'node:path';
import { runGxi, ERROR_MARKER, RESULT_MARKER, buildLoadpathEnv } from '../gxi.js';

interface ProcedureInfo {
  name: string;
  kind: 'procedure' | 'value' | 'unknown';
  /** ##subprocedure-nb-parameters result: positive = fixed arity, negative = variadic, -1 = unknown */
  arity?: number;
}

/**
 * Get exported symbols with their kinds and arities from a Gerbil module.
 * Imports the module first so that (eval name) can find the bound procedures.
 */
async function getModuleSignatures(
  modPath: string,
  env?: Record<string, string>,
): Promise<{ procs: ProcedureInfo[]; error?: string }> {
  // Build the introspection expression as a single joined string.
  // Uses ##subprocedure-nb-parameters (Gambit primitive) for arity; wraps in
  // with-catch because it errors on non-closures (builtins, primitives).
  const introspect = [
    `(with-catch`,
    `  (lambda (e) (display "${ERROR_MARKER}\\n") (display-exception e (current-output-port)))`,
    `  (lambda ()`,
    `    (let* ((mod (import-module '${modPath} #f #t))`,
    `           (exports (module-context-export mod)))`,
    `      (display "${RESULT_MARKER}\\n")`,
    `      (for-each`,
    `        (lambda (e)`,
    `          (let* ((name (module-export-name e))`,
    `                 (val (with-catch (lambda (_) #f) (lambda () (eval name)))))`,
    `            (cond`,
    `              ((procedure? val)`,
    `               (let ((nb (with-catch (lambda (_) -1)`,
    `                          (lambda () (##subprocedure-nb-parameters val)))))`,
    `                 (displayln "P " name " " nb)))`,
    `              ((and val (not (eq? val #f)) (not (eq? val #!void)))`,
    `               (displayln "V " name))`,
    `              (else (displayln "U " name)))))`,
    `        exports))))`,
  ].join(' ');

  const exprs = [
    `(import ${modPath})`,
    '(import :gerbil/expander)',
    introspect,
  ];

  const result = await runGxi(exprs, { env, timeout: 20_000 });

  if (result.timedOut) {
    return { procs: [], error: 'Timed out' };
  }

  const stdout = result.stdout;
  const errorIdx = stdout.indexOf(ERROR_MARKER);
  if (errorIdx !== -1) {
    return { procs: [], error: stdout.slice(errorIdx + ERROR_MARKER.length).trim() };
  }

  const resultIdx = stdout.indexOf(RESULT_MARKER);
  if (resultIdx === -1) {
    return { procs: [], error: 'No output' };
  }

  const procs: ProcedureInfo[] = [];
  const lines = stdout
    .slice(resultIdx + RESULT_MARKER.length)
    .trim()
    .split('\n')
    .map((s) => s.trim())
    .filter(Boolean);

  for (const line of lines) {
    const parts = line.split(' ');
    if (parts[0] === 'P' && parts.length >= 3) {
      procs.push({
        name: parts[1],
        kind: 'procedure',
        arity: parseInt(parts[2], 10),
      });
    } else if (parts[0] === 'V' && parts.length >= 2) {
      procs.push({ name: parts[1], kind: 'value' });
    } else if (parts[0] === 'U' && parts.length >= 2) {
      procs.push({ name: parts[1], kind: 'unknown' });
    }
  }

  return { procs };
}

/**
 * Format arity as a human-readable string.
 * ##subprocedure-nb-parameters returns: positive = fixed arity, negative = variadic, -1 = unknown.
 */
function formatArity(arity?: number): string {
  if (arity === undefined || arity === -1) return '?';
  if (arity < 0) return `${Math.abs(arity) - 1}+`; // variadic: -N means N-1 required + rest
  return `${arity}`;
}

export function registerCrossPackageDiffTool(server: McpServer): void {
  server.registerTool(
    'gerbil_cross_package_diff',
    {
      title: 'Cross-Package Function Diff',
      description:
        'Compare function signatures and export lists between two Gerbil modules, ' +
        'potentially from different packages. Identifies procedures with different arities, ' +
        'kind mismatches (value vs procedure), and symbols present in only one module. ' +
        'Useful for debugging wrapper/wrapped function mismatches where a shim in package A ' +
        'wraps a function from package B but has different arity or missing parameters. ' +
        'Imports each module and introspects procedure-arity to show exact differences.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        module_a: z
          .string()
          .describe('First module path (e.g. ":myproject/sci-shim")'),
        module_b: z
          .string()
          .describe('Second module path to compare against (e.g. ":gerbil-scintilla/scintilla")'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Directories to add to GERBIL_LOADPATH for resolving both modules'),
        project_path: z
          .string()
          .optional()
          .describe('Project directory — adds .gerbil/lib to GERBIL_LOADPATH automatically'),
        show_shared: z
          .boolean()
          .optional()
          .describe('Include shared symbols with identical signatures in output (default: false — only show differences)'),
      },
    },
    async ({ module_a, module_b, loadpath, project_path, show_shared }) => {
      const modA = module_a.startsWith(':') ? module_a : `:${module_a}`;
      const modB = module_b.startsWith(':') ? module_b : `:${module_b}`;

      const effectiveLoadpath: string[] = [...(loadpath ?? [])];
      if (project_path) {
        effectiveLoadpath.push(join(project_path, '.gerbil', 'lib'));
      }
      const env =
        effectiveLoadpath.length > 0 ? buildLoadpathEnv(effectiveLoadpath) : undefined;

      // Fetch signatures from both modules in parallel (separate gxi processes)
      const [resultA, resultB] = await Promise.all([
        getModuleSignatures(modA, env),
        getModuleSignatures(modB, env),
      ]);

      if (resultA.error && resultB.error) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to load both modules:\n- ${modA}: ${resultA.error}\n- ${modB}: ${resultB.error}`,
            },
          ],
          isError: true,
        };
      }

      const mapA = new Map<string, ProcedureInfo>(resultA.procs.map((p) => [p.name, p]));
      const mapB = new Map<string, ProcedureInfo>(resultB.procs.map((p) => [p.name, p]));

      const onlyInA = resultA.procs
        .filter((p) => !mapB.has(p.name))
        .sort((a, b) => a.name.localeCompare(b.name));
      const onlyInB = resultB.procs
        .filter((p) => !mapA.has(p.name))
        .sort((a, b) => a.name.localeCompare(b.name));

      const sharedDiff: Array<{ name: string; a: ProcedureInfo; b: ProcedureInfo }> = [];
      const sharedSame: Array<{ name: string; info: ProcedureInfo }> = [];

      for (const pa of resultA.procs) {
        const pb = mapB.get(pa.name);
        if (!pb) continue;

        const same =
          pa.kind === pb.kind &&
          pa.arity === pb.arity;

        if (same) {
          sharedSame.push({ name: pa.name, info: pa });
        } else {
          sharedDiff.push({ name: pa.name, a: pa, b: pb });
        }
      }

      sharedDiff.sort((a, b) => a.name.localeCompare(b.name));
      sharedSame.sort((a, b) => a.name.localeCompare(b.name));

      const sections: string[] = [];
      sections.push(`## Cross-Package Diff: ${modA} vs ${modB}\n`);

      if (resultA.error) {
        sections.push(`**Warning**: Could not fully load ${modA}: ${resultA.error}\n`);
      }
      if (resultB.error) {
        sections.push(`**Warning**: Could not fully load ${modB}: ${resultB.error}\n`);
      }

      sections.push('| | Count |');
      sections.push('|---|---|');
      sections.push(`| Only in ${modA} | ${onlyInA.length} |`);
      sections.push(`| Only in ${modB} | ${onlyInB.length} |`);
      sections.push(`| Shared — different signature | ${sharedDiff.length} |`);
      sections.push(`| Shared — same signature | ${sharedSame.length} |`);
      sections.push('');

      if (sharedDiff.length > 0) {
        sections.push(`### Signature Differences (${sharedDiff.length}):\n`);
        sections.push(`| Symbol | ${modA} | ${modB} |`);
        sections.push('|---|---|---|');
        for (const { name, a, b } of sharedDiff) {
          const descA =
            a.kind === 'procedure'
              ? `procedure (arity ${formatArity(a.arity)})`
              : a.kind;
          const descB =
            b.kind === 'procedure'
              ? `procedure (arity ${formatArity(b.arity)})`
              : b.kind;
          sections.push(`| ${name} | ${descA} | ${descB} |`);
        }
        sections.push('');
      }

      if (onlyInA.length > 0) {
        sections.push(`### Only in ${modA} (${onlyInA.length}):\n`);
        for (const p of onlyInA) {
          const desc =
            p.kind === 'procedure'
              ? ` — procedure (arity ${formatArity(p.arity)})`
              : p.kind !== 'unknown'
                ? ` — ${p.kind}`
                : '';
          sections.push(`  - ${p.name}${desc}`);
        }
        sections.push('');
      }

      if (onlyInB.length > 0) {
        sections.push(`### Only in ${modB} (${onlyInB.length}):\n`);
        for (const p of onlyInB) {
          const desc =
            p.kind === 'procedure'
              ? ` — procedure (arity ${formatArity(p.arity)})`
              : p.kind !== 'unknown'
                ? ` — ${p.kind}`
                : '';
          sections.push(`  + ${p.name}${desc}`);
        }
        sections.push('');
      }

      if (show_shared && sharedSame.length > 0) {
        sections.push(`### Shared — Same Signature (${sharedSame.length}):\n`);
        for (const { name, info } of sharedSame) {
          const desc =
            info.kind === 'procedure'
              ? ` — procedure (arity ${formatArity(info.arity)})`
              : info.kind !== 'unknown'
                ? ` — ${info.kind}`
                : '';
          sections.push(`    ${name}${desc}`);
        }
        sections.push('');
      }

      if (sharedDiff.length === 0 && onlyInA.length === 0 && onlyInB.length === 0) {
        sections.push(
          'No differences found — both modules export identical symbols with identical signatures.',
        );
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
