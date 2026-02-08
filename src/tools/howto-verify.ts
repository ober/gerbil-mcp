import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { writeFile, unlink, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { runGxi, runGxc, escapeSchemeString } from '../gxi.js';
import { RECIPES, loadCookbook, REPO_COOKBOOK_PATH, type Recipe } from './howto.js';

const PASS_MARKER = 'GERBIL-MCP-VERIFY-PASS:';
const FAIL_MARKER = 'GERBIL-MCP-VERIFY-FAIL:';
const BATCH_SIZE = 5;

export function registerHowtoVerifyTool(server: McpServer): void {
  server.registerTool(
    'gerbil_howto_verify',
    {
      title: 'Verify Cookbook Recipes',
      description:
        'Verify that cookbook recipes have valid syntax by checking their imports and code ' +
        'against the Gerbil expander. Does not execute recipes — only checks that they parse ' +
        'and expand correctly. Reports pass/fail for each recipe. ' +
        'Use compile_check: true to also run gxc compilation, which catches unbound identifiers ' +
        'and other issues that the expander alone misses (e.g. REPL-only patterns).',
      inputSchema: {
        cookbook_path: z
          .string()
          .optional()
          .describe(
            'Absolute path to an external cookbook JSON file to verify (merged with built-in recipes)',
          ),
        recipe_id: z
          .string()
          .optional()
          .describe('Single recipe ID to verify. If omitted, verifies all recipes.'),
        compile_check: z
          .boolean()
          .optional()
          .describe(
            'If true, also compile-check recipes with gxc (not just syntax expansion). ' +
            'This catches unbound identifiers and REPL-only patterns that fail during compilation. ' +
            'Slower but more thorough. Default: false.',
          ),
        gerbil_version: z
          .string()
          .optional()
          .describe(
            'Only verify recipes matching this Gerbil version (e.g. "v0.18", "v0.19"). ' +
            'Untagged recipes are always included. If omitted, verifies all recipes.',
          ),
      },
    },
    async ({ cookbook_path, recipe_id, compile_check, gerbil_version }) => {
      // Load and merge recipes
      let recipes: Recipe[] = [...RECIPES];
      const sources = [REPO_COOKBOOK_PATH];
      if (cookbook_path) sources.push(cookbook_path);
      for (const src of sources) {
        const external = loadCookbook(src);
        if (external.length > 0) {
          const externalIds = new Set(external.map((r) => r.id));
          recipes = recipes.filter((r) => !externalIds.has(r.id));
          recipes.push(...external);
        }
      }

      // Filter by recipe_id if provided
      if (recipe_id) {
        recipes = recipes.filter((r) => r.id === recipe_id);
        if (recipes.length === 0) {
          return {
            content: [
              {
                type: 'text' as const,
                text: `Recipe "${recipe_id}" not found.`,
              },
            ],
            isError: true,
          };
        }
      }

      // Filter by gerbil_version if provided (untagged recipes always included)
      if (gerbil_version) {
        recipes = recipes.filter(
          (r) => !r.gerbil_version || r.gerbil_version === gerbil_version,
        );
      }

      // Verify in batches (syntax expansion)
      const results: Array<{ id: string; passed: boolean; error?: string; compileError?: string }> = [];

      for (let i = 0; i < recipes.length; i += BATCH_SIZE) {
        const batch = recipes.slice(i, i + BATCH_SIZE);
        const batchResults = await verifyBatch(batch);
        results.push(...batchResults);
      }

      // Compile-check if requested (only for recipes that passed syntax check)
      if (compile_check) {
        const passedRecipes = results
          .filter((r) => r.passed)
          .map((r) => recipes.find((rec) => rec.id === r.id)!)
          .filter(Boolean);

        if (passedRecipes.length > 0) {
          const compileResults = await compileCheckRecipes(passedRecipes);
          for (const cr of compileResults) {
            const existing = results.find((r) => r.id === cr.id);
            if (existing && !cr.passed) {
              existing.compileError = cr.error;
            }
          }
        }
      }

      // Format output
      const mode = compile_check ? 'syntax + compile' : 'syntax';
      const versionLabel = gerbil_version ? `, version: ${gerbil_version}` : '';
      const syntaxPassed = results.filter((r) => r.passed);
      const syntaxFailed = results.filter((r) => !r.passed);
      const compileFailed = compile_check
        ? results.filter((r) => r.passed && r.compileError)
        : [];

      const sections: string[] = [
        `Cookbook verification (${mode}${versionLabel}): ${results.length} recipe(s) checked`,
        '',
      ];

      // Build a map of recipe id -> version for display
      const recipeVersionMap = new Map<string, string | undefined>();
      for (const rec of recipes) {
        recipeVersionMap.set(rec.id, rec.gerbil_version);
      }

      for (const r of results) {
        const ver = recipeVersionMap.get(r.id);
        const verTag = ver ? ` [${ver}]` : '';
        if (!r.passed) {
          sections.push(`  FAIL  ${r.id}${verTag} — ${r.error || 'unknown error'}`);
        } else if (r.compileError) {
          sections.push(`  COMPILE-FAIL  ${r.id}${verTag} — ${r.compileError} (repl_only?)`);
        } else {
          sections.push(`  PASS  ${r.id}${verTag}`);
        }
      }

      sections.push('');
      const summaryParts = [`${syntaxPassed.length} syntax passed`, `${syntaxFailed.length} syntax failed`];
      if (compile_check) {
        summaryParts.push(`${compileFailed.length} compile failed`);
      }
      sections.push(`Summary: ${summaryParts.join(', ')}`);

      const hasFailures = syntaxFailed.length > 0 || compileFailed.length > 0;
      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: hasFailures,
      };
    },
  );
}

async function verifyBatch(
  recipes: Recipe[],
): Promise<Array<{ id: string; passed: boolean; error?: string }>> {
  // Collect all unique imports across the batch — these must be at top level,
  // not inside a lambda (import is an expansion-time form, not a runtime form).
  const allImports = new Set<string>();
  for (const recipe of recipes) {
    for (const imp of recipe.imports) {
      allImports.add(imp);
    }
  }
  const topLevelImports = [...allImports].map((imp) => `(import ${imp})`).join(' ');

  // Build a check expression per recipe (imports are already at top level)
  const checks = recipes.map((recipe) => {
    const escapedId = escapeSchemeString(recipe.id);
    const escapedCode = escapeSchemeString(recipe.code);

    return [
      '(with-catch',
      '  (lambda (e)',
      `    (display "${FAIL_MARKER}${escapedId}\\t")`,
      '    (display-exception e (current-output-port)))',
      '  (lambda ()',
      `    (let ((p (open-input-string "${escapedCode}")))`,
      '      (let loop ((form (read p)))',
      '        (unless (eof-object? form)',
      // Skip import forms — they are handled at top level and
      // core-expand cannot process them inside eval context
      '          (unless (and (pair? form) (memq (car form) (quote (import export))))',
      '            (core-expand form))',
      '          (loop (read p)))))',
      `    (display "${PASS_MARKER}${escapedId}\\n")))`,
    ]
      .join(' ');
  });

  // Pass imports and checks as separate -e arguments.
  // (import :gerbil/expander) combined with other forms in a single -e
  // causes gxi to swallow all output from subsequent expressions.
  const importExpr = `(import :gerbil/expander) ${topLevelImports}`;
  const checksExpr = checks.join(' ');

  const result = await runGxi([importExpr, checksExpr], { timeout: 60_000 });

  // Parse results
  const output = result.stdout;
  const resultMap = new Map<string, { passed: boolean; error?: string }>();

  for (const line of output.split('\n')) {
    if (line.startsWith(PASS_MARKER)) {
      const id = line.slice(PASS_MARKER.length).trim();
      resultMap.set(id, { passed: true });
    } else if (line.startsWith(FAIL_MARKER)) {
      const rest = line.slice(FAIL_MARKER.length);
      const tabIdx = rest.indexOf('\t');
      if (tabIdx !== -1) {
        const id = rest.slice(0, tabIdx);
        const error = rest.slice(tabIdx + 1).trim();
        resultMap.set(id, { passed: false, error });
      } else {
        const id = rest.trim();
        resultMap.set(id, { passed: false, error: 'verification failed' });
      }
    }
  }

  // Build results preserving order, marking missing ones as failed
  return recipes.map((recipe) => {
    const r = resultMap.get(recipe.id);
    if (r) {
      return { id: recipe.id, ...r };
    }
    // If a recipe didn't produce output, it might have crashed the batch
    if (result.timedOut) {
      return { id: recipe.id, passed: false, error: 'batch timed out' };
    }
    if (result.exitCode !== 0) {
      return {
        id: recipe.id,
        passed: false,
        error: `batch failed (exit ${result.exitCode})`,
      };
    }
    return { id: recipe.id, passed: false, error: 'no result (batch crash?)' };
  });
}

/**
 * Compile-check recipes by writing each to a temp .ss file and running gxc -S.
 * This catches unbound identifiers and patterns that work in the REPL but fail
 * when compiled (e.g. destructuring in for bindings).
 */
async function compileCheckRecipes(
  recipes: Recipe[],
): Promise<Array<{ id: string; passed: boolean; error?: string }>> {
  const tempDir = join(tmpdir(), 'gerbil-mcp-verify');
  await mkdir(tempDir, { recursive: true });

  const results: Array<{ id: string; passed: boolean; error?: string }> = [];

  for (const recipe of recipes) {
    const importLines = recipe.imports
      .map((imp) => `(import ${imp})`)
      .join('\n');
    const content = `${importLines}\n${recipe.code}\n`;
    const filePath = join(tempDir, `verify-${recipe.id}.ss`);

    try {
      await writeFile(filePath, content, 'utf-8');
      const result = await runGxc(filePath, { timeout: 30_000 });

      if (result.exitCode === 0) {
        results.push({ id: recipe.id, passed: true });
      } else {
        const combined = [result.stdout, result.stderr]
          .filter(Boolean)
          .join('\n')
          .trim();
        // Extract the most relevant error line
        const errorLine = extractCompileError(combined);
        results.push({
          id: recipe.id,
          passed: false,
          error: errorLine || 'compilation failed',
        });
      }
    } catch (e) {
      results.push({
        id: recipe.id,
        passed: false,
        error: `exception: ${e instanceof Error ? e.message : String(e)}`,
      });
    } finally {
      try {
        await unlink(filePath);
      } catch {
        // ignore cleanup errors
      }
    }
  }

  return results;
}

/**
 * Extract a concise error message from gxc output.
 */
function extractCompileError(output: string): string {
  // Look for "Reference to unbound identifier" or similar
  for (const line of output.split('\n')) {
    if (/unbound identifier/i.test(line)) {
      return line.trim();
    }
    if (/Syntax Error/i.test(line)) {
      return line.trim();
    }
    if (/error/i.test(line) && line.trim().length > 5) {
      return line.trim();
    }
  }
  // Return first non-empty line as fallback
  const firstLine = output.split('\n').find((l) => l.trim().length > 0);
  return firstLine?.trim() || output.slice(0, 200);
}
