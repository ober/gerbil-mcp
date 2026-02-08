#!/usr/bin/env npx tsx
/**
 * Cross-version cookbook recipe tester.
 *
 * Tests cookbook recipes against multiple Gerbil installations and records
 * which versions each recipe is confirmed working on (valid_for field).
 *
 * Usage:
 *   npx tsx scripts/test-cookbooks.ts --gxi /path/to/v18/gxi --gxi /path/to/v19/gxi
 *   npx tsx scripts/test-cookbooks.ts --gxi $(which gxi) --dry-run
 *   npx tsx scripts/test-cookbooks.ts --gxi /path/to/gxi --recipe-id json-parse
 *   npx tsx scripts/test-cookbooks.ts --gxi /path/to/gxi --no-compile
 */

import { execFile } from 'node:child_process';
import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { RECIPES, loadCookbook, REPO_COOKBOOK_PATH, type Recipe } from '../src/tools/howto.js';
import {
  DEFAULT_BATCH_SIZE,
  runVerifyBatch,
  runCompileCheck,
} from '../src/tools/verify-utils.js';
import { mkdir } from 'node:fs/promises';
import { tmpdir } from 'node:os';

// ── Argument parsing ─────────────────────────────────────────────────

interface Args {
  gxiPaths: string[];
  dryRun: boolean;
  noCompile: boolean;
  recipeId?: string;
}

function parseArgs(): Args {
  const args = process.argv.slice(2);
  const gxiPaths: string[] = [];
  let dryRun = false;
  let noCompile = false;
  let recipeId: string | undefined;

  for (let i = 0; i < args.length; i++) {
    switch (args[i]) {
      case '--gxi':
        if (i + 1 >= args.length) {
          console.error('Error: --gxi requires a path argument');
          process.exit(1);
        }
        gxiPaths.push(args[++i]);
        break;
      case '--dry-run':
        dryRun = true;
        break;
      case '--no-compile':
        noCompile = true;
        break;
      case '--recipe-id':
        if (i + 1 >= args.length) {
          console.error('Error: --recipe-id requires an argument');
          process.exit(1);
        }
        recipeId = args[++i];
        break;
      case '--help':
      case '-h':
        console.log(`Usage: npx tsx scripts/test-cookbooks.ts [options]

Options:
  --gxi <path>       Path to a gxi binary (repeatable for multiple versions)
  --dry-run           Print results without modifying cookbooks.json
  --no-compile        Skip compile checking (syntax-only)
  --recipe-id <id>    Test only a specific recipe
  --help, -h          Show this help message`);
        process.exit(0);
        break;
      default:
        console.error(`Unknown argument: ${args[i]}`);
        process.exit(1);
    }
  }

  if (gxiPaths.length === 0) {
    console.error('Error: at least one --gxi <path> is required');
    process.exit(1);
  }

  return { gxiPaths, dryRun, noCompile, recipeId };
}

// ── Version detection ────────────────────────────────────────────────

function detectVersion(gxiPath: string): Promise<string> {
  return new Promise((resolve, reject) => {
    execFile(
      gxiPath,
      ['-e', '(display (gerbil-version-string))'],
      { timeout: 10_000 },
      (error, stdout) => {
        if (error) {
          reject(new Error(`Failed to detect version from ${gxiPath}: ${error.message}`));
          return;
        }
        const ver = stdout.trim();
        // Extract version up to build number, dropping git hash
        const match = ver.match(/^(v\d+\.\d+\.\d+(?:-\d+)?)/);
        resolve(match ? match[1] : ver);
      },
    );
  });
}

function deriveGxcPath(gxiPath: string): string {
  const dir = dirname(gxiPath);
  const base = gxiPath.endsWith('/gxi') || gxiPath === 'gxi' ? 'gxc' : gxiPath.replace(/gxi$/, 'gxc');
  return gxiPath === 'gxi' ? 'gxc' : join(dir, 'gxc');
}

// ── Recipe results tracking ──────────────────────────────────────────

interface VersionResult {
  version: string;
  gxiPath: string;
  gxcPath: string;
  syntaxResults: Map<string, { passed: boolean; error?: string }>;
  compileResults: Map<string, { passed: boolean; error?: string }>;
}

// ── Main ─────────────────────────────────────────────────────────────

async function main(): Promise<void> {
  const args = parseArgs();

  // Load all recipes (built-in + cookbooks.json)
  let recipes: Recipe[] = [...RECIPES];
  const external = loadCookbook(REPO_COOKBOOK_PATH);
  if (external.length > 0) {
    const externalIds = new Set(external.map((r) => r.id));
    recipes = recipes.filter((r) => !externalIds.has(r.id));
    recipes.push(...external);
  }

  // Filter by recipe-id if requested
  if (args.recipeId) {
    recipes = recipes.filter((r) => r.id === args.recipeId);
    if (recipes.length === 0) {
      console.error(`Error: recipe "${args.recipeId}" not found`);
      process.exit(1);
    }
  }

  console.log(`Testing ${recipes.length} recipes against ${args.gxiPaths.length} Gerbil installation(s)...\n`);

  const versionResults: VersionResult[] = [];

  // Process versions sequentially (different gxi binaries may conflict)
  for (const gxiPath of args.gxiPaths) {
    let version: string;
    try {
      version = await detectVersion(gxiPath);
    } catch (e) {
      console.error(`Error: ${e instanceof Error ? e.message : String(e)}`);
      process.exit(1);
    }

    const gxcPath = deriveGxcPath(gxiPath);
    console.log(`── ${version} (gxi: ${gxiPath}, gxc: ${gxcPath}) ──\n`);

    const result: VersionResult = {
      version,
      gxiPath,
      gxcPath,
      syntaxResults: new Map(),
      compileResults: new Map(),
    };

    // Syntax-check in batches
    let syntaxPass = 0;
    let syntaxFail = 0;
    for (let i = 0; i < recipes.length; i += DEFAULT_BATCH_SIZE) {
      const batch = recipes.slice(i, i + DEFAULT_BATCH_SIZE);
      let batchResults = await runVerifyBatch(batch, gxiPath, { timeout: 60_000 });

      // If any recipe didn't get a result (batch crash), retry individually
      const needsRetry = batchResults.filter(
        (r) => !r.passed && (r.error?.includes('batch crash') || r.error?.includes('batch failed')),
      );
      if (needsRetry.length > 0 && needsRetry.length < batch.length) {
        for (const failed of needsRetry) {
          const recipe = batch.find((r) => r.id === failed.id);
          if (recipe) {
            const [retryResult] = await runVerifyBatch([recipe], gxiPath, { timeout: 60_000 });
            const idx = batchResults.findIndex((r) => r.id === failed.id);
            if (idx >= 0) batchResults[idx] = retryResult;
          }
        }
      }

      for (const r of batchResults) {
        result.syntaxResults.set(r.id, { passed: r.passed, error: r.error });
        if (r.passed) syntaxPass++;
        else syntaxFail++;
      }
    }

    console.log(`  Syntax: ${syntaxPass} passed, ${syntaxFail} failed`);

    // Compile-check (only for syntax-passing recipes)
    if (!args.noCompile) {
      const syntaxPassed = recipes.filter((r) => result.syntaxResults.get(r.id)?.passed);
      if (syntaxPassed.length > 0) {
        const tempDir = join(tmpdir(), `gerbil-mcp-crossver-${version.replace(/[^a-z0-9.-]/gi, '_')}`);
        await mkdir(tempDir, { recursive: true });

        let compilePass = 0;
        let compileFail = 0;
        for (const recipe of syntaxPassed) {
          const cr = await runCompileCheck(recipe, gxcPath, tempDir, { timeout: 30_000 });
          result.compileResults.set(cr.id, { passed: cr.passed, error: cr.error });
          if (cr.passed) compilePass++;
          else compileFail++;
        }
        console.log(`  Compile: ${compilePass} passed, ${compileFail} failed`);
      }
    }

    versionResults.push(result);
    console.log('');
  }

  // Compute valid_for per recipe
  const validForMap = new Map<string, string[]>();
  for (const recipe of recipes) {
    const versions: string[] = [];
    for (const vr of versionResults) {
      const syntaxOk = vr.syntaxResults.get(recipe.id)?.passed ?? false;
      const compileOk = args.noCompile ? true : (vr.compileResults.get(recipe.id)?.passed ?? true);
      if (syntaxOk && compileOk) {
        versions.push(vr.version);
      }
    }
    if (versions.length > 0) {
      validForMap.set(recipe.id, versions);
    }
  }

  // Print summary
  console.log('── Summary ──\n');
  const allVersionLabels = versionResults.map((vr) => vr.version);
  console.log(`Versions tested: ${allVersionLabels.join(', ')}`);
  console.log(`Total recipes: ${recipes.length}`);

  const allPass = recipes.filter((r) => {
    const vf = validForMap.get(r.id);
    return vf && vf.length === versionResults.length;
  });
  const somePass = recipes.filter((r) => {
    const vf = validForMap.get(r.id);
    return vf && vf.length > 0 && vf.length < versionResults.length;
  });
  const nonePass = recipes.filter((r) => !validForMap.has(r.id));

  console.log(`  Pass all versions: ${allPass.length}`);
  if (versionResults.length > 1) {
    console.log(`  Pass some versions: ${somePass.length}`);
  }
  console.log(`  Pass no versions: ${nonePass.length}`);

  // Print per-recipe failures
  if (nonePass.length > 0 || somePass.length > 0) {
    console.log('\n── Failures ──\n');
    for (const recipe of [...nonePass, ...somePass]) {
      const parts: string[] = [`  ${recipe.id}:`];
      for (const vr of versionResults) {
        const syntaxOk = vr.syntaxResults.get(recipe.id)?.passed ?? false;
        const compileOk = args.noCompile ? true : (vr.compileResults.get(recipe.id)?.passed ?? true);
        if (!syntaxOk) {
          const err = vr.syntaxResults.get(recipe.id)?.error || 'syntax fail';
          parts.push(`${vr.version}=SYNTAX-FAIL(${err.slice(0, 60)})`);
        } else if (!compileOk) {
          const err = vr.compileResults.get(recipe.id)?.error || 'compile fail';
          parts.push(`${vr.version}=COMPILE-FAIL(${err.slice(0, 60)})`);
        } else {
          parts.push(`${vr.version}=PASS`);
        }
      }
      console.log(parts.join(' '));
    }
  }

  // Update cookbooks.json (unless dry-run)
  if (!args.dryRun) {
    let cookbookRecipes: Recipe[] = [];
    try {
      const raw = readFileSync(REPO_COOKBOOK_PATH, 'utf-8');
      const parsed = JSON.parse(raw);
      if (Array.isArray(parsed)) cookbookRecipes = parsed;
    } catch {
      // File doesn't exist or invalid — skip updating
      console.log('\nNo cookbooks.json found — skipping update.');
    }

    if (cookbookRecipes.length > 0) {
      let updated = 0;
      for (const recipe of cookbookRecipes) {
        const vf = validForMap.get(recipe.id);
        if (vf && vf.length > 0) {
          recipe.valid_for = vf;
          updated++;
        }
      }

      writeFileSync(REPO_COOKBOOK_PATH, JSON.stringify(cookbookRecipes, null, 2) + '\n');
      console.log(`\nUpdated ${updated} recipes in ${REPO_COOKBOOK_PATH}`);
    }
  } else {
    console.log('\n(dry-run: cookbooks.json not modified)');
  }

  // Exit code: 0 if all pass, 1 if any failures
  const hasFailures = nonePass.length > 0 || somePass.length > 0;
  process.exit(hasFailures ? 1 : 0);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
