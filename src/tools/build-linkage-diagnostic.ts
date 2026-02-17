import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { readFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join, dirname, basename, resolve } from 'node:path';
import { z } from 'zod';
import { runGxi, buildLoadpathEnv } from '../gxi.js';
import { scanSchemeFiles, parseBuildTargets } from './parse-utils.js';

/** FFI markers that indicate a module uses C interop */
const FFI_MARKERS = [
  'begin-foreign', 'begin-ffi', 'c-lambda', 'c-declare', 'c-define',
  'c-define-type', 'define-c-lambda',
];

/**
 * Extract ld-options, cc-options, and pkg-config entries for a specific exe
 * target from build.ss content. Falls back to global ld-options if no
 * per-target options found.
 */
function extractExeLdOptions(
  content: string,
  exeModule?: string,
): { ldFlags: string[]; ccFlags: string[]; pkgConfigs: string[]; staticLibs: string[] } {
  const ldFlags: string[] = [];
  const ccFlags: string[] = [];
  const pkgConfigs: string[] = [];
  const staticLibs: string[] = [];

  // Extract ld-options: values
  const ldPattern = /ld-options:\s*"([^"]*)"/g;
  let match;
  while ((match = ldPattern.exec(content)) !== null) {
    const flags = match[1].trim();
    for (const flag of flags.split(/\s+/)) {
      if (flag.endsWith('.a')) {
        staticLibs.push(flag);
      } else if (flag) {
        ldFlags.push(flag);
      }
    }
  }

  // Also check for (ld-options: "...") form with parens
  const ldPattern2 = /\(ld-options:\s+"([^"]*)"\)/g;
  while ((match = ldPattern2.exec(content)) !== null) {
    const flags = match[1].trim();
    for (const flag of flags.split(/\s+/)) {
      if (flag.endsWith('.a')) {
        staticLibs.push(flag);
      } else if (flag) {
        ldFlags.push(flag);
      }
    }
  }

  // Extract cc-options: values
  const ccPattern = /cc-options:\s*"([^"]*)"/g;
  while ((match = ccPattern.exec(content)) !== null) {
    ccFlags.push(...match[1].trim().split(/\s+/).filter(Boolean));
  }

  // Extract pkg-config entries
  const pkgPattern = /pkg-config:\s*"([^"]*)"/g;
  while ((match = pkgPattern.exec(content)) !== null) {
    pkgConfigs.push(match[1].trim());
  }

  return { ldFlags, ccFlags, pkgConfigs, staticLibs };
}

/**
 * Check if a source file contains FFI markers.
 */
function hasFFIMarkers(content: string): boolean {
  for (const marker of FFI_MARKERS) {
    if (content.includes(marker)) return true;
  }
  return false;
}

/**
 * Extract ld-options from a module's source file c-declare blocks.
 * Looks for #include directives and linker pragmas.
 */
function extractCDeclareLibs(content: string): string[] {
  const libs: string[] = [];

  // Find #include directives in c-declare blocks to identify required libs
  const includePattern = /#include\s*[<"]([^>"]+)[>"]/g;
  let match;
  while ((match = includePattern.exec(content)) !== null) {
    libs.push(match[1]);
  }

  return libs;
}

/**
 * Trace transitive imports from a module via gxi.
 */
async function traceTransitiveImports(
  modulePath: string,
  env?: Record<string, string>,
): Promise<string[]> {
  const exprs = [
    '(import :gerbil/expander)',
    [
      '(let ((visited (make-hash-table)))',
      '  (let trace ((mod-sym (quote ' + modulePath + ')))',
      '    (unless (hash-get visited mod-sym)',
      '      (hash-put! visited mod-sym #t)',
      '      (with-catch',
      '        (lambda (e) #f)',
      '        (lambda ()',
      '          (let ((mod (import-module mod-sym #f #t)))',
      '            (display "IMPORT:") (displayln mod-sym)',
      '            (for-each',
      '              (lambda (dep)',
      '                (when (symbol? dep)',
      '                  (trace dep)))',
      '              (module-context-import mod))))))))',
    ].join(' '),
  ];

  const result = await runGxi(exprs, { env, timeout: 30_000 });
  const imports: string[] = [];
  for (const line of result.stdout.split('\n')) {
    const trimmed = line.trim();
    if (trimmed.startsWith('IMPORT:')) {
      imports.push(trimmed.slice('IMPORT:'.length).trim());
    }
  }
  return imports;
}

export function registerBuildLinkageDiagnosticTool(server: McpServer): void {
  server.registerTool(
    'gerbil_build_linkage_diagnostic',
    {
      title: 'Build Linkage Diagnostic',
      description:
        'Analyze exe targets in build.ss for missing transitive FFI link dependencies. ' +
        'Traces imports from each exe module, identifies which use FFI (c-lambda, c-declare, ' +
        'begin-foreign), and compares required link libraries against the exe\'s ld-options. ' +
        'Reports missing C libraries that could cause silent link failures or broken binaries.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        project_path: z
          .string()
          .describe('Path to the Gerbil project directory containing build.ss'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe('Additional GERBIL_LOADPATH entries for module resolution'),
        exe_target: z
          .string()
          .optional()
          .describe('Specific exe module name to check (default: check all exe targets)'),
      },
    },
    async ({ project_path, loadpath, exe_target }) => {
      // Read build.ss
      const buildSsPath = join(project_path, 'build.ss');
      let buildContent: string;
      try {
        buildContent = await readFile(buildSsPath, 'utf-8');
      } catch {
        return {
          content: [{
            type: 'text' as const,
            text: `No build.ss found in ${project_path}.`,
          }],
          isError: true,
        };
      }

      // Parse build targets
      const targets = parseBuildTargets(buildContent);
      if (targets.executables.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: 'No exe targets found in build.ss.',
          }],
        };
      }

      // Filter to specific exe if requested
      const exesToCheck = exe_target
        ? targets.executables.filter(e => e.module === exe_target)
        : targets.executables;

      if (exesToCheck.length === 0) {
        return {
          content: [{
            type: 'text' as const,
            text: `Exe target "${exe_target}" not found in build.ss. Available: ${targets.executables.map(e => e.module).join(', ')}`,
          }],
          isError: true,
        };
      }

      // Build env for gxi calls
      const env = loadpath ? buildLoadpathEnv(loadpath) : {};

      // Scan project files
      const projectFiles = await scanSchemeFiles(project_path);
      const fileContents = new Map<string, string>();
      for (const file of projectFiles) {
        try {
          fileContents.set(file, await readFile(file, 'utf-8'));
        } catch {
          // skip unreadable files
        }
      }

      // Extract global ld-options from build.ss
      const globalLdOpts = extractExeLdOptions(buildContent);

      const sections: string[] = [];
      let hasIssues = false;

      for (const exe of exesToCheck) {
        sections.push(`## Exe: ${exe.module} (bin: ${exe.binary})`);
        sections.push('');

        // Get the exe's own ld-options
        const exeLdOpts = extractExeLdOptions(buildContent, exe.module);
        const allProvidedLibs = new Set([
          ...exeLdOpts.ldFlags,
          ...exeLdOpts.staticLibs,
          ...exeLdOpts.pkgConfigs,
          ...globalLdOpts.ldFlags,
          ...globalLdOpts.staticLibs,
          ...globalLdOpts.pkgConfigs,
        ]);

        // Trace transitive imports
        let transImports: string[];
        try {
          transImports = await traceTransitiveImports(exe.module, env);
        } catch {
          sections.push('  Could not trace imports (module may not be loadable)');
          sections.push('');
          continue;
        }

        if (transImports.length === 0) {
          sections.push('  No transitive imports found.');
          sections.push('');
          continue;
        }

        // Find FFI modules among transitive imports
        const ffiModules: Array<{ module: string; file?: string; includes: string[] }> = [];

        for (const imp of transImports) {
          // Check if this import maps to a project file
          for (const [file, content] of fileContents) {
            const relPath = file.slice(project_path.length + 1);
            // Simple module path matching: ./module matches module.ss
            const moduleSuffix = imp.startsWith(':')
              ? imp.slice(1).replace(/\//g, '/') + '.ss'
              : imp.replace('./', '') + '.ss';

            if (relPath.endsWith(moduleSuffix) || relPath === moduleSuffix) {
              if (hasFFIMarkers(content)) {
                ffiModules.push({
                  module: imp,
                  file,
                  includes: extractCDeclareLibs(content),
                });
              }
              break;
            }
          }

          // Also check for FFI markers in any file that might correspond
          for (const [file, content] of fileContents) {
            const base = basename(file, '.ss');
            if (imp.endsWith(base) && hasFFIMarkers(content)) {
              const existing = ffiModules.find(m => m.module === imp);
              if (!existing) {
                ffiModules.push({
                  module: imp,
                  file,
                  includes: extractCDeclareLibs(content),
                });
              }
            }
          }
        }

        if (ffiModules.length === 0) {
          sections.push('  No FFI modules in transitive imports.');
          sections.push('');
          continue;
        }

        sections.push(`  FFI modules found: ${ffiModules.length}`);
        for (const ffi of ffiModules) {
          const fileLabel = ffi.file ? ` (${basename(ffi.file)})` : '';
          sections.push(`    - ${ffi.module}${fileLabel}`);
          if (ffi.includes.length > 0) {
            sections.push(`      includes: ${ffi.includes.join(', ')}`);
          }
        }

        // Check if FFI modules' own build targets have ld-options not in exe
        // Look for build.ss files in FFI module directories
        const missingDeps: Array<{ module: string; missingFlags: string[] }> = [];

        for (const ffi of ffiModules) {
          if (!ffi.file) continue;
          const ffiDir = dirname(ffi.file);
          const ffiBuildSs = join(ffiDir, 'build.ss');

          if (existsSync(ffiBuildSs) && ffiBuildSs !== buildSsPath) {
            try {
              const ffiBuildContent = await readFile(ffiBuildSs, 'utf-8');
              const ffiLdOpts = extractExeLdOptions(ffiBuildContent);
              const missing = [
                ...ffiLdOpts.ldFlags.filter(f => !allProvidedLibs.has(f)),
                ...ffiLdOpts.staticLibs.filter(f => !allProvidedLibs.has(f)),
                ...ffiLdOpts.pkgConfigs.filter(f => !allProvidedLibs.has(f)),
              ];
              if (missing.length > 0) {
                missingDeps.push({ module: ffi.module, missingFlags: missing });
              }
            } catch {
              // skip unreadable build.ss
            }
          }
        }

        sections.push('');
        if (missingDeps.length > 0) {
          hasIssues = true;
          sections.push('  MISSING LINK DEPENDENCIES:');
          for (const dep of missingDeps) {
            sections.push(`    ${dep.module}: ${dep.missingFlags.join(' ')}`);
          }
        } else {
          sections.push('  All transitive FFI link dependencies appear satisfied.');
        }
        sections.push('');

        // Report provided libs for reference
        if (allProvidedLibs.size > 0) {
          sections.push(`  Provided ld-options: ${[...allProvidedLibs].join(' ')}`);
          sections.push('');
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: hasIssues,
      };
    },
  );
}
