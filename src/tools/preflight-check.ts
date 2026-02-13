import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { existsSync } from 'node:fs';
import { stat } from 'node:fs/promises';
import { execFile } from 'node:child_process';
import { join } from 'node:path';
import { z } from 'zod';

interface CheckResult {
  name: string;
  status: 'ok' | 'warn' | 'fail';
  message: string;
}

/**
 * Check if a command is available in PATH.
 */
function checkCommand(cmd: string): Promise<CheckResult> {
  return new Promise((resolve) => {
    execFile('which', [cmd], { timeout: 5000 }, (error, stdout) => {
      if (error || !stdout.trim()) {
        resolve({ name: cmd, status: 'fail', message: `${cmd} not found in PATH` });
      } else {
        resolve({ name: cmd, status: 'ok', message: `${cmd} found at ${stdout.trim()}` });
      }
    });
  });
}

/**
 * Get Gerbil version string.
 */
function getGerbilVersion(gxiPath: string): Promise<string> {
  return new Promise((resolve) => {
    execFile(gxiPath, ['-e', '(display (gerbil-version-string))'], { timeout: 10000 }, (error, stdout) => {
      if (error || !stdout.trim()) {
        resolve('unknown');
      } else {
        resolve(stdout.trim());
      }
    });
  });
}

export function registerPreflightCheckTool(server: McpServer): void {
  server.registerTool(
    'gerbil_preflight_check',
    {
      title: 'MCP Server Preflight Check',
      description:
        'Diagnostic tool that verifies MCP server prerequisites and Gerbil environment health. ' +
        'Checks: gxi/gxc/gerbil availability, Gerbil version, GERBIL_HOME and GERBIL_PATH, ' +
        'dist/ folder existence (for TypeScript MCP servers), and basic eval functionality. ' +
        'Use when tools are not working or to verify the environment.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        server_path: z
          .string()
          .optional()
          .describe('Path to an MCP server project to check (checks dist/, node_modules)'),
      },
    },
    async ({ server_path }) => {
      const checks: CheckResult[] = [];

      // Check Gerbil tools
      const [gxiCheck, gxcCheck, gerbilCheck] = await Promise.all([
        checkCommand('gxi'),
        checkCommand('gxc'),
        checkCommand('gerbil'),
      ]);
      checks.push(gxiCheck, gxcCheck, gerbilCheck);

      // Check Gerbil version
      if (gxiCheck.status === 'ok') {
        const gxiPath = gxiCheck.message.split(' at ')[1] || 'gxi';
        const version = await getGerbilVersion(gxiPath);
        checks.push({ name: 'gerbil-version', status: 'ok', message: `Gerbil ${version}` });

        // Test basic eval
        try {
          const evalResult = await new Promise<string>((resolve) => {
            execFile(gxiPath, ['-e', '(display (+ 1 2))'], { timeout: 10000 }, (error, stdout) => {
              if (error) resolve('fail');
              else resolve(stdout.trim());
            });
          });
          if (evalResult === '3') {
            checks.push({ name: 'basic-eval', status: 'ok', message: 'gxi eval works correctly' });
          } else {
            checks.push({ name: 'basic-eval', status: 'warn', message: `gxi eval returned unexpected: ${evalResult}` });
          }
        } catch {
          checks.push({ name: 'basic-eval', status: 'fail', message: 'gxi eval failed' });
        }
      }

      // Check GERBIL_HOME
      const gerbilHome = process.env.GERBIL_HOME;
      if (gerbilHome) {
        if (existsSync(gerbilHome)) {
          checks.push({ name: 'GERBIL_HOME', status: 'ok', message: gerbilHome });
        } else {
          checks.push({ name: 'GERBIL_HOME', status: 'fail', message: `${gerbilHome} does not exist` });
        }
      } else {
        // Check common locations
        const commonPaths = ['/opt/gerbil', join(process.env.HOME || '', '.gerbil')];
        const found = commonPaths.find(p => existsSync(p));
        if (found) {
          checks.push({ name: 'GERBIL_HOME', status: 'ok', message: `${found} (auto-detected)` });
        } else {
          checks.push({ name: 'GERBIL_HOME', status: 'warn', message: 'GERBIL_HOME not set' });
        }
      }

      // Check GERBIL_PATH
      const gerbilPath = process.env.GERBIL_PATH || join(process.env.HOME || '', '.gerbil');
      if (existsSync(gerbilPath)) {
        checks.push({ name: 'GERBIL_PATH', status: 'ok', message: gerbilPath });
      } else {
        checks.push({ name: 'GERBIL_PATH', status: 'warn', message: `${gerbilPath} does not exist` });
      }

      // Check MCP server project if provided
      if (server_path) {
        const distDir = join(server_path, 'dist');
        const nodeModules = join(server_path, 'node_modules');
        const pkgJson = join(server_path, 'package.json');

        if (existsSync(pkgJson)) {
          checks.push({ name: 'package.json', status: 'ok', message: 'found' });
        } else {
          checks.push({ name: 'package.json', status: 'fail', message: 'not found' });
        }

        if (existsSync(nodeModules)) {
          checks.push({ name: 'node_modules', status: 'ok', message: 'found' });
        } else {
          checks.push({ name: 'node_modules', status: 'fail', message: 'missing — run npm install' });
        }

        if (existsSync(distDir)) {
          const indexJs = join(distDir, 'index.js');
          if (existsSync(indexJs)) {
            const st = await stat(indexJs);
            checks.push({ name: 'dist/index.js', status: 'ok', message: `found (${Math.round(st.size / 1024)}KB)` });
          } else {
            checks.push({ name: 'dist/index.js', status: 'fail', message: 'missing — run npm run build' });
          }
        } else {
          checks.push({ name: 'dist/', status: 'fail', message: 'missing — run npm run build' });
        }
      }

      // Format output
      const sections: string[] = ['MCP Server Preflight Check:', ''];
      let hasFailures = false;

      for (const check of checks) {
        const icon = check.status === 'ok' ? 'OK' : check.status === 'warn' ? 'WARN' : 'FAIL';
        if (check.status === 'fail') hasFailures = true;
        sections.push(`  [${icon}] ${check.name}: ${check.message}`);
      }

      sections.push('');
      if (hasFailures) {
        sections.push('Some checks failed. Fix the issues above and try again.');
      } else {
        sections.push('All checks passed. Environment is healthy.');
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
        isError: hasFailures,
      };
    },
  );
}
