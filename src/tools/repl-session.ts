import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import {
  createReplSession,
  evalInSession,
  destroyReplSession,
  listReplSessions,
  buildLoadpathEnv,
} from '../gxi.js';
import { parseDefinitions } from './parse-utils.js';

export function registerReplSessionTool(server: McpServer): void {
  server.registerTool(
    'gerbil_repl_session',
    {
      title: 'REPL Session',
      description:
        'Manage persistent Gerbil REPL sessions. State (definitions, imports, variables) ' +
        'persists across evaluations within a session. Actions: ' +
        '"create" starts a new session, ' +
        '"eval" evaluates an expression in an existing session, ' +
        '"destroy" closes a session, ' +
        '"list" shows active sessions. ' +
        'Sessions auto-expire after 10 minutes of inactivity. Max 5 concurrent sessions.',
      inputSchema: {
        action: z
          .enum(['create', 'eval', 'destroy', 'list'])
          .describe('Action to perform'),
        session_id: z
          .string()
          .optional()
          .describe('Session ID (required for eval and destroy actions)'),
        expression: z
          .string()
          .optional()
          .describe('Gerbil expression to evaluate (required for eval action)'),
        loadpath: z
          .array(z.string())
          .optional()
          .describe(
            'Directories to add to GERBIL_LOADPATH (used with "create" action)',
          ),
        project_path: z
          .string()
          .optional()
          .describe(
            'Project directory for auto-configuring GERBIL_LOADPATH from .gerbil/lib (used with "create" action)',
          ),
        preload_file: z
          .string()
          .optional()
          .describe(
            'Path to a .ss file whose imports will be loaded into the session (used with "create" action). ' +
            'This lets you immediately use functions from the file\'s imported modules.',
          ),
        env: z
          .record(z.string())
          .optional()
          .describe(
            'Environment variables to pass to the REPL subprocess (used with "create" action). ' +
            'E.g. {"DYLD_LIBRARY_PATH": "/usr/local/lib"}',
          ),
      },
    },
    async ({ action, session_id, expression, loadpath, project_path, preload_file, env: extraEnv }) => {
      switch (action) {
        case 'create':
          return await handleCreate(loadpath, project_path, preload_file, extraEnv);
        case 'eval':
          return await handleEval(session_id, expression);
        case 'destroy':
          return await handleDestroy(session_id);
        case 'list':
          return handleList();
        default:
          return {
            content: [
              {
                type: 'text' as const,
                text: `Unknown action: ${action}`,
              },
            ],
            isError: true,
          };
      }
    },
  );
}

async function handleCreate(
  loadpath?: string[],
  projectPath?: string,
  preloadFile?: string,
  extraEnv?: Record<string, string>,
) {
  // Build loadpath from explicit array and/or project_path
  const effectiveLoadpath: string[] = [...(loadpath ?? [])];

  if (projectPath) {
    const gerbilLib = join(projectPath, '.gerbil', 'lib');
    effectiveLoadpath.push(gerbilLib);
  }

  const loadpathEnv =
    effectiveLoadpath.length > 0
      ? buildLoadpathEnv(effectiveLoadpath)
      : undefined;
  const env = { ...loadpathEnv, ...extraEnv };
  const hasEnv = Object.keys(env).length > 0;

  const result = await createReplSession(hasEnv ? { env } : undefined);

  if (result.error) {
    return {
      content: [{ type: 'text' as const, text: result.error }],
      isError: true,
    };
  }

  // Build informative response
  const parts: string[] = [
    `Session created: ${result.id}`,
    '',
    `Use action "eval" with session_id "${result.id}" to evaluate expressions.`,
  ];

  if (effectiveLoadpath.length > 0) {
    parts.push('');
    parts.push(`GERBIL_LOADPATH configured with:`);
    for (const p of effectiveLoadpath) {
      parts.push(`  ${p}`);
    }
  }

  if (projectPath) {
    // Try to read package name for informational purposes
    try {
      const pkgContent = await readFile(
        join(projectPath, 'gerbil.pkg'),
        'utf-8',
      );
      const pkgMatch = pkgContent.match(/\(package:\s+([^\s)]+)\)/);
      if (pkgMatch) {
        parts.push(`Project package: ${pkgMatch[1]}`);
      }
    } catch {
      // gerbil.pkg not found — fine, just skip
    }
  }

  // Preload imports from file
  if (preloadFile) {
    try {
      const fileContent = await readFile(preloadFile, 'utf-8');
      const analysis = parseDefinitions(fileContent);

      if (analysis.imports.length === 0) {
        parts.push('');
        parts.push(`Preload: ${preloadFile} has no import forms.`);
      } else {
        const loaded: string[] = [];
        const failed: string[] = [];

        for (const imp of analysis.imports) {
          const evalResult = await evalInSession(result.id, imp.raw);
          if (evalResult.error && !evalResult.output) {
            failed.push(`  ${imp.raw.trim()} -- ${evalResult.error}`);
          } else {
            loaded.push(`  ${imp.raw.trim()}`);
          }
        }

        parts.push('');
        parts.push(`Preloaded imports from ${preloadFile}:`);
        if (loaded.length > 0) {
          parts.push(`Loaded (${loaded.length}):`);
          for (const l of loaded) parts.push(l);
        }
        if (failed.length > 0) {
          parts.push(`Failed (${failed.length}):`);
          for (const f of failed) parts.push(f);
        }
      }
    } catch (err) {
      const msg = err instanceof Error ? err.message : 'Unknown error';
      parts.push('');
      parts.push(`Preload warning: could not read ${preloadFile}: ${msg}`);
    }
  }

  return {
    content: [{ type: 'text' as const, text: parts.join('\n') }],
  };
}

async function handleEval(
  sessionId: string | undefined,
  expression: string | undefined,
) {
  if (!sessionId) {
    return {
      content: [
        {
          type: 'text' as const,
          text: 'session_id is required for eval action.',
        },
      ],
      isError: true,
    };
  }

  if (!expression) {
    return {
      content: [
        {
          type: 'text' as const,
          text: 'expression is required for eval action.',
        },
      ],
      isError: true,
    };
  }

  const result = await evalInSession(sessionId, expression);

  if (result.error && !result.output) {
    return {
      content: [{ type: 'text' as const, text: result.error }],
      isError: true,
    };
  }

  const parts: string[] = [];
  if (result.output) parts.push(result.output);
  if (result.error) parts.push(`\nStderr:\n${result.error}`);

  return {
    content: [
      {
        type: 'text' as const,
        text: parts.join('') || '(void)',
      },
    ],
  };
}

async function handleDestroy(sessionId: string | undefined) {
  if (!sessionId) {
    return {
      content: [
        {
          type: 'text' as const,
          text: 'session_id is required for destroy action.',
        },
      ],
      isError: true,
    };
  }

  const destroyed = destroyReplSession(sessionId);
  if (!destroyed) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Session "${sessionId}" not found.`,
        },
      ],
      isError: true,
    };
  }

  return {
    content: [
      {
        type: 'text' as const,
        text: `Session "${sessionId}" destroyed.`,
      },
    ],
  };
}

function handleList() {
  const sessions = listReplSessions();

  if (sessions.length === 0) {
    return {
      content: [
        { type: 'text' as const, text: 'No active REPL sessions.' },
      ],
    };
  }

  const now = Date.now();
  const lines = sessions.map((s) => {
    const age = Math.round((now - s.createdAt) / 1000);
    const idle = Math.round((now - s.lastUsedAt) / 1000);
    return `  ${s.id}  (age: ${age}s, idle: ${idle}s)`;
  });

  const formatted = [
    `Active REPL sessions (${sessions.length}):`,
    '',
    ...lines,
  ].join('\n');

  return {
    content: [{ type: 'text' as const, text: formatted }],
  };
}
