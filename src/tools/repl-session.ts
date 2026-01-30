import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import {
  createReplSession,
  evalInSession,
  destroyReplSession,
  listReplSessions,
} from '../gxi.js';

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
      },
    },
    async ({ action, session_id, expression }) => {
      switch (action) {
        case 'create':
          return await handleCreate();
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

async function handleCreate() {
  const result = await createReplSession();

  if (result.error) {
    return {
      content: [{ type: 'text' as const, text: result.error }],
      isError: true,
    };
  }

  return {
    content: [
      {
        type: 'text' as const,
        text: `Session created: ${result.id}\n\nUse action "eval" with session_id "${result.id}" to evaluate expressions.`,
      },
    ],
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
