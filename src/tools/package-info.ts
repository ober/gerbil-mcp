import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { homedir } from 'node:os';
import { runGxpkg } from '../gxi.js';

export function registerPackageInfoTool(server: McpServer): void {
  server.registerTool(
    'gerbil_package_info',
    {
      title: 'Package Information',
      description:
        'List installed Gerbil packages, search the package directory, ' +
        'or show metadata for a specific package. Uses gxpkg for package management.',
      inputSchema: {
        action: z
          .enum(['list', 'search', 'info'])
          .describe(
            'Action: "list" installed packages, "search" the package directory, ' +
            'or "info" for package details',
          ),
        query: z
          .string()
          .optional()
          .describe(
            'Keywords for search, or package name for info (required for search and info)',
          ),
      },
    },
    async ({ action, query }) => {
      if ((action === 'search' || action === 'info') && !query) {
        return {
          content: [
            {
              type: 'text' as const,
              text: `The "query" parameter is required for the "${action}" action.`,
            },
          ],
          isError: true,
        };
      }

      switch (action) {
        case 'list':
          return handleList();
        case 'search':
          return handleSearch(query!);
        case 'info':
          return handleInfo(query!);
      }
    },
  );
}

async function handleList(): Promise<{
  content: Array<{ type: 'text'; text: string }>;
  isError?: boolean;
}> {
  const result = await runGxpkg(['list']);

  if (result.timedOut) {
    return {
      content: [{ type: 'text' as const, text: 'Package list timed out.' }],
      isError: true,
    };
  }

  if (result.exitCode === 127) {
    return {
      content: [
        {
          type: 'text' as const,
          text: 'gxpkg not found. Ensure Gerbil is installed and gxpkg is in PATH.',
        },
      ],
      isError: true,
    };
  }

  const output = [result.stdout, result.stderr]
    .filter(Boolean)
    .join('\n')
    .trim();

  if (result.exitCode !== 0) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Failed to list packages:\n${output}`,
        },
      ],
      isError: true,
    };
  }

  if (!output) {
    return {
      content: [
        { type: 'text' as const, text: 'No packages installed.' },
      ],
    };
  }

  const packages = output
    .split('\n')
    .map((l) => l.trim())
    .filter(Boolean);

  const sections: string[] = [
    `Installed packages (${packages.length}):`,
    '',
  ];
  for (const pkg of packages) {
    sections.push(`  ${pkg}`);
  }

  return {
    content: [{ type: 'text' as const, text: sections.join('\n') }],
  };
}

async function handleSearch(query: string): Promise<{
  content: Array<{ type: 'text'; text: string }>;
  isError?: boolean;
}> {
  const result = await runGxpkg(['search', query]);

  if (result.timedOut) {
    return {
      content: [{ type: 'text' as const, text: 'Package search timed out.' }],
      isError: true,
    };
  }

  if (result.exitCode === 127) {
    return {
      content: [
        {
          type: 'text' as const,
          text: 'gxpkg not found. Ensure Gerbil is installed and gxpkg is in PATH.',
        },
      ],
      isError: true,
    };
  }

  const output = [result.stdout, result.stderr]
    .filter(Boolean)
    .join('\n')
    .trim();

  if (result.exitCode !== 0) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Search failed:\n${output}`,
        },
      ],
      isError: true,
    };
  }

  if (!output) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `No packages found matching "${query}".`,
        },
      ],
    };
  }

  const sections: string[] = [
    `Search results for "${query}":`,
    '',
    output,
  ];

  return {
    content: [{ type: 'text' as const, text: sections.join('\n') }],
  };
}

async function handleInfo(query: string): Promise<{
  content: Array<{ type: 'text'; text: string }>;
  isError?: boolean;
}> {
  // Try reading the gerbil.pkg file directly
  const gerbilPath =
    process.env.GERBIL_PATH || join(homedir(), '.gerbil');
  const pkgFile = join(gerbilPath, 'pkg', query, 'gerbil.pkg');

  try {
    const contents = await readFile(pkgFile, 'utf-8');
    const sections: string[] = [
      `Package: ${query}`,
      `Location: ${join(gerbilPath, 'pkg', query)}`,
      '',
      'gerbil.pkg:',
      contents.trim(),
    ];

    return {
      content: [{ type: 'text' as const, text: sections.join('\n') }],
    };
  } catch {
    // Package not found locally — try gxpkg deps as fallback
    const result = await runGxpkg(['deps', query]);
    if (result.exitCode === 0 && result.stdout.trim()) {
      const sections: string[] = [
        `Package: ${query}`,
        '',
        'Dependencies:',
        result.stdout.trim(),
      ];
      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    }

    return {
      content: [
        {
          type: 'text' as const,
          text: `Package "${query}" not found. It may not be installed.`,
        },
      ],
      isError: true,
    };
  }
}
