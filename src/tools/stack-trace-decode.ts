import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';

interface DecodedFrame {
  frameNum: number;
  rawSymbol: string;
  demangled: string;
  module: string;
  isRuntime: boolean;
}

/**
 * Demangle a single Gambit C symbol name.
 */
function demangleSymbol(sym: string): { module: string; symbol: string } {
  let s = sym;

  // Strip known Gambit prefixes
  if (s.startsWith('___H_')) {
    s = s.slice(5);
    return { module: demanglePath(s), symbol: '(module init)' };
  }
  if (s.startsWith('___G_')) {
    s = s.slice(5);
  }

  // Decode hex escapes: _XX_ -> character
  s = s.replace(/_([0-9a-fA-F]{2})_/g, (_m, hex) => {
    return String.fromCharCode(parseInt(hex, 16));
  });

  // Replace double underscores with /
  s = s.replace(/__/g, '/');

  // Split module/symbol at last /
  const lastSlash = s.lastIndexOf('/');
  if (lastSlash > 0) {
    return {
      module: s.slice(0, lastSlash),
      symbol: s.slice(lastSlash + 1),
    };
  }

  return { module: '', symbol: s };
}

function demanglePath(s: string): string {
  s = s.replace(/_([0-9a-fA-F]{2})_/g, (_m, hex) => {
    return String.fromCharCode(parseInt(hex, 16));
  });
  return s.replace(/__/g, '/');
}

/**
 * Parse GDB backtrace format.
 */
function parseGdbBacktrace(text: string): DecodedFrame[] {
  const frames: DecodedFrame[] = [];
  const lines = text.split('\n');

  for (const line of lines) {
    // #N 0xADDR in SYMBOL (args) at file:line
    // #N 0xADDR in SYMBOL () from lib.so
    // #N SYMBOL (args) at file:line
    const match = line.match(
      /^#(\d+)\s+(?:0x[0-9a-fA-F]+\s+in\s+)?(\S+)\s*\(/,
    );
    if (match) {
      const frameNum = parseInt(match[1], 10);
      const rawSymbol = match[2];
      const { module, symbol } = demangleSymbol(rawSymbol);
      const isRuntime =
        rawSymbol.startsWith('___') ||
        rawSymbol.startsWith('_pthread') ||
        rawSymbol === '__start' ||
        rawSymbol.includes('gambit') ||
        module.startsWith('_');

      frames.push({
        frameNum,
        rawSymbol,
        demangled: module ? `${module}#${symbol}` : symbol,
        module: module || '(C runtime)',
        isRuntime,
      });
    }
  }

  return frames;
}

/**
 * Parse Gambit Scheme-level exception traces.
 */
function parseSchemeTrace(text: string): DecodedFrame[] {
  const frames: DecodedFrame[] = [];
  const lines = text.split('\n');
  let frameNum = 0;

  for (const line of lines) {
    const trimmed = line.trim();

    // Scheme-style trace: N  PROC  "file" line col
    const match = trimmed.match(
      /^(\d+)\s+(\S+)\s+"([^"]+)"(?:@(\d+))?/,
    );
    if (match) {
      frames.push({
        frameNum: frameNum++,
        rawSymbol: match[2],
        demangled: match[2],
        module: match[3],
        isRuntime: match[3].includes('_gambit') || match[3].includes('~~lib'),
      });
      continue;
    }

    // display-continuation-backtrace format: N  proc_name
    const simpleMatch = trimmed.match(/^(\d+)\s+([a-zA-Z_#:][^\s]*)/);
    if (simpleMatch) {
      const raw = simpleMatch[2];
      frames.push({
        frameNum: frameNum++,
        rawSymbol: raw,
        demangled: raw,
        module: '',
        isRuntime: raw.startsWith('##'),
      });
    }
  }

  return frames;
}

/**
 * Auto-detect trace format and parse.
 */
function decodeStackTrace(text: string): DecodedFrame[] {
  // Detect GDB format
  if (text.includes('#0 ') || text.includes('#1 ') || text.match(/#\d+\s+0x/)) {
    return parseGdbBacktrace(text);
  }

  // Detect Gambit mangled names
  if (text.includes('___H_') || text.includes('___G_')) {
    return parseGdbBacktrace(text);
  }

  // Default: try Scheme trace format
  return parseSchemeTrace(text);
}

export function registerStackTraceDecodeTool(server: McpServer): void {
  server.registerTool(
    'gerbil_stack_trace_decode',
    {
      title: 'Decode Stack Trace',
      description:
        'Parse and decode Gambit/Gerbil stack traces into readable function names. ' +
        'Handles GDB backtraces with ___H_/___G_ mangled names, Gambit exception traces, ' +
        'and display-continuation-backtrace output. Demanges C symbol names and identifies ' +
        'user code vs runtime frames.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        trace: z
          .string()
          .describe('The full stack trace text to decode'),
      },
    },
    async ({ trace }) => {
      const frames = decodeStackTrace(trace);

      if (frames.length === 0) {
        return {
          content: [
            {
              type: 'text' as const,
              text: 'Could not parse any stack frames from the input.\n\n' +
                'Supported formats:\n' +
                '- GDB backtrace (#N 0xADDR in SYMBOL)\n' +
                '- Gambit exception trace (N PROC "file")\n' +
                '- display-continuation-backtrace output',
            },
          ],
        };
      }

      const sections: string[] = [
        '## Decoded Stack Trace',
        '',
        `Frames decoded: ${frames.length}`,
        `User frames: ${frames.filter((f) => !f.isRuntime).length}`,
        '',
        'Frame | Demangled | Module | Type',
        '------|-----------|--------|-----',
      ];

      for (const frame of frames) {
        const typeTag = frame.isRuntime ? 'runtime' : '**user**';
        sections.push(
          `${frame.frameNum} | ${frame.demangled} | ${frame.module || '—'} | ${typeTag}`,
        );
      }

      // Show user frames summary
      const userFrames = frames.filter((f) => !f.isRuntime);
      if (userFrames.length > 0) {
        sections.push('');
        sections.push('### User Code Frames');
        for (const f of userFrames) {
          sections.push(`  ${f.frameNum}: ${f.demangled}${f.module ? ` (${f.module})` : ''}`);
        }
      }

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
