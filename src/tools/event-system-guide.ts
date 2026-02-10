import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { runGxi } from '../gxi.js';

export function registerEventSystemGuideTool(server: McpServer): void {
  server.registerTool(
    'gerbil_event_system_guide',
    {
      title: 'Event System Guide',
      description:
        'Interactive guide for :std/event (sync, select, choice, wrap, handle, event sources). ' +
        'Shows available event combinators, demonstrates sync/select patterns with timeouts, ' +
        'and explains the relationship between events, channels, and actors.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        topic: z
          .enum(['overview', 'sync', 'select', 'timeout', 'channels', 'custom'])
          .optional()
          .describe('Specific topic to explore (default: overview)'),
      },
    },
    async ({ topic }) => {
      const selectedTopic = topic || 'overview';

      // Try to get actual module exports
      let exports = '';
      try {
        const result = await runGxi([
          '(import :gerbil/expander)',
          '(import :std/event)',
          '(display (map car (module-context-e (import-module (quote :std/event) #t #t))))',
        ], { timeout: 10000 });
        exports = result.stdout;
      } catch { /* fallback to static info */ }

      const sections: string[] = [
        '## Gerbil Event System Guide',
        '',
      ];

      if (exports) {
        sections.push(`### Available Exports from :std/event`);
        sections.push(`\`${exports}\``);
        sections.push('');
      }

      if (selectedTopic === 'overview' || selectedTopic === 'sync') {
        sections.push('### sync — Wait for a single event');
        sections.push('');
        sections.push('`sync` blocks until an event is ready, then returns its value.');
        sections.push('');
        sections.push('```scheme');
        sections.push('(import :std/event :std/actor)');
        sections.push('');
        sections.push(';; Wait for a channel message');
        sections.push('(def ch (make-channel))');
        sections.push('(spawn (lambda () (channel-put ch "hello")))');
        sections.push('(sync ch) ;; => "hello"');
        sections.push('```');
        sections.push('');
      }

      if (selectedTopic === 'overview' || selectedTopic === 'select') {
        sections.push('### select / choice — Wait for first of multiple events');
        sections.push('');
        sections.push('`choice` combines multiple events; `sync` returns the first ready.');
        sections.push('');
        sections.push('```scheme');
        sections.push('(import :std/event :std/actor)');
        sections.push('');
        sections.push(';; Wait for either channel message');
        sections.push('(def ch1 (make-channel))');
        sections.push('(def ch2 (make-channel))');
        sections.push('(sync (choice ch1 ch2)) ;; returns whichever is ready first');
        sections.push('```');
        sections.push('');
      }

      if (selectedTopic === 'overview' || selectedTopic === 'timeout') {
        sections.push('### Timeout patterns');
        sections.push('');
        sections.push('Use `make-timeout` or `alarm` for time-limited waits.');
        sections.push('');
        sections.push('```scheme');
        sections.push('(import :std/event :std/actor)');
        sections.push('');
        sections.push(';; Wait for message OR timeout');
        sections.push('(def ch (make-channel))');
        sections.push('(let ((result (sync (choice');
        sections.push('                     (wrap ch (lambda (msg) (list \'message msg)))');
        sections.push('                     (wrap (make-timeout 5) ;; 5 second timeout');
        sections.push('                           (lambda (_) \'timeout))))))');
        sections.push('  (match result');
        sections.push("    (['message msg] (displayln \"Got: \" msg))");
        sections.push("    ('timeout (displayln \"Timed out\"))))");
        sections.push('```');
        sections.push('');
      }

      if (selectedTopic === 'overview' || selectedTopic === 'channels') {
        sections.push('### Channels as events');
        sections.push('');
        sections.push('Channels are first-class events in Gerbil.');
        sections.push('');
        sections.push('```scheme');
        sections.push('(import :std/actor)');
        sections.push('');
        sections.push(';; Channels work directly with sync/choice');
        sections.push('(def ch (make-channel))');
        sections.push('(spawn (lambda () (channel-put ch 42)))');
        sections.push('(channel-get ch) ;; => 42');
        sections.push('');
        sections.push(';; Fan-out: one producer, multiple consumers');
        sections.push('(def producer-ch (make-channel))');
        sections.push('(for-each');
        sections.push('  (lambda (id)');
        sections.push('    (spawn (lambda ()');
        sections.push('      (let ((msg (channel-get producer-ch)))');
        sections.push('        (displayln "Worker " id " got: " msg)))))');
        sections.push("  '(1 2 3))");
        sections.push('```');
        sections.push('');
      }

      if (selectedTopic === 'overview' || selectedTopic === 'custom') {
        sections.push('### wrap / handle — Transform event values');
        sections.push('');
        sections.push('`wrap` transforms the value when an event fires.');
        sections.push('`handle` runs a side effect.');
        sections.push('');
        sections.push('```scheme');
        sections.push('(import :std/event :std/actor)');
        sections.push('');
        sections.push(';; Transform channel values');
        sections.push('(def ch (make-channel))');
        sections.push('(def doubled-ch (wrap ch (lambda (x) (* x 2))))');
        sections.push('(spawn (lambda () (channel-put ch 21)))');
        sections.push('(sync doubled-ch) ;; => 42');
        sections.push('```');
        sections.push('');
      }

      sections.push('### Key concepts');
      sections.push('- Events are first-class values that can be combined with `choice`');
      sections.push('- `sync` blocks until one event in a choice is ready');
      sections.push('- `wrap` transforms event values without side effects');
      sections.push('- `handle` runs side effects when an event fires');
      sections.push('- Channels, timeouts, and actor mailboxes are all event sources');
      sections.push('');
      sections.push('**Tip**: Use `gerbil_module_exports :std/event` for the complete API.');

      return {
        content: [{ type: 'text' as const, text: sections.join('\n') }],
      };
    },
  );
}
