import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';

export function registerGracefulShutdownScaffoldTool(server: McpServer): void {
  server.registerTool(
    'gerbil_graceful_shutdown_scaffold',
    {
      title: 'Graceful Shutdown Scaffold',
      description:
        'Generate graceful shutdown patterns for long-running Gerbil services. ' +
        'Produces signal handler registration, shutdown coordination across threads/actors, ' +
        'resource cleanup with unwind-protect, and proper exit codes.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        service_name: z
          .string()
          .describe('Name for the service'),
        components: z
          .array(z.string())
          .describe('Service components that need cleanup (e.g. "http-server", "db-pool", "worker-threads")'),
        has_actors: z
          .boolean()
          .optional()
          .describe('Include actor system shutdown (default: false)'),
      },
    },
    async ({ service_name, components, has_actors }) => {
      const sections: string[] = [];

      sections.push(`;;; ${service_name} — Graceful shutdown framework`);
      sections.push('(import :std/sugar');
      sections.push('        :std/logger');
      sections.push('        :std/os/signal-handler');
      sections.push('        :std/iter');
      sections.push('        :std/misc/sync)');
      if (has_actors) {
        sections.push('(import :std/actor)');
      }
      sections.push('');

      sections.push('(export');
      sections.push('  start-service!');
      sections.push('  shutdown!');
      sections.push('  with-shutdown-hook)');
      sections.push('');

      // Shutdown state
      sections.push(';;; Shutdown coordination');
      sections.push('(def *shutdown-requested* #f)');
      sections.push('(def *shutdown-mutex* (make-mutex))');
      sections.push('(def *shutdown-condvar* (make-condition-variable))');
      sections.push('(def *cleanup-hooks* [])');
      sections.push('');

      // Register cleanup hook
      sections.push(';;; Register a cleanup function to run during shutdown');
      sections.push('(def (register-cleanup! name thunk)');
      sections.push('  (mutex-lock! *shutdown-mutex*)');
      sections.push('  (set! *cleanup-hooks*');
      sections.push('    (cons (cons name thunk) *cleanup-hooks*))');
      sections.push('  (mutex-unlock! *shutdown-mutex*))');
      sections.push('');

      // Convenience macro
      sections.push(';;; Convenience: register a component with automatic cleanup');
      sections.push('(def (with-shutdown-hook name start-fn cleanup-fn)');
      sections.push('  (let ((result (start-fn)))');
      sections.push('    (register-cleanup! name (lambda () (cleanup-fn result)))');
      sections.push('    result))');
      sections.push('');

      // Check if shutdown requested
      sections.push(';;; Check if shutdown has been requested');
      sections.push('(def (shutdown-requested?)');
      sections.push('  *shutdown-requested*)');
      sections.push('');

      // Shutdown function
      sections.push(';;; Execute shutdown sequence');
      sections.push('(def (shutdown! (exit-code 0))');
      sections.push('  (mutex-lock! *shutdown-mutex*)');
      sections.push('  (when (not *shutdown-requested*)');
      sections.push('    (set! *shutdown-requested* #t)');
      sections.push('    (mutex-unlock! *shutdown-mutex*)');
      sections.push('');
      sections.push(`    (infof "${service_name}: Initiating graceful shutdown...")`);
      sections.push('');

      // Shutdown each component
      for (let i = 0; i < components.length; i++) {
        const comp = components[i];
        sections.push(`    ;; Stop ${comp}`);
        sections.push(`    (infof "Stopping ${comp}...")`);
      }
      sections.push('');

      sections.push('    ;; Run registered cleanup hooks in reverse order');
      sections.push('    (for ((hook *cleanup-hooks*))');
      sections.push('      (let ((name (car hook))');
      sections.push('            (cleanup (cdr hook)))');
      sections.push('        (infof "Cleanup: ~a" name)');
      sections.push('        (with-catch');
      sections.push('          (lambda (e) (errorf "Cleanup failed for ~a: ~a" name e))');
      sections.push('          cleanup)))');
      sections.push('');

      if (has_actors) {
        sections.push('    ;; Shutdown actor system');
        sections.push('    (infof "Stopping actor system...")');
        sections.push('    ;; (actor-system-shutdown!)');
        sections.push('');
      }

      sections.push(`    (infof "${service_name}: Shutdown complete.")`);
      sections.push('    (exit exit-code))');
      sections.push('');
      sections.push('  ;; If already shutting down, just release mutex');
      sections.push('  (mutex-unlock! *shutdown-mutex*))');
      sections.push('');

      // Signal handlers
      sections.push(';;; Install signal handlers');
      sections.push('(def (install-signal-handlers!)');
      sections.push('  ;; SIGTERM — graceful shutdown (e.g. from systemd, docker)');
      sections.push('  (add-signal-handler! SIGTERM');
      sections.push('    (lambda (sig)');
      sections.push(`      (infof "${service_name}: Received SIGTERM")`);
      sections.push('      (shutdown!)))');
      sections.push('');
      sections.push('  ;; SIGINT — Ctrl+C');
      sections.push('  (add-signal-handler! SIGINT');
      sections.push('    (lambda (sig)');
      sections.push(`      (infof "${service_name}: Received SIGINT")`);
      sections.push('      (shutdown!)))');
      sections.push('');
      sections.push('  ;; SIGHUP — reload config (optional)');
      sections.push('  (add-signal-handler! SIGHUP');
      sections.push('    (lambda (sig)');
      sections.push(`      (infof "${service_name}: Received SIGHUP — reload not implemented")`);
      sections.push('      )))');
      sections.push('');

      // Main service starter
      sections.push(';;; Start service with signal handlers and shutdown coordination');
      sections.push('(def (start-service! main-fn)');
      sections.push('  (start-logger! (current-logger))');
      sections.push(`  (infof "${service_name}: Starting...")`);
      sections.push('');
      sections.push('  ;; Install signal handlers');
      sections.push('  (install-signal-handlers!)');
      sections.push('');
      sections.push('  ;; Run main function with cleanup guarantee');
      sections.push('  (unwind-protect');
      sections.push('    (with-catch');
      sections.push('      (lambda (e)');
      sections.push(`        (errorf "${service_name}: Fatal error: ~a" e)`);
      sections.push('        (shutdown! 1))');
      sections.push('      main-fn)');
      sections.push('    (shutdown! 0)))');
      sections.push('');

      // Example usage
      sections.push(`;;; Example usage for ${service_name}:`);
      sections.push(';;;');
      sections.push(';;; (start-service!');
      sections.push(';;;   (lambda ()');

      for (const comp of components) {
        sections.push(`;;;     (with-shutdown-hook "${comp}"`);
        sections.push(`;;;       (lambda () (start-${comp.replace(/[^a-zA-Z0-9]/g, '-')}!))`);
        sections.push(`;;;       (lambda (instance) (stop-${comp.replace(/[^a-zA-Z0-9]/g, '-')}! instance)))`);
      }

      sections.push(';;;');
      sections.push(';;;     ;; Block until shutdown');
      sections.push(';;;     (mutex-lock! *shutdown-mutex*)');
      sections.push(';;;     (let loop ()');
      sections.push(';;;       (unless *shutdown-requested*');
      sections.push(';;;         (mutex-unlock! *shutdown-mutex*)');
      sections.push(';;;         (condition-variable-wait! *shutdown-condvar* *shutdown-mutex*)');
      sections.push(';;;         (loop)))');
      sections.push(';;;     (mutex-unlock! *shutdown-mutex*)))');

      const code = sections.join('\n');

      const output = [
        `## Graceful Shutdown Scaffold: ${service_name}`,
        '',
        `Components: ${components.join(', ')}`,
        `Actor system: ${has_actors ? 'yes' : 'no'}`,
        '',
        '```scheme',
        code,
        '```',
        '',
        '### Features',
        '- SIGTERM/SIGINT/SIGHUP signal handling',
        '- Registered cleanup hooks (run in reverse order)',
        '- Thread-safe shutdown coordination with mutex/condvar',
        '- unwind-protect for guaranteed cleanup',
        '- Error-tolerant cleanup (catches and logs failures)',
        '',
        '**Note**: Verify signal handler API with `gerbil_module_exports :std/os/signal-handler`.',
      ];

      return {
        content: [{ type: 'text' as const, text: output.join('\n') }],
      };
    },
  );
}
