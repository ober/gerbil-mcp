import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';

export function registerActorEnsembleScaffoldTool(server: McpServer): void {
  server.registerTool(
    'gerbil_actor_ensemble_scaffold',
    {
      title: 'Actor Ensemble Scaffold',
      description:
        'Generate a distributed actor ensemble project template using gxensemble. ' +
        'Produces actor protocol definitions, message types, supervisor tree, and ' +
        'a working example with communicating actors.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        project_name: z
          .string()
          .describe('Project name (kebab-case)'),
        actors: z
          .array(
            z.object({
              name: z.string().describe('Actor name'),
              messages: z.array(z.string()).describe('Message types this actor handles'),
            }),
          )
          .describe('Actor definitions with their message handlers'),
        use_tls: z
          .boolean()
          .optional()
          .describe('Include TLS configuration (default: false)'),
      },
    },
    async ({ project_name, actors, use_tls }) => {
      const files: Array<{ name: string; content: string }> = [];

      // gerbil.pkg
      files.push({
        name: 'gerbil.pkg',
        content: `(package: ${project_name})`,
      });

      // Protocol definitions
      const protoLines: string[] = [
        `;;; ${project_name} — Actor protocol definitions`,
        '(import :std/actor/message',
        '        :std/actor/proto)',
        '',
        `(export`,
      ];

      for (const actor of actors) {
        for (const msg of actor.messages) {
          protoLines.push(`  ${msg}-message`);
        }
      }
      protoLines.push(')');
      protoLines.push('');

      for (const actor of actors) {
        protoLines.push(`;;; ${actor.name} messages`);
        for (const msg of actor.messages) {
          protoLines.push(`(defmessage (${msg}-message)`);
          protoLines.push(`  id: data:)`);
        }
        protoLines.push('');
      }

      files.push({ name: 'proto.ss', content: protoLines.join('\n') });

      // Actor implementations
      for (const actor of actors) {
        const actorLines: string[] = [
          `;;; ${actor.name} actor implementation`,
          `(import :std/actor`,
          `        :std/sugar`,
          `        :std/logger`,
          `        ./${project_name}/proto)`,
          '',
          `(export ${actor.name}-start)`,
          '',
          `(def (${actor.name}-start)`,
          `  (spawn/name '${actor.name}`,
          `    (lambda ()`,
          `      (infof "${actor.name} started")`,
          `      (let loop ()`,
          `        (<- (,evt`,
          `          (match evt`,
        ];

        for (const msg of actor.messages) {
          actorLines.push(`            ((${msg}-message id: id data: data)`);
          actorLines.push(`             (infof "${actor.name}: handling ${msg} ~a" id)`);
          actorLines.push(`             ;; TODO: Process ${msg}`);
          actorLines.push(`             (void))`);
        }

        actorLines.push('            (else');
        actorLines.push(`             (warnf "${actor.name}: unknown message ~a" evt))))`);
        actorLines.push('        (loop)))))');

        files.push({ name: `${actor.name}.ss`, content: actorLines.join('\n') });
      }

      // Supervisor
      const supLines: string[] = [
        `;;; ${project_name} supervisor`,
        '(import :std/actor',
        '        :std/sugar',
        '        :std/logger',
      ];
      for (const actor of actors) {
        supLines.push(`        ./${project_name}/${actor.name}`);
      }
      supLines.push(')');
      supLines.push('');
      supLines.push(`(export ${project_name}-supervisor)`);
      supLines.push('');
      supLines.push(`(def (${project_name}-supervisor)`);
      supLines.push('  (infof "Starting supervisor")');
      supLines.push('  (let ((children');
      supLines.push('         (list');
      for (const actor of actors) {
        supLines.push(`          (${actor.name}-start)`);
      }
      supLines.push('         )))');
      supLines.push('    ;; Monitor children');
      supLines.push('    (for-each thread-join! children)))');

      files.push({ name: 'supervisor.ss', content: supLines.join('\n') });

      // Main entry point
      const mainLines: string[] = [
        `;;; ${project_name} — main entry point`,
        '(import :std/actor',
        '        :std/sugar',
        '        :std/logger',
        `        ./${project_name}/supervisor)`,
        '',
        '(export main)',
        '',
        '(def (main . args)',
        `  (start-logger! (current-logger))`,
        `  (infof "${project_name} starting")`,
      ];

      if (use_tls) {
        mainLines.push('  ;; TLS configuration');
        mainLines.push('  ;; (current-actor-server-tls-context');
        mainLines.push('  ;;   (make-tls-context key: "server.key" cert: "server.crt"))');
      }

      mainLines.push(`  (${project_name}-supervisor))`);

      files.push({ name: 'main.ss', content: mainLines.join('\n') });

      // Build file
      const buildLines = [
        '(import :std/build-script)',
        '',
        '(defbuild-script',
        "  '(",
        `    "proto"`,
      ];
      for (const actor of actors) {
        buildLines.push(`    "${actor.name}"`);
      }
      buildLines.push('    "supervisor"');
      buildLines.push(`    (exe: "main" bin: "${project_name}")`);
      buildLines.push('  ))');

      files.push({ name: 'build.ss', content: buildLines.join('\n') });

      // Format output
      const output: string[] = [
        `## Actor Ensemble Scaffold: ${project_name}`,
        '',
        `Actors: ${actors.length}`,
        `TLS: ${use_tls ? 'enabled' : 'disabled'}`,
        '',
      ];

      for (const file of files) {
        output.push(`### ${file.name}`);
        output.push('```scheme');
        output.push(file.content);
        output.push('```');
        output.push('');
      }

      output.push('### Setup');
      output.push('```sh');
      output.push(`mkdir -p ${project_name}`);
      output.push('# Save each file above into the project directory');
      output.push('gerbil build');
      output.push(`.gerbil/bin/${project_name}`);
      output.push('```');
      output.push('');
      output.push('**Note**: The gxensemble API is largely undocumented. Verify imports with');
      output.push('`gerbil_module_exports` and test incrementally with `gerbil_eval`.');

      return {
        content: [{ type: 'text' as const, text: output.join('\n') }],
      };
    },
  );
}
