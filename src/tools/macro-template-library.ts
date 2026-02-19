import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';

interface MacroTemplate {
  definition: string;
  examples: string[];
  expansions?: string[];
  description: string;
  notes?: string;
}

/**
 * Generate hash accessor macro template.
 */
function generateHashAccessors(
  prefix: string,
  fields: string[],
): MacroTemplate {
  const macroName = `def-${prefix}-getter`;

  const definition = `(defrule (${macroName} field)
  (def (${prefix}-field obj)
    (hash-ref obj 'field)))`;

  const examples = fields.map((field) => `(${macroName} ${field})`);
  const expansions = fields.map(
    (field) => `(def (${prefix}-${field} obj)\n  (hash-ref obj '${field}))`,
  );

  return {
    definition,
    examples,
    expansions,
    description: `Macro to generate hash-ref accessor functions for ${prefix} objects`,
    notes: `Generates ${fields.length} accessor functions: ${fields.map((f) => `${prefix}-${f}`).join(', ')}`,
  };
}

/**
 * Generate method delegation macro template.
 */
function generateMethodDelegation(
  delegateField: string,
  methods: string[],
): MacroTemplate {
  const macroName = 'def-delegate';

  const definition = `(defrule (${macroName} method-name)
  (def (method-name obj . args)
    (apply {method-name (${delegateField} obj)} args)))`;

  const examples = methods.map((method) => `(${macroName} ${method})`);

  return {
    definition,
    examples,
    description: `Macro to generate method delegation wrappers that forward to ${delegateField}`,
    notes: `Delegates ${methods.length} methods: ${methods.join(', ')}`,
  };
}

/**
 * Generate validation guard macro template.
 */
function generateValidationGuards(
  checks: Array<{ name: string; predicate: string }>,
): MacroTemplate {
  const macroName = 'def-validator';

  const definition = `(defrule (${macroName} name pred)
  (def (validate-name value)
    (unless (pred value)
      (error "validation failed" 'name value))
    value))`;

  const examples = checks.map(
    (check) => `(${macroName} ${check.name} ${check.predicate})`,
  );

  return {
    definition,
    examples,
    description: 'Macro to generate validation functions with automatic error messages',
    notes: `Generates ${checks.length} validators: ${checks.map((c) => `validate-${c.name}`).join(', ')}`,
  };
}

/**
 * Generate enum constants macro template.
 */
function generateEnumConstants(
  enumName: string,
  values: string[],
): MacroTemplate {
  const macroName = `def-${enumName}-enum`;

  const definition = `(defrule (${macroName} (name value) ...)
  (begin
    (def name value) ...))`;

  const valuesList = values
    .map((v, idx) => `(${v} ${idx})`)
    .join(' ');
  const examples = [`(${macroName} ${valuesList})`];

  return {
    definition,
    examples,
    description: `Macro to define ${enumName} enumeration constants`,
    notes: `Generates ${values.length} constants with sequential integer values`,
  };
}

/**
 * Generate event handler macro template.
 */
function generateEventHandlers(
  eventType: string,
  handlers: string[],
): MacroTemplate {
  const macroName = 'def-event-handler';

  const definition = `(defrule (${macroName} event-name body ...)
  (def (handle-event-name event)
    (when (eq? (event-type event) 'event-name)
      body ...)))`;

  const examples = handlers.map(
    (handler) =>
      `(${macroName} ${handler}\n  (displayln "Handling ${handler} event" event))`,
  );

  return {
    definition,
    examples,
    description: `Macro to generate event handlers for ${eventType} events`,
    notes: `Generates ${handlers.length} handlers: ${handlers.map((h) => `handle-${h}`).join(', ')}`,
  };
}

/**
 * Generate type-safe setter macro template.
 */
function generateTypeSetters(
  prefix: string,
  fields: Array<{ name: string; type: string }>,
): MacroTemplate {
  const macroName = `def-${prefix}-setter`;

  const definition = `(defrule (${macroName} field type-pred)
  (def (set-${prefix}-field! obj value)
    (unless (type-pred value)
      (error "type check failed" 'field value))
    (hash-put! obj 'field value)))`;

  const examples = fields.map(
    (field) => `(${macroName} ${field.name} ${field.type})`,
  );

  const expansions = fields.map(
    (field) =>
      `(def (set-${prefix}-${field.name}! obj value)\n  (unless (${field.type} value)\n    (error "type check failed" '${field.name} value))\n  (hash-put! obj '${field.name} value))`,
  );

  return {
    definition,
    examples,
    expansions,
    description: `Macro to generate type-checked setters for ${prefix} objects`,
    notes: `Generates ${fields.length} setters with runtime type validation`,
  };
}

export function registerMacroTemplateLibraryTool(server: McpServer): void {
  server.registerTool(
    'gerbil_macro_template_library',
    {
      title: 'Macro Template Library',
      description:
        'Generate reusable macro templates for common patterns. Given a pattern name and ' +
        'customization parameters, generates a tested, working defrule/defsyntax definition ' +
        'with example invocations. Supports: (1) hash-accessors - generate hash-ref getters, ' +
        '(2) method-delegation - forward methods to a delegate field, (3) validation-guards - ' +
        'generate validators with error messages, (4) enum-constants - define enumeration values, ' +
        '(5) event-handlers - generate event dispatch handlers, (6) type-setters - generate ' +
        'type-checked hash-put! setters. Returns the macro definition and usage examples ready ' +
        'to include in your project.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        pattern: z
          .enum([
            'hash-accessors',
            'method-delegation',
            'validation-guards',
            'enum-constants',
            'event-handlers',
            'type-setters',
          ])
          .describe('The macro pattern to generate'),
        prefix: z
          .string()
          .optional()
          .describe(
            'Prefix for generated functions (required for hash-accessors, type-setters)',
          ),
        fields: z
          .array(z.string())
          .optional()
          .describe(
            'List of field names (required for hash-accessors, enum-constants values)',
          ),
        delegate_field: z
          .string()
          .optional()
          .describe(
            'Name of the delegate field (required for method-delegation)',
          ),
        methods: z
          .array(z.string())
          .optional()
          .describe(
            'List of method names (required for method-delegation, event-handlers)',
          ),
        checks: z
          .array(
            z.object({
              name: z.string(),
              predicate: z.string(),
            }),
          )
          .optional()
          .describe(
            'List of {name, predicate} objects (required for validation-guards)',
          ),
        enum_name: z
          .string()
          .optional()
          .describe('Name of the enumeration (required for enum-constants)'),
        event_type: z
          .string()
          .optional()
          .describe('Type of event (required for event-handlers)'),
        typed_fields: z
          .array(
            z.object({
              name: z.string(),
              type: z.string(),
            }),
          )
          .optional()
          .describe(
            'List of {name, type} objects where type is a predicate (required for type-setters)',
          ),
      },
    },
    async ({
      pattern,
      prefix,
      fields,
      delegate_field,
      methods,
      checks,
      enum_name,
      event_type,
      typed_fields,
    }) => {
      let template: MacroTemplate;

      try {
        switch (pattern) {
          case 'hash-accessors':
            if (!prefix || !fields || fields.length === 0) {
              return {
                content: [
                  {
                    type: 'text' as const,
                    text: 'hash-accessors pattern requires: prefix (string) and fields (non-empty array)',
                  },
                ],
                isError: true,
              };
            }
            template = generateHashAccessors(prefix, fields);
            break;

          case 'method-delegation':
            if (!delegate_field || !methods || methods.length === 0) {
              return {
                content: [
                  {
                    type: 'text' as const,
                    text: 'method-delegation pattern requires: delegate_field (string) and methods (non-empty array)',
                  },
                ],
                isError: true,
              };
            }
            template = generateMethodDelegation(delegate_field, methods);
            break;

          case 'validation-guards':
            if (!checks || checks.length === 0) {
              return {
                content: [
                  {
                    type: 'text' as const,
                    text: 'validation-guards pattern requires: checks (non-empty array of {name, predicate})',
                  },
                ],
                isError: true,
              };
            }
            template = generateValidationGuards(checks);
            break;

          case 'enum-constants':
            if (!enum_name || !fields || fields.length === 0) {
              return {
                content: [
                  {
                    type: 'text' as const,
                    text: 'enum-constants pattern requires: enum_name (string) and fields (non-empty array of value names)',
                  },
                ],
                isError: true,
              };
            }
            template = generateEnumConstants(enum_name, fields);
            break;

          case 'event-handlers':
            if (!event_type || !methods || methods.length === 0) {
              return {
                content: [
                  {
                    type: 'text' as const,
                    text: 'event-handlers pattern requires: event_type (string) and methods (non-empty array of handler names)',
                  },
                ],
                isError: true,
              };
            }
            template = generateEventHandlers(event_type, methods);
            break;

          case 'type-setters':
            if (!prefix || !typed_fields || typed_fields.length === 0) {
              return {
                content: [
                  {
                    type: 'text' as const,
                    text: 'type-setters pattern requires: prefix (string) and typed_fields (non-empty array of {name, type})',
                  },
                ],
                isError: true,
              };
            }
            template = generateTypeSetters(prefix, typed_fields);
            break;

          default:
            return {
              content: [
                {
                  type: 'text' as const,
                  text: `Unknown pattern: ${pattern}`,
                },
              ],
              isError: true,
            };
        }

        // Format output
        const sections: string[] = [
          `Macro Template: ${pattern}`,
          ``,
          template.description,
          ``,
          `Macro Definition:`,
          ``,
          template.definition,
          ``,
          `Usage Examples:`,
          ``,
        ];

        template.examples.forEach((example, idx) => {
          sections.push(`${idx + 1}. ${example}`);
        });

        if (template.expansions) {
          sections.push(``);
          sections.push(`Example Expansions:`);
          sections.push(``);
          template.expansions.forEach((expansion, idx) => {
            sections.push(`${idx + 1}. ${expansion}`);
          });
        }

        if (template.notes) {
          sections.push(``);
          sections.push(`Notes:`);
          sections.push(template.notes);
        }

        sections.push(``);
        sections.push(`To use:`);
        sections.push(
          `1. Copy the macro definition into your project module`,
        );
        sections.push(`2. Use the example invocations to generate your code`);
        sections.push(
          `3. Customize the macro name and body as needed for your project`,
        );

        return {
          content: [{ type: 'text' as const, text: sections.join('\n') }],
        };
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'Unknown error';
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to generate macro template: ${msg}`,
            },
          ],
          isError: true,
        };
      }
    },
  );
}
