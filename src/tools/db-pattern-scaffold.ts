import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';

export function registerDbPatternScaffoldTool(server: McpServer): void {
  server.registerTool(
    'gerbil_db_pattern_scaffold',
    {
      title: 'Database Pattern Scaffold',
      description:
        'Generate database access patterns with connection pooling, transactions, and error ' +
        'handling. Supports SQLite and PostgreSQL via :std/db/dbi, :std/db/sqlite, ' +
        ':std/db/postgresql, and :std/db/conpool.',
      annotations: {
        readOnlyHint: true,
        idempotentHint: true,
      },
      inputSchema: {
        db_type: z
          .enum(['sqlite', 'postgresql'])
          .describe('Database type'),
        tables: z
          .array(
            z.object({
              name: z.string().describe('Table name'),
              columns: z.array(
                z.object({
                  name: z.string().describe('Column name'),
                  type: z.string().describe('SQL type (TEXT, INTEGER, etc.)'),
                  primary_key: z.boolean().optional(),
                }),
              ).describe('Column definitions'),
            }),
          )
          .describe('Table definitions'),
        use_pool: z
          .boolean()
          .optional()
          .describe('Include connection pooling (default: true)'),
      },
    },
    async ({ db_type, tables, use_pool }) => {
      const withPool = use_pool !== false;
      const sections: string[] = [];

      // Imports
      sections.push(`;;; Database access layer — ${db_type}`);
      sections.push('(import :std/db/dbi');
      if (db_type === 'sqlite') {
        sections.push('        :std/db/sqlite');
      } else {
        sections.push('        :std/db/postgresql');
      }
      if (withPool) {
        sections.push('        :std/db/conpool');
      }
      sections.push('        :std/sugar');
      sections.push('        :std/error)');
      sections.push('');

      // Exports
      sections.push('(export');
      sections.push('  connect!');
      sections.push('  disconnect!');
      sections.push('  with-transaction');
      for (const table of tables) {
        sections.push(`  create-${table.name}-table!`);
        sections.push(`  insert-${table.name}!`);
        sections.push(`  get-${table.name}`);
        sections.push(`  list-${table.name}s`);
        sections.push(`  update-${table.name}!`);
        sections.push(`  delete-${table.name}!`);
      }
      sections.push(')');
      sections.push('');

      // Connection management
      if (withPool) {
        sections.push(';;; Connection pool');
        sections.push('(def *pool* #f)');
        sections.push('');
        if (db_type === 'sqlite') {
          sections.push('(def (connect! db-path)');
          sections.push('  (set! *pool* (make-conpool');
          sections.push('    (lambda () (sql-connect sqlite-connect db-path))');
          sections.push('    pool-size: 5)))');
        } else {
          sections.push('(def (connect! host port db user password)');
          sections.push('  (set! *pool* (make-conpool');
          sections.push('    (lambda () (sql-connect postgresql-connect');
          sections.push('      host: host port: port db: db user: user password: password))');
          sections.push('    pool-size: 5)))');
        }
        sections.push('');
        sections.push('(def (disconnect!)');
        sections.push('  (when *pool*');
        sections.push('    (conpool-close *pool*)');
        sections.push('    (set! *pool* #f)))');
        sections.push('');
        sections.push('(def (get-conn)');
        sections.push('  (conpool-get *pool*))');
        sections.push('');
        sections.push('(def (return-conn conn)');
        sections.push('  (conpool-put *pool* conn))');
      } else {
        sections.push(';;; Single connection');
        sections.push('(def *conn* #f)');
        sections.push('');
        if (db_type === 'sqlite') {
          sections.push('(def (connect! db-path)');
          sections.push('  (set! *conn* (sql-connect sqlite-connect db-path)))');
        } else {
          sections.push('(def (connect! host port db user password)');
          sections.push('  (set! *conn* (sql-connect postgresql-connect');
          sections.push('    host: host port: port db: db user: user password: password)))');
        }
        sections.push('');
        sections.push('(def (disconnect!)');
        sections.push('  (when *conn*');
        sections.push('    (sql-close *conn*)');
        sections.push('    (set! *conn* #f)))');
      }
      sections.push('');

      // Transaction wrapper
      sections.push(';;; Transaction wrapper with rollback on error');
      if (withPool) {
        sections.push('(def (with-transaction thunk)');
        sections.push('  (let ((conn (get-conn)))');
        sections.push('    (unwind-protect');
        sections.push('      (begin');
        sections.push('        (sql-exec conn "BEGIN")');
        sections.push('        (with-catch');
        sections.push('          (lambda (e)');
        sections.push('            (sql-exec conn "ROLLBACK")');
        sections.push('            (raise e))');
        sections.push('          (lambda ()');
        sections.push('            (let ((result (thunk conn)))');
        sections.push('              (sql-exec conn "COMMIT")');
        sections.push('              result))))');
        sections.push('      (return-conn conn))))');
      } else {
        sections.push('(def (with-transaction thunk)');
        sections.push('  (sql-exec *conn* "BEGIN")');
        sections.push('  (with-catch');
        sections.push('    (lambda (e)');
        sections.push('      (sql-exec *conn* "ROLLBACK")');
        sections.push('      (raise e))');
        sections.push('    (lambda ()');
        sections.push('      (let ((result (thunk *conn*)))');
        sections.push('        (sql-exec *conn* "COMMIT")');
        sections.push('        result))))');
      }
      sections.push('');

      // CRUD for each table
      for (const table of tables) {
        const pk = table.columns.find((c) => c.primary_key) || table.columns[0];
        const nonPkCols = table.columns.filter((c) => c !== pk);

        sections.push(`;;; === ${table.name} ===`);
        sections.push('');

        // Create table
        const colDefs = table.columns.map((c) => {
          let def = `${c.name} ${c.type}`;
          if (c.primary_key) def += ' PRIMARY KEY';
          return def;
        }).join(', ');

        sections.push(`(def (create-${table.name}-table! conn)`);
        sections.push(`  (sql-exec conn "CREATE TABLE IF NOT EXISTS ${table.name} (${colDefs})"))`);
        sections.push('');

        // Insert
        const insertCols = nonPkCols.map((c) => c.name).join(', ');
        const insertPlaceholders = nonPkCols.map((_, i) => `?${i + 1}`).join(', ');
        const insertParams = nonPkCols.map((c) => c.name).join(' ');

        sections.push(`(def (insert-${table.name}! conn ${insertParams})`);
        if (pk.primary_key && pk.type.includes('INTEGER')) {
          sections.push(`  (sql-exec conn "INSERT INTO ${table.name} (${insertCols}) VALUES (${insertPlaceholders})" ${insertParams})`);
          sections.push(`  (sql-exec conn "SELECT last_insert_rowid()"))`);
        } else {
          const allCols = table.columns.map((c) => c.name).join(', ');
          const allPlaceholders = table.columns.map((_, i) => `?${i + 1}`).join(', ');
          const allParams = table.columns.map((c) => c.name).join(' ');
          sections.push(`  (sql-exec conn "INSERT INTO ${table.name} (${allCols}) VALUES (${allPlaceholders})" ${allParams}))`);
        }
        sections.push('');

        // Get by PK
        sections.push(`(def (get-${table.name} conn ${pk.name})`);
        sections.push(`  (let ((rows (sql-query conn "SELECT * FROM ${table.name} WHERE ${pk.name} = ?1" ${pk.name})))`);
        sections.push('    (if (null? rows) #f (car rows))))');
        sections.push('');

        // List all
        sections.push(`(def (list-${table.name}s conn)`);
        sections.push(`  (sql-query conn "SELECT * FROM ${table.name}"))`);
        sections.push('');

        // Update
        if (nonPkCols.length > 0) {
          const setClauses = nonPkCols.map((c, i) => `${c.name} = ?${i + 1}`).join(', ');
          const updateParams = nonPkCols.map((c) => c.name).join(' ');
          sections.push(`(def (update-${table.name}! conn ${pk.name} ${updateParams})`);
          sections.push(`  (sql-exec conn "UPDATE ${table.name} SET ${setClauses} WHERE ${pk.name} = ?${nonPkCols.length + 1}" ${updateParams} ${pk.name}))`);
        }
        sections.push('');

        // Delete
        sections.push(`(def (delete-${table.name}! conn ${pk.name})`);
        sections.push(`  (sql-exec conn "DELETE FROM ${table.name} WHERE ${pk.name} = ?1" ${pk.name}))`);
        sections.push('');
      }

      const code = sections.join('\n');

      const output = [
        `## Database Pattern Scaffold: ${db_type}`,
        '',
        `Tables: ${tables.length}`,
        `Connection pooling: ${withPool}`,
        '',
        '```scheme',
        code,
        '```',
        '',
        '### Usage',
        '```scheme',
        db_type === 'sqlite'
          ? '(connect! "my-database.db")'
          : '(connect! "localhost" 5432 "mydb" "user" "password")',
        '',
        '(with-transaction',
        '  (lambda (conn)',
        `    (create-${tables[0]?.name || 'example'}-table! conn)`,
        '    ;; ... more operations',
        '  ))',
        '',
        '(disconnect!)',
        '```',
        '',
        '**Note**: Verify DBI API with `gerbil_module_exports :std/db/dbi` — ',
        'the exact function names may vary between Gerbil versions.',
      ];

      return {
        content: [{ type: 'text' as const, text: output.join('\n') }],
      };
    },
  );
}
