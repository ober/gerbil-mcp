import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';

// ── C type → Gambit FFI type mapping ───────────────────────────────────

const C_TYPE_MAP: Record<string, string> = {
  void: 'void',
  int: 'int',
  'unsigned int': 'unsigned-int',
  'unsigned': 'unsigned-int',
  long: 'long',
  'unsigned long': 'unsigned-long',
  'long long': 'long-long',
  'unsigned long long': 'unsigned-long-long',
  short: 'short',
  'unsigned short': 'unsigned-short',
  float: 'float',
  double: 'double',
  char: 'char',
  'unsigned char': 'unsigned-char',
  'signed char': 'signed-char',
  size_t: 'size_t',
  ssize_t: 'ssize_t',
  int8_t: 'int8',
  uint8_t: 'unsigned-int8',
  int16_t: 'int16',
  uint16_t: 'unsigned-int16',
  int32_t: 'int32',
  uint32_t: 'unsigned-int32',
  int64_t: 'int64',
  uint64_t: 'unsigned-int64',
  bool: 'bool',
  _Bool: 'bool',
};

// ── Parsed C declarations ──────────────────────────────────────────────

interface CTypedef {
  name: string;
  isPointer: boolean;
  baseType: string;
}

interface CFunction {
  name: string;
  returnType: string;
  params: Array<{ type: string; name: string }>;
}

interface CConstant {
  name: string;
  value: string;
}

interface CEnum {
  name: string | null;
  members: Array<{ name: string; value?: string }>;
}

interface ParsedHeader {
  typedefs: CTypedef[];
  functions: CFunction[];
  constants: CConstant[];
  enums: CEnum[];
}

// ── C header parser ────────────────────────────────────────────────────

/**
 * Strip C comments (both // and /* ... *​/) and preprocessor conditionals
 * that are not #define.
 */
function stripComments(source: string): string {
  let result = '';
  let i = 0;
  while (i < source.length) {
    // Line comment
    if (source[i] === '/' && source[i + 1] === '/') {
      while (i < source.length && source[i] !== '\n') i++;
      continue;
    }
    // Block comment
    if (source[i] === '/' && source[i + 1] === '*') {
      i += 2;
      while (i < source.length && !(source[i] === '*' && source[i + 1] === '/')) i++;
      i += 2; // skip */
      result += ' ';
      continue;
    }
    // String literal
    if (source[i] === '"') {
      result += source[i++];
      while (i < source.length && source[i] !== '"') {
        if (source[i] === '\\') {
          result += source[i++];
        }
        if (i < source.length) result += source[i++];
      }
      if (i < source.length) result += source[i++]; // closing "
      continue;
    }
    result += source[i++];
  }
  return result;
}

/**
 * Normalize a C type string by collapsing whitespace and removing
 * unnecessary qualifiers for FFI purposes.
 */
function normalizeType(raw: string): string {
  return raw
    .replace(/\b(const|volatile|restrict|extern|static|inline)\b/g, '')
    .replace(/\s+/g, ' ')
    .trim();
}

/**
 * Map a C type string to a Gambit FFI type.
 * Returns [ffiType, isCustomPointer] where isCustomPointer indicates
 * whether we need a c-define-type for this type.
 */
function mapCType(
  cType: string,
  knownTypes: Set<string>,
): { ffi: string; needsDefine: boolean } {
  const normalized = normalizeType(cType);

  // Direct mapping for primitive types
  if (C_TYPE_MAP[normalized]) {
    return { ffi: C_TYPE_MAP[normalized], needsDefine: false };
  }

  // char* / const char* → UTF-8-string
  if (/^(const\s+)?char\s*\*$/.test(normalized) ||
      normalized === 'char *' || normalized === 'const char *') {
    return { ffi: 'UTF-8-string', needsDefine: false };
  }

  // unsigned char* → scheme-object (byte buffer)
  if (/^(const\s+)?unsigned\s+char\s*\*$/.test(normalized)) {
    return { ffi: 'scheme-object', needsDefine: false };
  }

  // void* → (pointer void)
  if (/^(const\s+)?void\s*\*$/.test(normalized)) {
    return { ffi: '(pointer void)', needsDefine: false };
  }

  // TypeName* → TypeName* (custom pointer type needing c-define-type)
  const ptrMatch = normalized.match(/^(const\s+)?(\w+)\s*\*$/);
  if (ptrMatch) {
    const baseName = ptrMatch[2];
    const ptrName = baseName + '*';
    return { ffi: ptrName, needsDefine: true };
  }

  // TypeName (no pointer) that is a known typedef
  if (knownTypes.has(normalized)) {
    return { ffi: normalized, needsDefine: false };
  }

  // Fallback: treat as opaque
  return { ffi: normalized, needsDefine: false };
}

/**
 * Parse typedef declarations from preprocessed source.
 */
function parseTypedefs(source: string): CTypedef[] {
  const results: CTypedef[] = [];

  // typedef struct X_st *X_t;  or  typedef struct X X_t;
  // typedef enum X_enum X_t;
  const typedefRe =
    /typedef\s+(?:struct|enum|union)\s+\w+\s*(\*?)\s*(\w+)\s*;/g;
  let m;
  while ((m = typedefRe.exec(source)) !== null) {
    results.push({
      name: m[2],
      isPointer: m[1] === '*',
      baseType: m[0].match(/(?:struct|enum|union)\s+(\w+)/)![1],
    });
  }

  // typedef void (*callback_t)(...);  — skip function pointer typedefs
  // typedef int X_t;  or  typedef unsigned long X_t;
  const simpleTypedefRe =
    /typedef\s+((?:(?:unsigned|signed|long|short|const|volatile)\s+)*\w+)\s+(\w+)\s*;/g;
  while ((m = simpleTypedefRe.exec(source)) !== null) {
    const name = m[2];
    const base = m[1].trim();
    // Skip if already captured above or if it looks like a function pointer
    if (results.some((r) => r.name === name)) continue;
    if (base.includes('(')) continue;
    results.push({
      name,
      isPointer: false,
      baseType: base,
    });
  }

  return results;
}

/**
 * Parse function declarations from preprocessed source.
 * Handles: return_type func_name(params);
 * Also handles: extern return_type func_name(params);
 * Handles pointer return types like: const char *func(...);
 */
function parseFunctions(source: string): CFunction[] {
  const results: CFunction[] = [];

  // Strategy: find "identifier ( params ) ;" patterns, then extract
  // the return type from everything before the identifier.
  // This handles pointer returns like "simple_ctx_t *func(void);"
  const funcDeclRe =
    /(?:^|[;\n}])\s*(?:extern\s+)?([\w\s*]+?)\s+(\*?\s*\w+)\s*\(([^)]*)\)\s*;/g;

  let m;
  while ((m = funcDeclRe.exec(source)) !== null) {
    let returnPart = m[1].trim();
    let namePart = m[2].trim();
    const paramStr = m[3].trim();

    // If the name starts with *, the * belongs to the return type
    if (namePart.startsWith('*')) {
      returnPart += ' *';
      namePart = namePart.slice(1).trim();
    }

    // Skip if return type contains typedef or #
    if (returnPart.includes('typedef') || returnPart.includes('#')) continue;
    // Skip if the name is not a valid C identifier
    if (!/^\w+$/.test(namePart)) continue;

    const returnType = normalizeType(returnPart);

    // Parse parameters
    const params: Array<{ type: string; name: string }> = [];
    if (paramStr && paramStr !== 'void') {
      const paramParts = splitParams(paramStr);
      for (const part of paramParts) {
        const parsed = parseParam(part.trim());
        if (parsed) params.push(parsed);
      }
    }

    results.push({ name: namePart, returnType, params });
  }

  return results;
}

/**
 * Split parameter string by commas, respecting nested parens
 * (for function pointer params).
 */
function splitParams(paramStr: string): string[] {
  const parts: string[] = [];
  let depth = 0;
  let current = '';
  for (const ch of paramStr) {
    if (ch === '(') depth++;
    else if (ch === ')') depth--;
    if (ch === ',' && depth === 0) {
      parts.push(current);
      current = '';
    } else {
      current += ch;
    }
  }
  if (current.trim()) parts.push(current);
  return parts;
}

/**
 * Parse a single parameter like "const char *name" or "int count".
 */
function parseParam(param: string): { type: string; name: string } | null {
  const trimmed = param.trim();
  if (!trimmed || trimmed === '...') return null;

  // Handle function pointer params: void (*name)(...)
  if (trimmed.includes('(*)') || trimmed.includes('( *)')) {
    return { type: '(pointer void)', name: 'callback' };
  }

  // Split into type and name: everything except the last word is the type,
  // unless the last token has a *.
  const tokens = trimmed.split(/\s+/);
  if (tokens.length === 1) {
    // Just a type with no name (e.g., in prototypes)
    return { type: tokens[0], name: 'arg' };
  }

  let lastToken = tokens[tokens.length - 1];
  // Handle "char *name" where * is attached to name
  if (lastToken.startsWith('*')) {
    const typePart = tokens.slice(0, -1).join(' ') + ' *';
    const namePart = lastToken.slice(1) || 'arg';
    return { type: normalizeType(typePart), name: namePart };
  }

  // Handle "char* name" where * is attached to type
  const typePart = tokens.slice(0, -1).join(' ');
  return { type: normalizeType(typePart), name: lastToken };
}

/**
 * Parse #define constants (numeric only).
 */
function parseConstants(source: string): CConstant[] {
  const results: CConstant[] = [];
  const defineRe = /^[ \t]*#define\s+([A-Z][A-Z0-9_]*)\s+((?:0x[\da-fA-F]+|[-+]?\d+))/gm;
  let m;
  while ((m = defineRe.exec(source)) !== null) {
    results.push({ name: m[1], value: m[2] });
  }
  return results;
}

/**
 * Parse enum declarations.
 */
function parseEnums(source: string): CEnum[] {
  const results: CEnum[] = [];
  // Match both named and anonymous enums
  const enumRe = /(?:typedef\s+)?enum\s*(?:\w+)?\s*\{([^}]+)\}\s*(\w+)?\s*;/g;
  let m;
  while ((m = enumRe.exec(source)) !== null) {
    const body = m[1];
    const name = m[2] || null;
    const members: Array<{ name: string; value?: string }> = [];

    for (const line of body.split(',')) {
      const trimmed = line.trim();
      if (!trimmed) continue;
      const eqMatch = trimmed.match(/^(\w+)\s*(?:=\s*(.+))?$/);
      if (eqMatch) {
        members.push({
          name: eqMatch[1],
          value: eqMatch[2]?.trim(),
        });
      }
    }

    results.push({ name, members });
  }
  return results;
}

/**
 * Parse a C header file into structured declarations.
 */
export function parseHeader(source: string): ParsedHeader {
  const cleaned = stripComments(source);
  return {
    typedefs: parseTypedefs(cleaned),
    functions: parseFunctions(cleaned),
    constants: parseConstants(source), // #define before comment stripping — they're on separate lines
    enums: parseEnums(cleaned),
  };
}

// ── Code generation ────────────────────────────────────────────────────

/**
 * Detect create/destroy function pairs.
 * Returns a map: destroy_func_name → create_func_name
 */
function detectCreateDestroyPairs(
  functions: CFunction[],
): Map<string, string> {
  const pairs = new Map<string, string>();
  const funcNames = new Set(functions.map((f) => f.name));

  for (const fn of functions) {
    // Pattern: X_create / X_destroy
    if (fn.name.endsWith('_destroy')) {
      const prefix = fn.name.slice(0, -'_destroy'.length);
      const createName = prefix + '_create';
      if (funcNames.has(createName)) {
        pairs.set(fn.name, createName);
      }
    }
    // Pattern: X_free / X_new or X_alloc
    if (fn.name.endsWith('_free')) {
      const prefix = fn.name.slice(0, -'_free'.length);
      for (const suffix of ['_new', '_alloc', '_create', '_open']) {
        const createName = prefix + suffix;
        if (funcNames.has(createName)) {
          pairs.set(fn.name, createName);
          break;
        }
      }
    }
    // Pattern: X_close / X_open
    if (fn.name.endsWith('_close')) {
      const prefix = fn.name.slice(0, -'_close'.length);
      const openName = prefix + '_open';
      if (funcNames.has(openName)) {
        pairs.set(fn.name, openName);
      }
    }
  }

  return pairs;
}

/**
 * Find the return type of a create function to determine
 * what pointer type gets auto-cleanup.
 */
function findReturnPointerType(
  createName: string,
  functions: CFunction[],
): string | null {
  const fn = functions.find((f) => f.name === createName);
  if (!fn) return null;
  const rt = normalizeType(fn.returnType);
  const ptrMatch = rt.match(/^(\w+)\s*\*$/);
  return ptrMatch ? ptrMatch[1] : null;
}

/**
 * Generate Gambit FFI Scheme code from parsed header.
 */
function generateFfiCode(
  parsed: ParsedHeader,
  headerPath: string,
  moduleName?: string,
): string {
  const lines: string[] = [];
  const indent = '  ';

  // Collect all export names
  const exportNames: string[] = [];

  // Track which pointer types need c-define-type declarations
  const pointerTypesNeeded = new Set<string>();
  const knownTypes = new Set(parsed.typedefs.map((t) => t.name));

  // Detect create/destroy pairs for automatic cleanup
  const destroyPairs = detectCreateDestroyPairs(parsed.functions);
  // Map: base_type_name → destroy_function_name
  const typeCleanup = new Map<string, string>();
  for (const [destroyName, createName] of destroyPairs) {
    const baseType = findReturnPointerType(createName, parsed.functions);
    if (baseType) {
      typeCleanup.set(baseType, destroyName);
    }
  }

  // First pass: determine which pointer types are referenced in functions
  for (const fn of parsed.functions) {
    const allTypes = [fn.returnType, ...fn.params.map((p) => p.type)];
    for (const t of allTypes) {
      const mapped = mapCType(t, knownTypes);
      if (mapped.needsDefine) {
        const baseName = mapped.ffi.replace('*', '');
        pointerTypesNeeded.add(baseName);
      }
    }
  }

  // Collect enum member names and constant names for exports
  for (const c of parsed.constants) exportNames.push(c.name);
  for (const e of parsed.enums) {
    for (const member of e.members) exportNames.push(member.name);
  }
  for (const fn of parsed.functions) exportNames.push(fn.name);

  // Module header
  if (moduleName) {
    lines.push(`(import :std/foreign)`);
    lines.push(`(export ${exportNames.join(' ')})`);
    lines.push('');
  }

  lines.push(`(begin-ffi (${exportNames.join(' ')})`);
  lines.push('');
  lines.push(`${indent}(c-declare "#include \\"${headerPath}\\"")`) ;
  lines.push('');

  // Generate cleanup functions for types with create/destroy pairs
  if (typeCleanup.size > 0) {
    lines.push(`${indent}(c-declare #<<END-C`);
    for (const [baseType, destroyName] of typeCleanup) {
      lines.push(`___SCMOBJ ffi_free_${baseType}(void *ptr)`);
      lines.push(`{`);
      lines.push(`  ${destroyName}((${baseType}*)ptr);`);
      lines.push(`  return ___FIX(___NO_ERR);`);
      lines.push(`}`);
      lines.push('');
    }
    lines.push(`END-C`);
    lines.push(`${indent})`);
    lines.push('');
  }

  // Generate c-define-type declarations for opaque pointer types
  for (const baseName of pointerTypesNeeded) {
    const ptrName = baseName + '*';
    lines.push(`${indent}(c-define-type ${baseName} "${baseName}")`);
    if (typeCleanup.has(baseName)) {
      lines.push(
        `${indent}(c-define-type ${ptrName} (pointer ${baseName} (${ptrName}) "ffi_free_${baseName}"))`,
      );
    } else {
      lines.push(
        `${indent}(c-define-type ${ptrName} (pointer ${baseName} (${ptrName})))`,
      );
    }
    lines.push('');
  }

  // Generate define-const for #define constants
  if (parsed.constants.length > 0) {
    lines.push(`${indent};; Constants`);
    for (const c of parsed.constants) {
      lines.push(`${indent}(define-const ${c.name})`);
    }
    lines.push('');
  }

  // Generate define-const for enum members
  for (const e of parsed.enums) {
    const label = e.name ? `enum ${e.name}` : 'enum';
    lines.push(`${indent};; ${label}`);
    for (const member of e.members) {
      lines.push(`${indent}(define-const ${member.name})`);
    }
    lines.push('');
  }

  // Generate define-c-lambda for functions
  if (parsed.functions.length > 0) {
    lines.push(`${indent};; Functions`);
    for (const fn of parsed.functions) {
      const paramTypes = fn.params.map((p) => mapCType(p.type, knownTypes).ffi);
      const retType = mapCType(fn.returnType, knownTypes).ffi;
      const paramList =
        paramTypes.length === 0 ? '' : paramTypes.join(' ');
      lines.push(
        `${indent}(define-c-lambda ${fn.name} (${paramList}) ${retType} "${fn.name}")`,
      );
    }
    lines.push('');
  }

  lines.push(')');

  return lines.join('\n');
}

// ── Tool registration ──────────────────────────────────────────────────

export function registerFfiScaffoldTool(server: McpServer): void {
  server.registerTool(
    'gerbil_ffi_scaffold',
    {
      title: 'Generate FFI Bindings from C Header',
      description:
        'Parse a C header file and generate Gambit FFI binding code. ' +
        'Recognizes: typedefs (opaque pointer types), function declarations, ' +
        '#define constants, enums, and create/destroy pairs (for automatic GC cleanup). ' +
        'Returns generated Scheme code with begin-ffi, c-define-type, and c-lambda declarations. ' +
        'Does not write to disk.',
      inputSchema: {
        file_path: z
          .string()
          .describe(
            'Absolute path to a C header file (.h) to generate bindings for',
          ),
        include_path: z
          .string()
          .optional()
          .describe(
            'Include path to use in the c-declare #include directive ' +
            '(e.g. "leveldb/c.h"). Defaults to the basename of the file.',
          ),
        module_name: z
          .string()
          .optional()
          .describe(
            'If provided, generates a complete Gerbil module with import/export. ' +
            'Otherwise generates just the begin-ffi block.',
          ),
      },
    },
    async ({ file_path, include_path, module_name }) => {
      let source: string;
      try {
        source = await readFile(file_path, 'utf-8');
      } catch (e: unknown) {
        const msg = e instanceof Error ? e.message : String(e);
        return {
          content: [
            {
              type: 'text' as const,
              text: `Failed to read header file: ${msg}`,
            },
          ],
          isError: true,
        };
      }

      const parsed = parseHeader(source);

      // Determine include path
      const headerInclude =
        include_path || file_path.split('/').pop() || 'header.h';

      const code = generateFfiCode(parsed, headerInclude, module_name);

      // Build a summary
      const summaryParts: string[] = [];
      if (parsed.typedefs.length > 0) {
        summaryParts.push(`${parsed.typedefs.length} typedef(s)`);
      }
      if (parsed.functions.length > 0) {
        summaryParts.push(`${parsed.functions.length} function(s)`);
      }
      if (parsed.constants.length > 0) {
        summaryParts.push(`${parsed.constants.length} constant(s)`);
      }
      if (parsed.enums.length > 0) {
        const totalMembers = parsed.enums.reduce(
          (acc, e) => acc + e.members.length,
          0,
        );
        summaryParts.push(
          `${parsed.enums.length} enum(s) with ${totalMembers} member(s)`,
        );
      }

      const destroyPairs = detectCreateDestroyPairs(parsed.functions);
      if (destroyPairs.size > 0) {
        summaryParts.push(
          `${destroyPairs.size} create/destroy pair(s) with auto-cleanup`,
        );
      }

      const summary =
        summaryParts.length > 0
          ? `Parsed: ${summaryParts.join(', ')}\n\n`
          : 'No declarations found in header.\n\n';

      return {
        content: [
          {
            type: 'text' as const,
            text: summary + code,
          },
        ],
      };
    },
  );
}
