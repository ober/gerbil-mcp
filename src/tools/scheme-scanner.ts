/**
 * Scheme scanner for structural editing.
 *
 * Extends the checkBalance state machine logic to track form spans —
 * the positions of opener/closer pairs, head tokens, and immediate children.
 * Used by wrap-form and splice-form tools.
 *
 * Handles the same reader syntax as check-balance.ts:
 * strings, line/block/datum comments, char literals, pipe symbols, #! directives.
 */

export interface ScanPosition {
  offset: number;
  line: number;
  col: number;
}

export interface FormSpan {
  start: ScanPosition;     // opening delimiter position
  end: ScanPosition;       // position AFTER closing delimiter
  opener: string;          // '(' or '[' or '{'
  headToken: string | null; // first token after opener (e.g. "def", "let")
  children: ChildSpan[];   // immediate children (forms + atoms)
}

export interface ChildSpan {
  start: ScanPosition;
  end: ScanPosition;
  kind: 'form' | 'atom';
  text: string;            // source text of this child
}

const OPENERS: Record<string, string> = { '(': ')', '[': ']', '{': '}' };
const CLOSERS: Record<string, string> = { ')': '(', ']': '[', '}': '{' };

interface StackEntry {
  opener: string;
  start: ScanPosition;
  headToken: string | null;
  headSet: boolean;
  children: ChildSpan[];
  // Track current atom being built
  atomStart: ScanPosition | null;
}

/**
 * Scan source code and extract all top-level form spans with their children.
 */
export function scanForms(source: string): FormSpan[] {
  const topLevelForms: FormSpan[] = [];
  const stack: StackEntry[] = [];
  let line = 1;
  let col = 1;
  let i = 0;
  const len = source.length;

  function pos(): ScanPosition {
    return { offset: i, line, col };
  }

  function flushAtom(): void {
    if (stack.length > 0) {
      const top = stack[stack.length - 1];
      if (top.atomStart !== null) {
        const text = source.slice(top.atomStart.offset, i);
        if (text.trim().length > 0) {
          top.children.push({
            start: { ...top.atomStart },
            end: pos(),
            kind: 'atom',
            text,
          });
          if (!top.headSet) {
            top.headToken = text;
            top.headSet = true;
          }
        }
        top.atomStart = null;
      }
    }
  }

  while (i < len) {
    const ch = source[i];

    // --- String literal ---
    if (ch === '"') {
      // Start tracking as atom if inside a form
      const strStart = pos();
      i++;
      col++;
      while (i < len && source[i] !== '"') {
        if (source[i] === '\\') {
          i++;
          col++;
        }
        if (i < len && source[i] === '\n') {
          line++;
          col = 1;
        } else {
          col++;
        }
        i++;
      }
      if (i < len) {
        i++;
        col++;
      }
      // Record string as an atom child
      if (stack.length > 0) {
        flushAtom();
        const top = stack[stack.length - 1];
        const text = source.slice(strStart.offset, i);
        top.children.push({
          start: strStart,
          end: pos(),
          kind: 'atom',
          text,
        });
        if (!top.headSet) {
          top.headToken = text;
          top.headSet = true;
        }
      }
      continue;
    }

    // --- Line comment ---
    if (ch === ';') {
      flushAtom();
      while (i < len && source[i] !== '\n') {
        i++;
      }
      continue;
    }

    // --- Block comment #| ... |# (nestable) ---
    if (ch === '#' && i + 1 < len && source[i + 1] === '|') {
      flushAtom();
      let depth = 1;
      i += 2;
      col += 2;
      while (i < len && depth > 0) {
        if (source[i] === '#' && i + 1 < len && source[i + 1] === '|') {
          depth++;
          i += 2;
          col += 2;
        } else if (source[i] === '|' && i + 1 < len && source[i + 1] === '#') {
          depth--;
          i += 2;
          col += 2;
        } else if (source[i] === '\n') {
          line++;
          col = 1;
          i++;
        } else {
          col++;
          i++;
        }
      }
      continue;
    }

    // --- Datum comment #; ---
    if (ch === '#' && i + 1 < len && source[i + 1] === ';') {
      flushAtom();
      i += 2;
      col += 2;
      continue;
    }

    // --- #! reader directives ---
    if (ch === '#' && i + 1 < len && source[i + 1] === '!') {
      flushAtom();
      i += 2;
      col += 2;
      while (i < len && /[a-zA-Z0-9_-]/.test(source[i])) {
        i++;
        col++;
      }
      continue;
    }

    // --- Character literal #\x ---
    if (ch === '#' && i + 1 < len && source[i + 1] === '\\') {
      const charStart = pos();
      i += 2;
      col += 2;
      if (i < len && /[a-zA-Z]/.test(source[i])) {
        while (i < len && /[a-zA-Z0-9]/.test(source[i])) {
          i++;
          col++;
        }
      } else if (i < len) {
        i++;
        col++;
      }
      // Record as atom child
      if (stack.length > 0) {
        flushAtom();
        const top = stack[stack.length - 1];
        const text = source.slice(charStart.offset, i);
        top.children.push({
          start: charStart,
          end: pos(),
          kind: 'atom',
          text,
        });
        if (!top.headSet) {
          top.headToken = text;
          top.headSet = true;
        }
      }
      continue;
    }

    // --- Pipe symbol |...| ---
    if (ch === '|') {
      const pipeStart = pos();
      i++;
      col++;
      while (i < len && source[i] !== '|') {
        if (source[i] === '\\') {
          i++;
          col++;
        }
        if (i < len && source[i] === '\n') {
          line++;
          col = 1;
        } else {
          col++;
        }
        i++;
      }
      if (i < len) {
        i++;
        col++;
      }
      if (stack.length > 0) {
        flushAtom();
        const top = stack[stack.length - 1];
        const text = source.slice(pipeStart.offset, i);
        top.children.push({
          start: pipeStart,
          end: pos(),
          kind: 'atom',
          text,
        });
        if (!top.headSet) {
          top.headToken = text;
          top.headSet = true;
        }
      }
      continue;
    }

    // --- Openers ---
    if (OPENERS[ch]) {
      flushAtom();
      const start = pos();
      stack.push({
        opener: ch,
        start,
        headToken: null,
        headSet: false,
        children: [],
        atomStart: null,
      });
      i++;
      col++;
      continue;
    }

    // --- Closers ---
    if (CLOSERS[ch]) {
      flushAtom();
      if (stack.length > 0) {
        const top = stack.pop()!;
        i++;
        col++;
        const endPos = pos();

        const form: FormSpan = {
          start: top.start,
          end: endPos,
          opener: top.opener,
          headToken: top.headToken,
          children: top.children,
        };

        if (stack.length === 0) {
          topLevelForms.push(form);
        } else {
          // Add as child form of parent
          const parent = stack[stack.length - 1];
          parent.children.push({
            start: top.start,
            end: endPos,
            kind: 'form',
            text: source.slice(top.start.offset, endPos.offset),
          });
          if (!parent.headSet) {
            parent.headToken = source.slice(top.start.offset, endPos.offset);
            parent.headSet = true;
          }
        }
      } else {
        i++;
        col++;
      }
      continue;
    }

    // --- Newline ---
    if (ch === '\n') {
      flushAtom();
      line++;
      col = 1;
      i++;
      continue;
    }

    // --- Whitespace ---
    if (/\s/.test(ch)) {
      flushAtom();
      col++;
      i++;
      continue;
    }

    // --- Quote, quasiquote, unquote prefixes ---
    if (ch === "'" || ch === '`' || ch === ',') {
      // These are prefixes; skip them and let the next token handle it
      // But if we're inside a form and building an atom, start it
      if (stack.length > 0) {
        const top = stack[stack.length - 1];
        if (top.atomStart === null) {
          top.atomStart = pos();
        }
      }
      col++;
      i++;
      // Handle ,@ (unquote-splicing)
      if (ch === ',' && i < len && source[i] === '@') {
        col++;
        i++;
      }
      continue;
    }

    // --- Regular characters (atom content) ---
    if (stack.length > 0) {
      const top = stack[stack.length - 1];
      if (top.atomStart === null) {
        top.atomStart = pos();
      }
    }
    col++;
    i++;
  }

  // Flush any trailing atom
  flushAtom();

  return topLevelForms;
}

/**
 * Find the form that starts at or near a given line number (1-based).
 * Searches top-level forms first, then recurses into nested forms.
 */
export function findFormAt(source: string, targetLine: number): FormSpan | null {
  const forms = scanForms(source);

  // First, try to find a top-level form that starts at exactly this line
  for (const form of forms) {
    if (form.start.line === targetLine) {
      return form;
    }
  }

  // Then try forms that contain this line (for nested forms, rescan)
  for (const form of forms) {
    if (form.start.line <= targetLine && form.end.line >= targetLine) {
      // Check children recursively
      const nested = findNestedFormAt(source, form, targetLine);
      if (nested) return nested;
      // If no nested form starts at this line, return the containing form
      if (form.start.line === targetLine) return form;
    }
  }

  // Look for the nearest form starting at or after the target line
  for (const form of forms) {
    if (form.start.line >= targetLine) {
      return form;
    }
  }

  return null;
}

function findNestedFormAt(source: string, parent: FormSpan, targetLine: number): FormSpan | null {
  for (const child of parent.children) {
    if (child.kind === 'form' && child.start.line === targetLine) {
      // Re-scan this child's text to get its full FormSpan with children
      const childSource = source.slice(child.start.offset, child.end.offset);
      const childForms = scanForms(childSource);
      if (childForms.length > 0) {
        // Adjust positions to be absolute
        const f = childForms[0];
        return adjustFormPositions(f, child.start);
      }
    }
  }
  return null;
}

function adjustFormPositions(form: FormSpan, base: ScanPosition): FormSpan {
  return {
    start: adjustPos(form.start, base),
    end: adjustPos(form.end, base),
    opener: form.opener,
    headToken: form.headToken,
    children: form.children.map(c => ({
      start: adjustPos(c.start, base),
      end: adjustPos(c.end, base),
      kind: c.kind,
      text: c.text,
    })),
  };
}

function adjustPos(p: ScanPosition, base: ScanPosition): ScanPosition {
  return {
    offset: p.offset + base.offset,
    line: p.line === 1 ? base.line + p.line - 1 : base.line + p.line - 1,
    col: p.line === 1 ? base.col + p.col - 1 : p.col,
  };
}

/**
 * Extract children of a form from source text.
 * This is a convenience wrapper around the form's children property.
 */
export function extractChildren(source: string, span: FormSpan): ChildSpan[] {
  return span.children;
}
