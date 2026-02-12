# Quick Wins for gerbil-mcp LLM Fluency

**Goal**: Implement highest-impact improvements with minimal effort

---

## 🔴 CRITICAL - Do These First (Est. 10-15 hours total)

### 1. Fix Howto Parallel Call Stability (2-4 hours)
**File**: `src/tools/howto.ts`  
**Issue**: Tool fails with "Spread syntax requires ...iterable[Symbol.iterator]" when called in parallel  
**Impact**: Affects EVERY session with multiple cookbook searches  
**Fix**: Add proper synchronization or cloning for cookbook data structure

```typescript
// Current issue: Shared state corruption in parallel calls
// Solution: Clone cookbook data or add mutex
```

### 2. Implement Duplicate Definition Checker (4-6 hours)
**New file**: `src/tools/check-duplicates.ts`  
**Issue**: Build fails with "Bad binding; rebind conflict" without line numbers  
**Impact**: Saves ~2000 tokens per large batch addition  
**Implementation**:
```typescript
// Use Gerbil reader or regex to extract all top-level bindings:
// def, defmethod, defrule, defsyntax, def-once, etc.
// Report duplicates with line numbers
```

### 3. Add Workflow Chains to INSTRUCTIONS (2-3 hours)
**File**: `src/index.ts` (INSTRUCTIONS string)  
**Impact**: Dramatically improves LLM workflow understanding  
**Add 5 common workflows**:

```markdown
## Common Workflows

**Debug a segfault**:
1. gerbil_stale_static (check for stale global artifacts)
2. gerbil_bisect_crash (isolate crashing forms)
3. gerbil_demangle (decode stack trace)

**Add a feature**:
1. gerbil_howto (search cookbook for patterns)
2. Write code
3. gerbil_check_syntax (validate syntax)
4. gerbil_compile_check (catch unbound identifiers)
5. gerbil_build_and_report (full build with diagnostics)

**Understand unfamiliar code**:
1. gerbil_file_summary (structural overview)
2. gerbil_document_symbols (list all definitions)
3. gerbil_call_graph (visualize call relationships)
4. gerbil_module_deps (show dependencies)

**Port from another Scheme**:
1. gerbil_howto (search for equivalent patterns)
2. gerbil_suggest_imports (resolve unbound identifiers)
3. gerbil_module_exports (verify available functions)
4. gerbil_check_syntax (validate Gerbil syntax)

**Refactor a module**:
1. gerbil_check_exports (verify export consistency)
2. gerbil_find_callers (find all usages)
3. gerbil_rename_symbol (safe renaming)
4. gerbil_check_import_conflicts (catch conflicts)
```

### 4. Update Key Prompts (1 hour)
**Files**: `src/prompts.ts`  
**Changes**:

```typescript
// write-gerbil-module prompt:
// Add: "Before writing code, search the cookbook with gerbil_howto"

// review-code prompt:
// Add: "Check for security issues with gerbil_security_scan"
// Add: "Verify FFI safety with gerbil_ffi_type_check"
// Add: "Check macro hygiene with gerbil_macro_hygiene_check"

// debug-gerbil-error prompt:
// Add: "Use gerbil_describe to inspect unexpected return values"
```

---

## 🟡 HIGH PRIORITY - Do Next (Est. 15-20 hours total)

### 5. Add Core Language Cookbook Recipes (6-8 hours)
**File**: `cookbooks.json`  
**Add 10-15 recipes for**:
- `syntax-case` macro writing (3-4 recipes)
- `pregexp` regular expressions (3-4 recipes)
- `call/cc` and continuations (2-3 recipes)
- `parameterize` / dynamic binding (2-3 recipes)
- Multiple return values (`values`/`receive`) (2 recipes)

**Example recipe structure**:
```json
{
  "id": "syntax-case-basic",
  "title": "Define a macro with syntax-case",
  "description": "Basic syntax-case macro with pattern matching",
  "code": "(defsyntax (my-when stx)\n  (syntax-case stx ()\n    [(_ test body ...)\n     #'(if test (begin body ...) #!void)]))",
  "imports": [],
  "tags": ["macro", "syntax-case", "core-language"],
  "valid_for": ["v0.18", "v0.19"]
}
```

### 6. Fix FFI Module Verification (4-6 hours)
**File**: `src/tools/verify-utils.ts`, `src/tools/howto-verify.ts`  
**Issue**: False EOF errors on files importing from `begin-ffi` modules  
**Fix**: Add `--skip-ffi-deps` flag or use compiled artifacts for resolution

### 7. Add Bundled Header Support (3-4 hours)
**File**: `src/tools/build-and-report.ts`  
**Issue**: Rejects bundled C headers as "missing system headers"  
**Fix**: Distinguish `#include <foo.h>` (system) from `#include "foo.h"` (local)

```typescript
// Detect system vs local includes:
const systemHeaderRegex = /#include\s*<([^>]+)>/g;
const localHeaderRegex = /#include\s*"([^"]+)"/g;
// Only flag system headers not found via pkg-config
```

### 8. Add Negative Guidance to INSTRUCTIONS (1-2 hours)
**File**: `src/index.ts` (INSTRUCTIONS string)  
**Add warnings**:

```markdown
## What NOT to Do

- ❌ Don't use gerbil_eval for syntax checking → use gerbil_check_syntax
- ❌ Don't guess function names → use gerbil_module_exports
- ❌ Don't assume arity → use gerbil_function_signature
- ❌ Don't skip the cookbook → use gerbil_howto before writing code
- ❌ Don't build without checking syntax first → run gerbil_check_syntax
```

---

## 🟢 NICE TO HAVE - Do Later (Est. 20-30 hours total)

### 9. REPL Buffer Management (3-4 hours)
**File**: `src/gxi.ts` (lines ~466-472)  
**Fix**: Add 512KB ring buffer limit for `stdoutBuffer`/`stderrBuffer`

### 10. Event-Driven REPL Sentinel (4-6 hours)
**File**: `src/gxi.ts` (waitForSentinel function)  
**Fix**: Replace 50ms polling with event-driven reads on stdout stream

### 11. Subprocess Result Caching (4-6 hours)
**New file**: `src/cache.ts`  
**Purpose**: Cache module introspection results (30-second TTL)  
**Key by**: module path + loadpath

### 12. Refactor Large Tool Files (4-6 hours)
**Files**: `src/tools/build-and-report.ts`, `src/tools/check-import-conflicts.ts`  
**Extract into**: `src/utils/build-utils.ts`
- Retry logic
- Loadpath detection
- Makefile fallback

### 13. Implement Remaining Feature Requests (12-16 hours)
**From features.json**:
- FFI link symbol check (#4)
- Multi-project build chain (#6)

---

## Implementation Order (Optimal)

**Week 1** (Critical items):
1. Day 1-2: Fix howto parallel stability + Update prompts (5 hours)
2. Day 3-4: Implement duplicate definition checker (6 hours)
3. Day 5: Add workflow chains to INSTRUCTIONS (3 hours)

**Week 2** (High priority):
1. Day 1-3: Add 10-15 core language cookbook recipes (8 hours)
2. Day 4: Fix FFI module verification (6 hours)
3. Day 5: Add bundled header support + negative guidance (5 hours)

**Week 3+** (Nice to have):
- Infrastructure improvements as time permits
- Remaining feature requests
- Test suite expansion

---

## Success Metrics

After implementing critical + high priority items:

✅ **Reduced token waste**:
- Howto parallel failures: 0 (down from ~400/session)
- Duplicate definition cycles: 0 (down from ~2000/batch)
- FFI false errors: 0 (down from ~300/occurrence)

✅ **Improved LLM fluency**:
- Workflow understanding: Clear task-based guidance
- Cookbook coverage: Core language patterns documented
- Prompt quality: Security and cookbook-first integrated

✅ **Better developer experience**:
- Fewer false positives
- Clearer error messages
- Faster iteration cycles

---

**Total Estimated Effort**: 25-35 hours for critical + high priority items  
**Expected Impact**: 80%+ reduction in common pain points
