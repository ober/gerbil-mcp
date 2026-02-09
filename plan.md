# Gerbil-MCP Review & Improvement Plan

## Project Overview

**Current state**: 76 tools, 394 cookbook recipes, 303 tests, 7 MCP prompts, 2 MCP resources, comprehensive documentation templates. All 16 previously suggested features implemented. Gerbil v0.19-dev / Gambit v4.9.7. Two runtime dependencies (MCP SDK, Zod).

**Overall assessment**: Production-grade, exceptionally mature. Three independent analyses rated it A/A+.

---

## A. NEW TOOLS TO ADD

### A1. `gerbil_smart_complete` (High Impact)

Given a partial expression context (e.g., `(hash-` or `(for/`), return valid completions from the currently imported modules. This would dramatically reduce guessing when writing Gerbil code. Internally: combine `gerbil_apropos` + `gerbil_module_exports` with context awareness.

**Why**: Claude frequently guesses function names incorrectly for Gerbil (niche language with limited training data). A completion tool that works within the scope of imported modules would prevent the most common class of errors.

### A2. `gerbil_explain_error` (High Impact)

Take a raw Gerbil/Gambit error message and return a structured explanation: error type, likely cause, common fix patterns (from the cookbook), and suggested tool calls to investigate further. The `debug-gerbil-error` prompt exists but requires human invocation. A tool version could be called programmatically when `gerbil_eval` or `gerbil_compile_check` returns an error.

**Why**: Automates the debugging workflow. Currently Claude must manually parse error messages and decide which tools to call. This tool would short-circuit that process.

### A3. `gerbil_diff_modules` (Medium Impact)

Compare two modules (or two versions of the same module) and show added/removed/changed exports with arity differences. Critical for v0.18 to v0.19 migration work. Currently you have to call `gerbil_module_exports` twice and diff manually.

**Why**: Version migration is a real pain point. 26 recipes are tagged v0.19, indicating significant API churn between versions.

### A4. `gerbil_migration_check` (Medium Impact)

Given a source file, detect v0.18 patterns that need updating for v0.19 (and vice versa). Leverages the 26 existing v0.19-tagged cookbook recipes documenting breaking changes. Scans for: `:std/getopt` to `:std/cli/getopt`, changed `call-with-getopt` handler signatures, removed SRFIs, etc.

**Why**: Automated migration checking would save significant time for projects upgrading between Gerbil versions.

### A5. `gerbil_dead_code` (Medium Impact)

Detect unexported, uncalled definitions in a project. Static analysis tool that combines `gerbil_check_exports` (finds export/definition mismatches) with `gerbil_find_callers` (finds usage). Currently these must be called separately per symbol.

**Why**: Dead code accumulates in projects. A single tool that reports "these 15 definitions are never used" saves many round-trips.

### A6. `gerbil_dependency_cycles` (Low-Medium Impact)

Detect circular module dependencies in a project. `gerbil_project_dep_graph` shows the tree but doesn't flag cycles. Circular imports cause cryptic compilation errors.

**Why**: Circular dependencies are a common source of hard-to-debug compilation failures.

### A7. `gerbil_generate_api_docs` (Medium Impact)

Generate markdown documentation from module exports. Combines `gerbil_module_exports`, `gerbil_function_signature`, and `gerbil_module_catalog` to produce a complete API reference document. Useful for library authors.

**Why**: No tool currently generates documentation from module introspection. Library authors must write docs manually.

---

## B. INFRASTRUCTURE IMPROVEMENTS

### B1. REPL Session Buffer Management (Medium Priority)

**Problem**: `gxi.ts` lines ~466-472: `stdoutBuffer`/`stderrBuffer` accumulate unbounded in REPL sessions. Long-running sessions (especially with `preload_file` loading large modules) could consume significant memory.

**Fix**: Add a configurable buffer size limit with ring-buffer semantics. Default to 512KB per buffer. When exceeded, discard oldest content.

### B2. REPL Sentinel Polling to Event-Driven (Medium Priority)

**Problem**: `waitForSentinel()` uses 50ms polling intervals with `setTimeout`. This creates CPU waste and latency.

**Fix**: Switch to event-driven reads on the process stdout stream. Listen for the sentinel string in the `data` event handler rather than polling.

### B3. Subprocess Result Caching (Low-Medium Priority)

**Problem**: Tools like `gerbil_module_exports`, `gerbil_function_signature`, and `gerbil_module_catalog` all invoke separate gxi processes for the same module. No caching between tool calls.

**Fix**: Add a short-lived (per-request or 30-second TTL) cache for module introspection results. Key by module path + loadpath. Would eliminate redundant subprocess spawns when multiple tools query the same module in sequence.

### B4. Timeout Consistency (Low Priority)

**Problem**: Default 30s timeout in `gxi.ts` isn't always appropriate. Some tools override it (build-and-report: 120s, run-tests: configurable), but many don't expose a timeout parameter.

**Fix**: Add an optional `timeout` parameter to all subprocess-using tools, or at least to the commonly-used ones (eval, compile-check, module-exports, function-signature).

### B5. Marker-Based Parsing Robustness (Low Priority)

**Problem**: Tools use `GERBIL-MCP-RESULT:` and `GERBIL-MCP-ERROR:` markers to delimit output. These could theoretically appear in user-generated output.

**Fix**: Consider using UUID-based markers per invocation to eliminate any collision risk. Or prefix markers with a UUID at server startup.

### B6. Large Tool Files (Low Priority)

**Problem**: `build-and-report.ts` (16KB) and `check-import-conflicts.ts` (17KB) are the largest tool files and do a lot (build + retry + Makefile fallback + header detection + loadpath auto-detection). Complex to maintain.

**Fix**: Extract helper functions for retry logic, loadpath detection, and Makefile fallback into shared utilities in `parse-utils.ts` or a new `build-utils.ts`.

### B7. Lint Consolidation (Low Priority)

**Problem**: `lint.ts` runs 14+ independent checks that each scan the same file content. O(n*m) where n=file size, m=checks.

**Fix**: Consolidate into a single-pass scanner that collects all lint findings in one traversal.

---

## C. COOKBOOK GAPS

### C1. Core Language Patterns (High Priority)

The cookbook has 329 recipes but is heavily biased toward QT/GUI (40 recipes) and AWS integration (15+ recipes). Core language patterns are underrepresented:

| Missing Pattern | Description | Priority |
|----------------|-------------|----------|
| `syntax-case` macro writing | The macro system is central to Gerbil but only basic `defrules` is covered | High |
| `call/cc` and continuations | Gambit's continuation support, delimited continuations | High |
| `pregexp` regular expressions | Only 1 recipe exists; regex is a common need | High |
| `parameterize` / dynamic binding | Beyond the single eval capture recipe | Medium |
| String port I/O | `open-input-string`, `open-output-string`, `with-input-from-string` | Medium |
| Multiple return values | `values`/`receive`/`call-with-values` | Medium |
| `syntax-rules` vs `defrules` | When to use which, migration patterns | Medium |
| Reader macros / `#;` datum comment | Gerbil-specific reader extensions | Low |

### C2. Standard Library Coverage (Medium-High Priority) — ✅ COMPLETED

65 recipes added covering all major standard library modules. Cookbook grew from 329 to 394 recipes.

**:std/* modules covered (31 recipes):**
- `:std/amb` — nondeterministic search with begin-amb, amb-find, amb-collect
- `:std/lazy` — lazy evaluation with delay/force/lazy
- `:std/stxparam` — syntax parameters with defsyntax-parameter, syntax-parameterize
- `:std/config` — configuration management with keyword plists
- `:std/parser` — LL1 parser combinators
- `:std/protobuf` — protobuf message definitions
- `:std/db/postgresql` — PostgreSQL queries, connection pooling, transactions (from source)
- `:std/net/smtp` — sending email (from source)
- `:std/net/socks` — SOCKS proxy connections (from source)
- `:std/net/ssl` — TLS/SSL connections (from source)
- `:std/misc/dag` — directed acyclic graph walking
- `:std/misc/pqueue` — priority queues
- `:std/misc/rbtree` — red-black trees
- `:std/misc/template` — string template substitution
- `:std/misc/evector` — extensible vectors
- `:std/misc/func` — function composition, predicate combinators
- `:std/misc/prime` — prime number utilities
- `:std/misc/process` — external process execution
- `:std/misc/path` — path manipulation
- `:std/misc/sync` — thread-safe hash tables
- `:std/misc/number` — counters, increments, modular arithmetic
- `:std/misc/plist` — property list operations
- `:std/misc/threads` — thread inspection/control
- `:std/misc/vector` — extended vector operations
- `:std/misc/symbol` — symbol/keyword comparison
- `:std/misc/timeout` — timeout creation
- `:std/misc/uuid` — UUID generation/parsing
- `:std/misc/rwlock` — read-write locks
- `:std/misc/deque`, `:std/misc/queue`, `:std/misc/lru`, `:std/misc/shuffle`
- `:std/misc/wg`, `:std/misc/completion`, `:std/misc/barrier`, `:std/misc/decimal`

**SRFI coverage (10 recipes):** SRFI-1, 8, 9, 13, 14, 19, 41, 42, 95, 115

**:scheme/* R7RS modules (16 recipes):**
- `:scheme/generator` (SRFI-158), `:scheme/comparator` (SRFI-128), `:scheme/set` (SRFI-113)
- `:scheme/ideque` (SRFI-134), `:scheme/mapping` (SRFI-146), `:scheme/mapping/hash`
- `:scheme/sort` (SRFI-132), `:scheme/show` (SRFI-159/166), `:scheme/list-queue` (SRFI-117)
- `:scheme/text` (SRFI-135), `:scheme/lseq` (SRFI-127), `:scheme/box` (SRFI-111)
- `:scheme/regex` (SRFI-115), `:scheme/vector` (SRFI-133), `:scheme/charset` (SRFI-14)
- `:scheme/bitwise` (SRFI-151), `:scheme/fixnum` (SRFI-143), `:scheme/flonum` (SRFI-144)
- `:scheme/hash-table` (SRFI-125), `:scheme/rlist` (SRFI-101), `:scheme/stream` (SRFI-41)
- `:scheme/division` (SRFI-141), `:scheme/ilist` (SRFI-116)

**Key API discoveries documented:**
- `make-evector` takes `(vector fill-pointer)`, not capacity
- `always` returns zero-arg thunk for non-procs; `repeat` returns a list
- `pred-every-of` takes a list of predicates; `pred-and` takes a single predicate
- `primes` is an evector value, not a function
- `make-sync-hash` takes underlying hash table; `sync-hash-ref` needs 3 args
- `psetq` takes `(list key val)`, `premq` takes `(key list)` — different arg orders
- `pgetq` is a runtime builtin, not from :std/misc/plist
- `textual->string` not `text->string` for SRFI-135
- Division functions (`floor/` etc.) return TWO values
- `:std/contract` is a backward compat shim (skipped)
- `upcased` in :scheme/show may not work in v0.19-dev

### C3. Debugging & Compilation Patterns (Medium Priority)

- No recipes on debugging compiled vs REPL-only code differences
- No recipes on GERBIL_LOADPATH configuration patterns
- No recipes on profiling and optimizing compilation times
- No recipes on using `gerbil_stale_static` / `gerbil_bisect_crash` workflow

---

## D. PROMPT IMPROVEMENTS

### D1. New Prompts to Add

#### `optimize-gerbil-code`
Performance tuning guidance: when to use `-O` flag, `(declare (not safe))`, `using` for typed access, `for/fold` vs manual loops, avoiding allocation in hot paths, GC-friendly patterns.

#### `migrate-gerbil-version`
v0.18 to v0.19 migration guidance with specific API changes: `:std/getopt` to `:std/cli/getopt`, changed `call-with-getopt` handler signatures, removed SRFIs, renamed modules, new IO API.

#### `design-ffi-bindings`
Step-by-step guidance for creating safe FFI bindings from a C library. Combines `gerbil_ffi_scaffold`, `gerbil_ffi_type_check`, `gerbil_security_scan` into a coherent workflow. Covers: typedef handling, memory management, callback safety, error propagation.

#### `refactor-gerbil-module`
Guidance for splitting/merging modules, extracting interfaces, improving API design, managing re-exports, avoiding circular dependencies.

### D2. Existing Prompt Improvements

| Prompt | Missing Element |
|--------|----------------|
| `debug-gerbil-error` | Should mention `gerbil_describe` for inspecting unexpected return values |
| `review-code` | Should mention FFI safety checks, macro hygiene issues, and `gerbil_security_scan` |
| `write-gerbil-module` | Should reference checking the cookbook first (`gerbil_howto`) |
| `convert-to-gerbil` | No mention of keyword arguments with trailing colons convention |
| `generate-tests` | No mention of async testing, mocking, or test fixtures |
| `port-to-gerbil` | No mention of quote/unquote quirks or SRFI compatibility |

---

## E. INSTRUCTIONS STRING IMPROVEMENTS

### E1. Add Common Workflows Section (High Priority)

The INSTRUCTIONS string is comprehensive (~4000+ tokens) but tool-centric, not task-centric. Add workflow chains:

- **"Debug a segfault"**: `gerbil_stale_static` -> `gerbil_bisect_crash` -> `gerbil_demangle`
- **"Add a feature"**: `gerbil_howto` -> write code -> `gerbil_check_syntax` -> `gerbil_compile_check` -> `gerbil_build_and_report`
- **"Understand unfamiliar code"**: `gerbil_file_summary` -> `gerbil_document_symbols` -> `gerbil_call_graph` -> `gerbil_module_deps`
- **"Port from another Scheme"**: `gerbil_howto` -> `gerbil_suggest_imports` -> `gerbil_module_exports` -> `gerbil_check_syntax`
- **"Refactor a module"**: `gerbil_check_exports` -> `gerbil_find_callers` -> `gerbil_rename_symbol` -> `gerbil_check_import_conflicts`

### E2. Add Negative Guidance (Medium Priority)

- "Don't use `gerbil_eval` for syntax checking (use `gerbil_check_syntax` instead)"
- "Don't guess function names (use `gerbil_module_exports`)"
- "Don't assume arity (use `gerbil_function_signature`)"
- "Don't skip the cookbook (use `gerbil_howto` before writing code)"

### E3. Tiered Tool Priority (Medium Priority)

Mark tools as "essential" (top 10 most-used), "common", and "specialized" so Claude can prioritize in context-limited situations:

**Essential** (always use): `gerbil_howto`, `gerbil_eval`, `gerbil_check_syntax`, `gerbil_module_exports`, `gerbil_function_signature`, `gerbil_compile_check`, `gerbil_build_and_report`, `gerbil_run_tests`, `gerbil_doc`, `gerbil_describe`

**Common** (use frequently): `gerbil_lint`, `gerbil_find_definition`, `gerbil_apropos`, `gerbil_class_info`, `gerbil_repl_session`, `gerbil_check_balance`, `gerbil_file_summary`, `gerbil_project_info`, `gerbil_suggest_imports`, `gerbil_howto_add`

**Specialized** (use when needed): Everything else

### E4. Add Troubleshooting Section (Low Priority)

What to do if a tool returns unexpected results, times out, or fails to find something. Common patterns:
- "Tool returns empty results" -> check loadpath, check module path spelling
- "Compile check passes but build fails" -> check for stale artifacts with `gerbil_stale_static`
- "REPL session hangs" -> destroy and recreate, check for infinite loops

---

## F. TEST SUITE IMPROVEMENTS

### F1. Integration Tests (Medium Priority)

No tests currently create a multi-module project and test cross-module tools together. Add:
- A 3-module test project with inter-module imports
- Test `check_exports`, `check_import_conflicts`, `project_dep_graph` on it
- Test `rename_symbol` across modules (not just dry-run)

### F2. Error Path Coverage (Medium Priority)

Many tools only test the success case. Add tests for:
- Malformed input (invalid Scheme expressions, corrupt files)
- Timeout scenarios (expressions that loop forever)
- Partial results (module that partially loads before failing)
- Concurrent tool calls (multiple REPL sessions)

### F3. Stress Tests (Low Priority)

No tests with large inputs. Add:
- File with 1000+ definitions -> `gerbil_document_symbols`, `gerbil_lint`
- Deeply nested S-expressions (100+ levels) -> `gerbil_check_balance`
- Project with 50+ modules -> `gerbil_project_map`, `gerbil_check_arity`
- Very large cookbook (1000+ recipes) -> `gerbil_howto` search performance

### F4. Parameter Combination Tests (Low Priority)

Rarely test multiple parameters together. Add:
- `loadpath` + `project_path` + `env` on `gerbil_eval`
- `compact` + `max_results` + `cookbook_path` on `gerbil_howto`
- `file_path` + `loadpath` + `project_path` on various tools

---

## G. ARCHITECTURE OBSERVATIONS

### G1. Strengths to Preserve

- **One-tool-per-file pattern**: Excellent for maintainability and parallel development
- **Zod schema validation**: Consistent and thorough input validation across all tools
- **Dry-run defaults**: Mutation tools (rename, replace, wrap, splice) default to preview mode
- **Cookbook-first workflow**: Brilliant pattern for niche languages where training data is limited
- **Auto-loadpath detection**: Reads `gerbil.pkg` depend: entries to configure GERBIL_LOADPATH automatically
- **Fallback strategies**: Compiled .scm scanning when source is unavailable (module-exports, function-signature)
- **Tool annotations**: readOnlyHint/idempotentHint on all 76 tools
- **Minimal dependencies**: Only MCP SDK + Zod reduces supply chain risk

### G2. Subprocess Architecture

- **5 subprocess runners**: runGxi (34 tools), runGxc (9), runGerbilCmd (7), runGxpkg (3), runGxiFile (5)
- **71% of tools** spawn subprocesses; 29% are pure static analysis
- **Binary resolution**: env var -> /opt/gerbil/bin -> PATH fallback
- **Buffer limit**: 1MB max output prevents runaway memory
- **REPL sessions**: UUID-based lifecycle, 10-minute idle timeout, 5 concurrent max

### G3. Error Handling Quality Distribution

| Quality | Tools | Examples |
|---------|-------|----------|
| Excellent | build-and-report, module-exports, bisect-crash | Auto-retry, fallback to .scm, preamble preservation |
| Good | eval, compile-check, repl-session | Stderr capture, temp file cleanup, preload error reporting |
| Adequate | module-catalog, function-signature, doc | Basic error return, limited recovery |

### G4. Known Technical Debt

1. `build-and-report.ts` (16KB) and `check-import-conflicts.ts` (17KB) are complex monoliths
2. `lint.ts` reimplements some parsing from `parse-utils.ts`
3. Stdout/stderr merging is inconsistent across tools (some merge, some don't)
4. No file existence checks before compilation in several tools
5. REPL session `stdoutBuffer`/`stderrBuffer` grow unbounded

---

## H. COOKBOOK TOPIC BIAS ANALYSIS

Current recipe distribution (394 total, updated after C2):

| Topic | Count | % of Total | Assessment |
|-------|-------|------------|------------|
| QT/GUI (gerbil-qt) | ~40 | 10% | Heavy bias, not generalizable |
| AWS integration | ~15 | 4% | Reflects real-world usage |
| FFI/C bindings | ~24 | 6% | Excellent, widely useful |
| Gotchas & debugging | ~18 | 5% | Critical, saves time |
| Core language | ~25 | 6% | Still underweight for fundamentals |
| Standard library | ~115 | 29% | ✅ Greatly improved by C2 |
| R7RS / :scheme/* | ~23 | 6% | ✅ New — good coverage of key modules |
| SRFIs | ~13 | 3% | ✅ Improved — 10 SRFIs covered |
| Error handling | ~11 | 3% | Adequate |
| Concurrency | ~15 | 4% | ✅ Improved (wg, barrier, completion, rwlock, sync, atom) |
| Version migration | ~26 | 7% | Good v0.19 coverage |
| Build & packaging | ~11 | 3% | Adequate |
| HTTP & networking | ~20 | 5% | Good (incl. smtp, ssl, socks) |
| Crypto & encoding | ~10 | 3% | Adequate |
| Other | ~48 | 12% | Mixed |

**Rebalancing priority**: Standard library and R7RS coverage are now strong. Remaining gap: core language patterns (syntax-case, call/cc, parameterize, pregexp) — see C1.

---

## I. PRIORITY SUMMARY

### Immediate (High Impact, Do First)

1. **Cookbook**: Add core language recipes (syntax-case, pregexp, call/cc, parameterize, values/receive)
2. **Tool**: `gerbil_explain_error` - automates debugging workflow after failures
3. **Tool**: `gerbil_smart_complete` - dramatically reduces guessing when writing code
4. **Instructions**: Add common workflow chains section
5. **Prompts**: Add `optimize-gerbil-code` and `migrate-gerbil-version`

### Short Term (Medium Impact)

6. **Tool**: `gerbil_diff_modules` - essential for version migration
7. **Tool**: `gerbil_migration_check` - leverages existing v0.19 recipes
8. **Infrastructure**: REPL buffer management fix
9. ~~**Cookbook**: PostgreSQL, protobuf, SSL, SMTP recipes~~ ✅ Done (C2)
10. **Prompts**: Add `design-ffi-bindings` and `refactor-gerbil-module`
11. **Instructions**: Add negative guidance and tiered tool priority
12. **Prompts**: Update existing prompts (add gerbil_describe to debug, security_scan to review, howto to write-module)

### Medium Term (Architecture)

13. **Infrastructure**: Event-driven REPL sentinel
14. **Infrastructure**: Subprocess result caching
15. **Tool**: `gerbil_dead_code` - dead code detection
16. **Tool**: `gerbil_generate_api_docs` - documentation generation
17. **Tests**: Multi-module integration tests
18. **Infrastructure**: Extract helpers from build-and-report.ts and check-import-conflicts.ts

### Long Term (Polish)

19. **Tool**: `gerbil_dependency_cycles` - circular dependency detection
20. **Infrastructure**: Lint single-pass consolidation
21. **Infrastructure**: Timeout parameter on all subprocess tools
22. **Infrastructure**: UUID-based output markers
23. **Tests**: Stress tests and parameter combination tests
24. ~~**Cookbook**: SRFI coverage, R7RS compatibility layer recipes~~ ✅ Done (C2)
