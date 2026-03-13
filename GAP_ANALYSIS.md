# Gerbil-MCP Gap Analysis for LLM Agent Fluency

**Date**: 2026-02-12  
**Scope**: Identifying gaps to help LLM agents (Claude, Copilot) be fluent in Gerbil Scheme  
**Project Status**: Production-grade, 114 tools, 471 cookbook recipes, 426 tests

---

## Executive Summary

The gerbil-mcp project is exceptionally mature and comprehensive. Based on detailed analysis:

- ✅ **Strengths**: 114 specialized tools, 471 verified cookbook recipes, comprehensive prompts, resources, and documentation
- ✅ **Test Coverage**: 426 tests covering all major tool categories
- ✅ **Documentation**: Excellent templates for both Claude and Copilot integration
- ⚠️ **Key Gaps**: 7 feature requests pending, some cookbook topic imbalances, infrastructure improvements needed

**Overall Assessment**: A-grade project with minor gaps. Most critical for LLM fluency are the 7 pending feature requests.

---

## 1. CRITICAL GAPS (High Impact on LLM Fluency)

### 1.1 Duplicate Definition Checker (`duplicate-definition-check`)
**Status**: Requested in features.json, HIGH impact  
**Problem**: Building large files with duplicate `def`/`defmethod` definitions fails with cryptic "Bad binding; rebind conflict" errors that don't show line numbers.  
**Impact**: LLM agents waste tokens on build-fail-grep-fix cycles (~2000 tokens per batch)  
**Solution**: Pre-build duplicate definition scanner using Gerbil reader or regex to extract all top-level binding names  
**Priority**: 🔴 **HIGH** - Directly impacts coding efficiency

### 1.2 FFI Module Verification (`verify-ffi-module-support`)
**Status**: Requested in features.json, MEDIUM impact  
**Problem**: `gerbil_verify` reports false "[ERROR] [syntax] Incomplete form, EOF reached" on files importing from `begin-ffi` modules  
**Impact**: Misleading errors waste ~300 tokens investigating non-issues  
**Solution**: Skip syntax phase for FFI dependencies or use project's compiled artifacts  
**Priority**: 🟡 **MEDIUM** - Causes confusion but workarounds exist

### 1.3 Bundled Header Support (`build-and-report-bundled-header-support`)
**Status**: Requested in features.json, MEDIUM impact  
**Problem**: `gerbil_build_and_report` rejects bundled C headers (e.g., `termbox2.h`) as "missing system headers"  
**Impact**: Forces fallback to manual `make build` (~200 tokens per occurrence)  
**Solution**: Distinguish between `#include <foo.h>` (system) and `#include "foo.h"` (local)  
**Priority**: 🟡 **MEDIUM** - FFI-heavy projects affected

### 1.4 Howto Parallel Call Stability (`howto-parallel-call-stability`)
**Status**: Requested in features.json, HIGH impact  
**Problem**: `gerbil_howto` fails with "Spread syntax requires ...iterable[Symbol.iterator]" when called in parallel  
**Impact**: Forces sequential cookbook searches, wastes ~400 tokens per session  
**Solution**: Fix race condition in cookbook loading/search logic  
**Priority**: 🔴 **HIGH** - Affects every session with multiple searches

---

## 2. MEDIUM PRIORITY GAPS

### 2.1 FFI Link Symbol Check (`ffi-link-symbol-check`)
**Status**: Requested in features.json  
**Purpose**: Cross-reference C function calls in `c-declare` blocks against `.a` library symbols using `nm`  
**Impact**: Catches missing library links before runtime (~800 tokens saved)  
**Priority**: 🟡 **MEDIUM** - Specialized but valuable for FFI work

### 2.2 Multi-Project Build Chain (`multi-project-build-chain`)
**Status**: Requested in features.json, 1 vote  
**Purpose**: Build dependent Gerbil projects in dependency order (reads `gerbil.pkg` depend:)  
**Impact**: Automates manual upstream builds (~300 tokens saved)  
**Priority**: 🟡 **MEDIUM** - Niche use case but time-saving

### 2.3 Edit Tool Whitespace Issue (`edit-tool-replace-all-whitespace-aware`)
**Status**: Requested in features.json  
**Note**: This is a **Claude Code tool issue**, not gerbil-mcp  
**Impact**: Replace-all operations strip trailing spaces, causing token merging  
**Action**: Document workaround, not fixable in gerbil-mcp  
**Priority**: 🟢 **LOW** - Out of scope for this project

---

## 3. COOKBOOK GAPS

### 3.1 Core Language Pattern Coverage
**Current**: 471 recipes, but gaps in fundamental patterns  

| Missing Pattern | Priority | Why Important |
|----------------|----------|---------------|
| `syntax-case` macro writing | 🔴 HIGH | Central to idiomatic Gerbil, only basic `defrules` covered |
| `pregexp` regular expressions | 🔴 HIGH | Common need, only 1 recipe exists |
| `call/cc` and continuations | 🟡 MEDIUM | Gambit's powerful continuation support underrepresented |
| `parameterize` / dynamic binding | 🟡 MEDIUM | Only 1 recipe (eval stdout capture) |
| String port I/O | 🟡 MEDIUM | `open-input-string`, `open-output-string` patterns missing |
| Multiple return values | 🟡 MEDIUM | `values`/`receive`/`call-with-values` not documented |
| Reader macros / `#;` datum comment | 🟢 LOW | Gerbil-specific reader extensions |

**Recommendation**: Add 10-15 recipes covering these core patterns

### 3.2 Debugging & Compilation Patterns
**Gaps**:
- No recipes on debugging compiled vs REPL-only code differences
- No recipes on `GERBIL_LOADPATH` configuration patterns
- No recipes on profiling/optimizing compilation times
- No workflow recipes for `gerbil_stale_static` / `gerbil_bisect_crash`

**Recommendation**: Add 5-8 debugging workflow recipes

---

## 4. INFRASTRUCTURE IMPROVEMENTS

### 4.1 REPL Session Buffer Management
**Issue**: `stdoutBuffer`/`stderrBuffer` grow unbounded in long-running REPL sessions  
**Risk**: Memory consumption in sessions with `preload_file` loading large modules  
**Fix**: Add configurable buffer limit (512KB) with ring-buffer semantics  
**Priority**: 🟡 **MEDIUM**

### 4.2 REPL Sentinel Polling to Event-Driven
**Issue**: `waitForSentinel()` uses 50ms polling with `setTimeout`  
**Impact**: CPU waste and latency  
**Fix**: Switch to event-driven reads on process stdout stream  
**Priority**: 🟡 **MEDIUM**

### 4.3 Subprocess Result Caching
**Issue**: No caching between tool calls querying the same module  
**Impact**: Redundant subprocess spawns when `gerbil_module_exports`, `gerbil_function_signature`, etc. query same module  
**Fix**: Add 30-second TTL cache keyed by module path + loadpath  
**Priority**: 🟢 **LOW-MEDIUM**

### 4.4 Large Tool File Refactoring
**Issue**: `build-and-report.ts` (16KB) and `check-import-conflicts.ts` (17KB) are complex monoliths  
**Fix**: Extract retry logic, loadpath detection, Makefile fallback into shared utilities  
**Priority**: 🟢 **LOW**

---

## 5. DOCUMENTATION GAPS

### 5.1 Workflow-Centric Guidance Missing
**Issue**: INSTRUCTIONS string is tool-centric, not task-centric  
**Fix**: Add common workflow chains:
- **Debug a segfault**: `gerbil_stale_static` → `gerbil_bisect_crash` → `gerbil_demangle`
- **Add a feature**: `gerbil_howto` → write code → `gerbil_check_syntax` → `gerbil_compile_check` → `gerbil_build_and_report`
- **Understand unfamiliar code**: `gerbil_file_summary` → `gerbil_document_symbols` → `gerbil_call_graph`
- **Port from another Scheme**: `gerbil_howto` → `gerbil_suggest_imports` → `gerbil_module_exports`

**Priority**: 🔴 **HIGH** - Directly improves LLM agent workflow understanding

### 5.2 Negative Guidance Missing
**Issue**: No explicit "don't do this" guidance  
**Fix**: Add warnings:
- Don't use `gerbil_eval` for syntax checking (use `gerbil_check_syntax`)
- Don't guess function names (use `gerbil_module_exports`)
- Don't assume arity (use `gerbil_function_signature`)
- Don't skip the cookbook (use `gerbil_howto` first)

**Priority**: 🟡 **MEDIUM**

### 5.3 Tiered Tool Priority Missing
**Issue**: All tools treated equally, no guidance on which to prioritize  
**Fix**: Mark tools as Essential (top 10), Common, or Specialized  

**Essential** (always use):
- `gerbil_howto`, `gerbil_eval`, `gerbil_check_syntax`, `gerbil_module_exports`, `gerbil_function_signature`, `gerbil_compile_check`, `gerbil_build_and_report`, `gerbil_run_tests`, `gerbil_doc`, `gerbil_describe`

**Priority**: 🟡 **MEDIUM**

---

## 6. PROMPT IMPROVEMENTS

### 6.1 Existing Prompts Need Updates

| Prompt | Missing Element | Priority |
|--------|----------------|----------|
| `debug-gerbil-error` | Should mention `gerbil_describe` for inspecting unexpected return values | 🟡 MEDIUM |
| `review-code` | Should mention FFI safety checks, macro hygiene, `gerbil_security_scan` | 🔴 HIGH |
| `write-gerbil-module` | Should reference checking cookbook first (`gerbil_howto`) | 🔴 HIGH |
| `convert-to-gerbil` | No mention of keyword arguments with trailing colons | 🟡 MEDIUM |
| `generate-tests` | No mention of async testing, mocking, fixtures | 🟡 MEDIUM |
| `port-to-gerbil` | No mention of quote/unquote quirks, SRFI compatibility | 🟡 MEDIUM |

### 6.2 All New Prompts Already Exist ✅
**Great News**: The following prompts requested in plan.md are already implemented:
- ✅ `optimize-gerbil-code` - Performance tuning guidance
- ✅ `migrate-gerbil-version` - v0.18→v0.19 migration
- ✅ `design-ffi-bindings` - Safe FFI binding workflow
- ✅ `refactor-gerbil-module` - Module refactoring guidance

---

## 7. TEST SUITE GAPS

### 7.1 Missing Test Categories
**Current**: 426 tests, excellent coverage of happy paths  
**Gaps**:
- ❌ **Error path coverage** - Most tools only test success cases
- ❌ **Timeout scenarios** - No tests for expressions that loop forever
- ❌ **Concurrent operations** - No tests for multiple REPL sessions
- ❌ **Multi-module integration** - No 3+ module project tests
- ❌ **Stress tests** - No large files (1000+ definitions), deep nesting (100+ levels)
- ❌ **Parameter combinations** - Rarely test multiple parameters together

**Recommendation**: Add ~50 tests covering these scenarios  
**Priority**: 🟢 **LOW-MEDIUM** - Nice to have, not critical

---

## 8. WHAT'S WORKING WELL (Preserve These)

✅ **One-tool-per-file pattern** - Excellent for maintainability  
✅ **Zod schema validation** - Consistent input validation  
✅ **Dry-run defaults** - Mutation tools preview changes  
✅ **Cookbook-first workflow** - Brilliant for niche languages  
✅ **Auto-loadpath detection** - Reads `gerbil.pkg` automatically  
✅ **Fallback strategies** - Compiled `.scm` scanning when source unavailable  
✅ **Tool annotations** - readOnlyHint/idempotentHint on all tools  
✅ **Minimal dependencies** - Only MCP SDK + Zod

---

## 9. PRIORITY RECOMMENDATIONS

### 🔴 **IMMEDIATE (Do First)**

1. **Fix `gerbil_howto` parallel call stability** (features.json #5)
   - High impact, affects every session with multiple cookbook searches
   - Estimated effort: 2-4 hours

2. **Implement duplicate definition checker** (features.json #7)
   - Prevents costly build-fail-grep-fix cycles
   - Estimated effort: 4-6 hours

3. **Add workflow-centric guidance to INSTRUCTIONS**
   - Dramatically improves LLM agent efficiency
   - Estimated effort: 2-3 hours

4. **Update `write-gerbil-module` and `review-code` prompts**
   - Add cookbook-first guidance and security scan mentions
   - Estimated effort: 1 hour

### 🟡 **SHORT TERM (Next)**

5. **Add core language cookbook recipes** (syntax-case, pregexp, call/cc)
   - 10-15 recipes, ~6-8 hours total
   - Fills major knowledge gap

6. **Fix FFI module verification** (features.json #2)
   - Eliminates false EOF errors
   - Estimated effort: 4-6 hours

7. **Add bundled header support** (features.json #1)
   - Improves FFI project workflow
   - Estimated effort: 3-4 hours

8. **Add negative guidance and tool tiers to INSTRUCTIONS**
   - Prevents common mistakes
   - Estimated effort: 1-2 hours

### 🟢 **LONG TERM (Polish)**

9. **REPL buffer management and event-driven sentinel**
   - Infrastructure improvements
   - Estimated effort: 6-8 hours

10. **Subprocess result caching**
    - Performance optimization
    - Estimated effort: 4-6 hours

11. **Test suite expansion** (error paths, stress tests)
    - Increased robustness
    - Estimated effort: 8-12 hours

12. **Implement remaining feature requests** (#3, #4, #6)
    - Nice-to-have improvements
    - Estimated effort: 12-16 hours total

---

## 10. IMPACT MATRIX

| Gap | Impact on LLM Fluency | Implementation Effort | Priority |
|-----|----------------------|----------------------|----------|
| Howto parallel stability | 🔴 Very High | 🟢 Low | 🔴 Critical |
| Duplicate definition check | 🔴 Very High | 🟡 Medium | 🔴 Critical |
| Workflow-centric docs | 🔴 Very High | 🟢 Low | 🔴 Critical |
| Core language recipes | 🔴 High | 🟡 Medium | 🔴 Critical |
| FFI module verification | 🟡 Medium | 🟡 Medium | 🟡 High |
| Bundled header support | 🟡 Medium | 🟢 Low-Medium | 🟡 High |
| Prompt improvements | 🟡 Medium | 🟢 Low | 🟡 High |
| REPL buffer management | 🟢 Low | 🟡 Medium | 🟢 Medium |
| Subprocess caching | 🟢 Low | 🟡 Medium | 🟢 Medium |
| Test suite expansion | 🟢 Low | 🟡 High | 🟢 Low |

---

## 11. SUMMARY

The gerbil-mcp project is exceptionally well-designed and comprehensive. The main gaps affecting LLM agent fluency are:

1. **7 pending feature requests** (features.json) - 4 are high-impact
2. **Cookbook imbalances** - Core language patterns underrepresented
3. **Documentation structure** - Tool-centric vs workflow-centric
4. **Minor infrastructure issues** - REPL buffers, polling, caching

**Recommended Focus**: Address the 4 high-impact items first (howto parallel stability, duplicate definition checker, workflow docs, core language recipes). These will yield the biggest improvement in LLM agent fluency with minimal effort.

**Overall Grade**: **A** (Production-ready with minor improvements needed)

---

## 12. APPENDIX: DETAILED STATISTICS

### Tool Distribution
- **Total tools**: 114
- **Subprocess-based**: 71% (81 tools)
- **Pure static analysis**: 29% (33 tools)
- **Read-only**: 90+ tools
- **Mutation tools**: ~10 (with dry-run defaults)

### Cookbook Distribution (471 recipes)
- QT/GUI: ~40 (10%)
- Standard library: ~115 (29%)
- R7RS / :scheme/*: ~23 (6%)
- FFI/C bindings: ~24 (6%)
- Core language: ~25 (6%) ⚠️ **Should be higher**
- AWS integration: ~15 (4%)
- SRFIs: ~13 (3%)
- Version migration: ~26 (7%)
- Other: ~190 (48%)

### Test Coverage
- **Total tests**: 426
- **Categories covered**: 50+ tool types
- **Integration tests**: Limited multi-module scenarios
- **Error path tests**: Sparse
- **Stress tests**: None

### Documentation
- **Prompts**: 11 (comprehensive set)
- **Resources**: 25+ (5 reference docs + 20+ dynamic stdlib docs)
- **Templates**: 2 (CLAUDE.md.gerbil-example, copilot-instructions.md.gerbil-example)

---

**Analysis Completed**: 2026-02-12  
**Next Action**: Review with maintainer, prioritize implementation
