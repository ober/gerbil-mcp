# Gerbil-MCP Gap Summary Table

Quick reference for all identified gaps and their priority.

---

## Feature Requests (from features.json)

| ID | Title | Impact | Effort | Priority | Status |
|----|-------|--------|--------|----------|--------|
| howto-parallel-call-stability | Fix gerbil_howto parallel call errors | 🔴 Very High | 🟢 Low | 🔴 **CRITICAL** | ❌ Not started |
| duplicate-definition-check | Pre-build duplicate definition checker | 🔴 Very High | 🟡 Medium | 🔴 **CRITICAL** | ❌ Not started |
| verify-ffi-module-support | Handle begin-ffi imports without EOF errors | 🟡 Medium | 🟡 Medium | 🟡 High | ❌ Not started |
| build-and-report-bundled-header-support | Don't reject bundled C headers | 🟡 Medium | 🟢 Low-Med | 🟡 High | ❌ Not started |
| ffi-link-symbol-check | Check c-declare references against .a symbols | 🟡 Medium | 🟡 Medium | 🟢 Medium | ❌ Not started |
| multi-project-build-chain | Build dependent projects in order | 🟡 Medium | 🟡 Medium | 🟢 Medium | ❌ Not started |
| edit-tool-replace-all-whitespace-aware | Claude Code tool whitespace issue | N/A | N/A | N/A | ⚠️ Out of scope |

**Total**: 7 requests (6 actionable, 1 out of scope)

---

## Documentation Gaps

| Gap | Description | Impact | Effort | Priority |
|-----|-------------|--------|--------|----------|
| Workflow chains | Add task-based guidance (debug, add feature, understand, port, refactor) | 🔴 Very High | 🟢 Low | 🔴 **CRITICAL** |
| Negative guidance | Add "don't do this" warnings | 🟡 Medium | 🟢 Low | 🟡 High |
| Tool priority tiers | Mark tools as Essential/Common/Specialized | 🟡 Medium | 🟢 Low | 🟡 High |
| Troubleshooting section | What to do when tools fail | 🟢 Low | 🟢 Low | 🟢 Medium |

---

## Cookbook Gaps

| Topic | Missing Patterns | Current Count | Target Count | Priority |
|-------|-----------------|---------------|--------------|----------|
| syntax-case macros | Basic, pattern matching, recursive | 0 | 3-4 | 🔴 **CRITICAL** |
| pregexp regular expressions | Basic patterns, groups, flags | 1 | 4-5 | 🔴 **CRITICAL** |
| call/cc continuations | Basic call/cc, delimited continuations | 0 | 2-3 | 🟡 High |
| parameterize / dynamic binding | Beyond stdout capture | 1 | 3-4 | 🟡 High |
| Multiple return values | values/receive/call-with-values | 0 | 2 | 🟡 High |
| String port I/O | open-input-string, with-input-from-string | 0 | 2-3 | 🟡 High |
| Reader macros | #; datum comment, custom readers | 0 | 1-2 | 🟢 Medium |
| Debugging workflows | Compiled vs REPL, stale artifacts | 0 | 5-8 | 🟡 High |
| GERBIL_LOADPATH patterns | Configuration, multi-project | 0 | 2-3 | 🟢 Medium |

**Total**: ~20-30 recipes needed across 9 categories

---

## Prompt Improvements

| Prompt | Missing Elements | Priority |
|--------|-----------------|----------|
| write-gerbil-module | Cookbook-first guidance | 🔴 **CRITICAL** |
| review-code | Security scan, FFI safety, macro hygiene | 🔴 **CRITICAL** |
| debug-gerbil-error | gerbil_describe for return values | 🟡 High |
| convert-to-gerbil | Keyword argument conventions | 🟡 High |
| generate-tests | Async testing, mocking, fixtures | 🟡 High |
| port-to-gerbil | Quote/unquote quirks, SRFI compat | 🟡 High |

---

## Infrastructure Gaps

| Issue | Location | Impact | Effort | Priority |
|-------|----------|--------|--------|----------|
| REPL buffer unbounded | src/gxi.ts (lines 466-472) | 🟡 Medium | 🟡 Medium | 🟡 High |
| REPL polling | src/gxi.ts (waitForSentinel) | 🟢 Low | 🟡 Medium | 🟢 Medium |
| No subprocess caching | All introspection tools | 🟢 Low | 🟡 Medium | 🟢 Medium |
| Large tool files | build-and-report.ts (16KB), check-import-conflicts.ts (17KB) | 🟢 Low | 🟡 Medium | 🟢 Low |
| Lint single-pass | src/tools/lint.ts | 🟢 Low | 🟡 Medium | 🟢 Low |
| Timeout parameters | Most subprocess tools | 🟢 Low | 🟡 Medium | 🟢 Low |
| Marker uniqueness | All marker-based parsing | 🟢 Low | 🟢 Low | 🟢 Low |

---

## Test Suite Gaps

| Gap Category | Examples | Impact | Effort | Priority |
|--------------|----------|--------|--------|----------|
| Error path coverage | Malformed input, corrupt files | 🟡 Medium | 🟡 High | 🟢 Medium |
| Timeout scenarios | Infinite loops, hanging expressions | 🟡 Medium | 🟡 Medium | 🟢 Medium |
| Concurrent operations | Multiple REPL sessions, parallel calls | 🟡 Medium | 🟡 Medium | 🟢 Medium |
| Multi-module integration | 3+ module project tests | 🟡 Medium | 🟡 Medium | 🟢 Medium |
| Stress tests | 1000+ definitions, 100+ nesting | 🟢 Low | 🟡 High | 🟢 Low |
| Parameter combinations | loadpath + project_path + env | 🟢 Low | 🟡 High | 🟢 Low |

**Estimated**: ~50 tests needed to fill gaps

---

## Summary Statistics

### By Priority

| Priority | Count | Total Effort (hours) |
|----------|-------|---------------------|
| 🔴 **CRITICAL** | 4 items | 10-15 |
| 🟡 High | 8 items | 15-20 |
| 🟢 Medium | 10 items | 15-25 |
| 🟢 Low | 12+ items | 20-30 |

### By Category

| Category | Gaps | Priority Distribution |
|----------|------|---------------------|
| Feature requests | 6 | 2 Critical, 2 High, 2 Medium |
| Documentation | 4 | 1 Critical, 2 High, 1 Medium |
| Cookbook | 9 topics (~25 recipes) | 2 Critical, 5 High, 2 Medium |
| Prompts | 6 | 2 Critical, 4 High |
| Infrastructure | 7 | 0 Critical, 1 High, 6 Medium-Low |
| Tests | 6 categories | 0 Critical, 0 High, 6 Medium-Low |

### Impact on LLM Fluency

| Impact Level | Item Count | Examples |
|--------------|-----------|----------|
| 🔴 Very High | 4 | Howto parallel, duplicate checker, workflow docs, core recipes |
| 🟡 Medium | 10 | FFI verification, bundled headers, prompt updates, cookbook expansions |
| 🟢 Low | 20+ | Infrastructure improvements, test expansion, polish |

---

## Recommended Implementation Phases

### Phase 1: Critical (Week 1, 10-15 hours)
1. Fix howto parallel call stability
2. Implement duplicate definition checker
3. Add workflow chains to INSTRUCTIONS
4. Update write-gerbil-module and review-code prompts

**Expected Impact**: 70% reduction in common pain points

### Phase 2: High Priority (Week 2, 15-20 hours)
5. Add 10-15 core language cookbook recipes
6. Fix FFI module verification
7. Add bundled header support
8. Add negative guidance to INSTRUCTIONS

**Expected Impact**: 90% reduction in common pain points

### Phase 3: Polish (Weeks 3+, 35-55 hours)
9. Infrastructure improvements (REPL, caching, refactoring)
10. Implement remaining feature requests
11. Test suite expansion
12. Additional cookbook recipes and refinements

**Expected Impact**: 95%+ coverage of LLM fluency needs

---

## What's Already Excellent (No Changes Needed)

✅ 114 tools covering all major Gerbil operations  
✅ 471 cookbook recipes with verified working code  
✅ 426 comprehensive tests  
✅ 11 prompts for common workflows  
✅ 25+ resources (reference docs + dynamic stdlib)  
✅ Excellent documentation templates  
✅ Auto-loadpath detection  
✅ Security scanning  
✅ Minimal dependencies (MCP SDK + Zod)  

**Overall Project Grade**: **A** (Production-ready with minor improvements needed)

---

**Last Updated**: 2026-02-12  
**Next Action**: Implement Phase 1 critical items
