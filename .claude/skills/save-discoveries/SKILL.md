---
name: save-discoveries
description: Save Gerbil discoveries as cookbook recipes, suggest tooling improvements, and add security patterns
---

Review what was learned during this session and save it in three ways:

## Step 1: Save Cookbook Recipes

For each non-trivial Gerbil pattern, workaround, or API discovery made during this session:

1. Call `gerbil_howto` with relevant keywords to check if a recipe already exists
2. If not found, call `gerbil_howto_add` with:
   - `id`: kebab-case identifier
   - `title`: human-readable title
   - `tags`: 4-6 search keywords (module name, task, alternative phrasings)
   - `imports`: all required imports (use `[]` if none)
   - `code`: complete, copy-pasteable working example
   - `notes`: gotchas, alternatives, or non-obvious details

Skip trivial one-liners and project-specific business logic.

## Step 2: Suggest Tooling Improvements

For each workflow friction point or missing tool capability noticed during this session:

1. Call `gerbil_list_features` with relevant keywords to check for duplicates
2. If a matching feature already exists, call `gerbil_vote_feature` with its `id` to upvote it
3. If not found, call `gerbil_suggest_feature` with:
   - `id`: kebab-case identifier
   - `title`: short description
   - `description`: detailed explanation
   - `impact`: `high` (saves many tokens regularly), `medium` (useful but less frequent), or `low` (nice-to-have)
   - `tags`: 3-5 search keywords
   - `use_case`: when this would be useful
   - `example_scenario`: concrete example of the problem
   - `estimated_token_reduction`: specific estimate (e.g. "~500 tokens per invocation", "eliminates 3 tool calls")

## Step 3: Add Security Patterns

For each new vulnerability pattern, unsafe coding practice, or FFI misuse discovered during this session:

1. Run `gerbil_security_scan` on relevant files to check if the pattern is already detected
2. If not already covered, call `gerbil_security_pattern_add` with:
   - `id`: kebab-case identifier (e.g., `"shell-injection-format-string"`)
   - `title`: human-readable title
   - `severity`: `critical`, `high`, `medium`, or `low`
   - `scope`: `scheme` (`.ss` files), `c-shim` (`.c`/`.h` files), or `ffi-boundary` (`.ss` `c-lambda`)
   - `pattern`: regex to detect the vulnerability in source lines
   - `message`: explanation of the vulnerability
   - `remediation`: how to fix the issue
   - Optional `tags`: search keywords for discoverability
   - Optional `related_recipe`: cookbook recipe ID with the safe alternative

Skip patterns that are too project-specific to generalize.

## What to Look For

Recipes to save:
- Correct imports/function names that required experimentation
- Workarounds for Gerbil quirks or undocumented behavior
- Multi-step patterns combining standard library functions
- Arity or signature discoveries that weren't obvious

Features to suggest or vote for:
- Multiple sequential tool calls that could be a single tool
- Missing tool parameters or modes
- Workflows that forced fallback to `gerbil_eval` or bash
- Repeated cross-session patterns that could be automated
- If the friction point matches an existing feature suggestion, vote for it instead of creating a duplicate

Security patterns to add:
- New vulnerability patterns found during code review or debugging
- Unsafe FFI calling conventions (e.g., missing null checks, type mismatches)
- Shell injection vectors via string concatenation or `open-process`
- Resource leaks (ports, file descriptors, mutexes) without `unwind-protect`
- Unsafe C shim patterns (buffer overflows, static globals, missing error checks)

Report what was saved, suggested, voted for, and security patterns added when done.
