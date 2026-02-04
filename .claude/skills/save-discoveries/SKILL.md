---
name: save-discoveries
description: Save Gerbil discoveries as cookbook recipes and suggest tooling improvements
---

Review what was learned during this session and save it in two ways:

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
2. If not found, call `gerbil_suggest_feature` with:
   - `id`: kebab-case identifier
   - `title`: short description
   - `description`: detailed explanation
   - `impact`: `high` (saves many tokens regularly), `medium` (useful but less frequent), or `low` (nice-to-have)
   - `tags`: 3-5 search keywords
   - `use_case`: when this would be useful
   - `example_scenario`: concrete example of the problem
   - `estimated_token_reduction`: specific estimate (e.g. "~500 tokens per invocation", "eliminates 3 tool calls")

## What to Look For

Recipes to save:
- Correct imports/function names that required experimentation
- Workarounds for Gerbil quirks or undocumented behavior
- Multi-step patterns combining standard library functions
- Arity or signature discoveries that weren't obvious

Features to suggest:
- Multiple sequential tool calls that could be a single tool
- Missing tool parameters or modes
- Workflows that forced fallback to `gerbil_eval` or bash
- Repeated cross-session patterns that could be automated

Report what was saved and suggested when done.
