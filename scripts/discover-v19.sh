#!/usr/bin/env bash
# discover-v19.sh — Discover v0.19 Gerbil patterns and save as cookbook recipes
#
# Usage:
#   ./discover-v19.sh                  # analyze last 10 non-bootstrap commits
#   ./discover-v19.sh -n 5             # analyze last 5 commits
#   ./discover-v19.sh -m std/iter      # focus on a specific module
#   ./discover-v19.sh -f src/std/io    # focus on files under a path
#   ./discover-v19.sh --since 2025-01-01  # commits since date
#   ./discover-v19.sh --dry-run        # show the prompt but don't run claude
#   ./discover-v19.sh --model haiku    # use a cheaper model for exploration
#
# Requires: claude CLI, git
# MCP: Expects gerbil MCP server configured in ~/.claude/mcp.json

set -euo pipefail

GERBIL_DIR="${GERBIL_DIR:-$HOME/mine/gerbil}"
BRANCH="v0.19-dev-test-basis"
BASE_BRANCH="master"
NUM_COMMITS=10
MODULE_FOCUS=""
FILE_FOCUS=""
SINCE=""
DRY_RUN=false
MODEL="sonnet"
MAX_BUDGET=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -n|--num-commits)
      NUM_COMMITS="$2"; shift 2 ;;
    -m|--module)
      MODULE_FOCUS="$2"; shift 2 ;;
    -f|--file-focus)
      FILE_FOCUS="$2"; shift 2 ;;
    --since)
      SINCE="$2"; shift 2 ;;
    --model)
      MODEL="$2"; shift 2 ;;
    --max-budget)
      MAX_BUDGET="$2"; shift 2 ;;
    --dry-run)
      DRY_RUN=true; shift ;;
    -h|--help)
      sed -n '2,/^$/p' "$0" | sed 's/^# \?//'
      exit 0 ;;
    *)
      echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

cd "$GERBIL_DIR"

# Ensure we're on the right branch
current_branch=$(git branch --show-current)
if [[ "$current_branch" != "$BRANCH" ]]; then
  echo "Switching to $BRANCH..."
  git checkout "$BRANCH"
fi

git pull --ff-only 2>/dev/null || true

# Build the git log command for recent changes
log_args=(--oneline)
if [[ -n "$SINCE" ]]; then
  log_args+=(--since "$SINCE")
else
  log_args+=(-n "$NUM_COMMITS")
fi

# Get the commit range info
echo "=== Recent commits on $BRANCH ==="
git log "${log_args[@]}"
echo ""

# Get the diff summary, excluding bootstrap
diff_filter=""
if [[ -n "$FILE_FOCUS" ]]; then
  diff_filter="-- $FILE_FOCUS"
fi

echo "=== Changed source files (non-bootstrap) ==="
eval git diff --stat "${BASE_BRANCH}..${BRANCH}" -- \
  "'src/std/*.ss'" "'src/std/**/*.ss'" \
  "'src/gerbil/*.ss'" "'src/gerbil/**/*.ss'" \
  $diff_filter \
  "':(exclude)src/bootstrap/*'" | tail -20
echo ""

# Build the module focus clause
module_clause=""
if [[ -n "$MODULE_FOCUS" ]]; then
  module_clause="
FOCUS MODULE: $MODULE_FOCUS
Pay special attention to this module. Check its exports, signatures, and any API changes."
fi

file_clause=""
if [[ -n "$FILE_FOCUS" ]]; then
  file_clause="
FOCUS PATH: $FILE_FOCUS
Concentrate on files under this path. Read the changed files and understand what's new."
fi

# Build the prompt
prompt="$(cat <<PROMPT_EOF
You are analyzing Gerbil Scheme v0.19 changes to discover new patterns and API updates for the cookbook.

WORKING DIRECTORY: $GERBIL_DIR
BRANCH: $BRANCH (compared against $BASE_BRANCH)
${module_clause}${file_clause}

## Your Task

1. **Understand what changed**: Look at recent git diffs for non-bootstrap source files (.ss files under src/std/ and src/gerbil/, excluding src/bootstrap/). Focus on:
   - New modules or files added
   - Changed function signatures (renamed, new parameters, removed parameters)
   - New exports from existing modules
   - Moved modules (e.g., from v0.19-TODO/ to their final location)
   - New syntax forms or macros
   - Changed idioms (old way vs new way)

2. **Test with MCP tools**: For each interesting change you find:
   - Use gerbil_module_exports to see what the module exports now
   - Use gerbil_function_signature to check new/changed signatures
   - Use gerbil_eval to test if the new API works (this runs against whatever gerbil is installed, so some v0.19 things may not be testable yet — that's OK, just note it)
   - Use gerbil_check_syntax to verify code examples

3. **Check existing cookbook**: Before saving anything, use gerbil_howto to search for existing recipes that might need updating. Look for recipes tagged with older versions that now have different APIs in v0.19.

4. **Save discoveries**: For each new or changed pattern:
   - If it's a NEW pattern: use gerbil_howto_add with gerbil_version: "v0.19"
   - If it UPDATES an existing recipe: use gerbil_howto_add with the same id (update semantics) or a new id with supersedes pointing to the old one
   - Include complete working code examples with correct imports
   - Note what changed from v0.18 in the notes field

5. **Report what you found**: At the end, summarize:
   - New modules discovered
   - Changed APIs (old vs new)
   - Recipes added or updated
   - Things that couldn't be tested yet (need v0.19 runtime)

## Guidelines

- Skip bootstrap files (compiled artifacts) — focus on .ss source files
- Skip trivial changes (whitespace, comments)
- Focus on user-facing API changes, not compiler internals
- Tag all recipes with gerbil_version: "v0.19"
- If a module moved from v0.19-TODO/ to its final location, that's worth noting
- If you find a pattern that works differently in v0.19, use supersedes to deprecate the v0.18 recipe

## Git commands to help you explore

Run these via bash to understand changes:
- git diff ${BASE_BRANCH}..${BRANCH} -- src/std/MODULE.ss  (see specific file changes)
- git diff ${BASE_BRANCH}..${BRANCH} --name-only -- 'src/std/' ':(exclude)src/bootstrap/'  (list changed files)
- git log --oneline ${BASE_BRANCH}..${BRANCH} -- src/std/MODULE.ss  (commits touching a file)
PROMPT_EOF
)"

if $DRY_RUN; then
  echo "=== PROMPT (dry run) ==="
  echo "$prompt"
  exit 0
fi

echo "=== Running claude -p (model: $MODEL) ==="

claude_args=(
  -p "$prompt"
  --model "$MODEL"
  --allowedTools "Bash(git:*),Bash(ls:*),Read,Glob,Grep,mcp__gerbil__*"
)
if [[ -n "$MAX_BUDGET" ]]; then
  claude_args+=(--max-budget-usd "$MAX_BUDGET")
fi

claude "${claude_args[@]}"
