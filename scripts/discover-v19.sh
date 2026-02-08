#!/usr/bin/env bash
# discover-v19.sh — Discover v0.19 Gerbil patterns and save as cookbook recipes
#
# Usage:
#   ./discover-v19.sh                  # analyze NEW commits since last run
#   ./discover-v19.sh -n 5             # analyze last 5 commits (ignores watcher)
#   ./discover-v19.sh --all            # analyze all commits (ignores watcher)
#   ./discover-v19.sh -m std/iter      # focus on a specific module
#   ./discover-v19.sh -f src/std/io    # focus on files under a path
#   ./discover-v19.sh --since 2025-01-01  # commits since date
#   ./discover-v19.sh --dry-run        # show the prompt but don't run claude
#   ./discover-v19.sh --model haiku    # use a cheaper model for exploration
#
# State is tracked in ~/.gerbil-watcher.json. After each successful run,
# the current HEAD commit is saved so the next run only processes new commits.
#
# Requires: claude CLI, git
# MCP: Expects gerbil MCP server configured in ~/.claude/mcp.json

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MCP_REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

GERBIL_DIR="${GERBIL_DIR:-$HOME/mine/gerbil}"
BRANCH="v0.19-dev-test-basis"
BASE_BRANCH="master"
NUM_COMMITS=""
MODULE_FOCUS=""
FILE_FOCUS=""
SINCE=""
DRY_RUN=false
MODEL="sonnet"
MAX_BUDGET=""
USE_ALL=false
WATCHER_FILE="$HOME/.gerbil-watcher.json"

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
    --all)
      USE_ALL=true; shift ;;
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

# Read the current HEAD commit
HEAD_COMMIT=$(git rev-parse HEAD)
HEAD_SHORT=$(git rev-parse --short HEAD)

# Read last-checked commit from watcher file
LAST_COMMIT=""
if [[ -f "$WATCHER_FILE" ]]; then
  LAST_COMMIT=$(python3 -c "
import json, sys
try:
  d = json.load(open('$WATCHER_FILE'))
  print(d.get('last_commit', ''))
except: pass
" 2>/dev/null || true)
fi

# Determine commit range mode:
#   --since / -n / --all  => explicit override, ignore watcher
#   otherwise             => use watcher state (new commits only)
COMMIT_RANGE=""
if [[ -n "$SINCE" ]]; then
  # Explicit --since flag
  log_args=(--oneline --since "$SINCE")
elif [[ -n "$NUM_COMMITS" ]]; then
  # Explicit -n flag
  log_args=(--oneline -n "$NUM_COMMITS")
elif $USE_ALL; then
  # --all flag: show everything on branch vs base
  log_args=(--oneline)
  COMMIT_RANGE="${BASE_BRANCH}..${BRANCH}"
elif [[ -n "$LAST_COMMIT" ]] && git cat-file -e "$LAST_COMMIT" 2>/dev/null; then
  # Watcher mode: only new commits since last run
  if [[ "$LAST_COMMIT" == "$HEAD_COMMIT" ]]; then
    echo "No new commits since last run (HEAD=$HEAD_SHORT)."
    echo "Use --all or -n N to re-analyze."
    exit 0
  fi
  COMMIT_RANGE="${LAST_COMMIT}..HEAD"
  log_args=(--oneline)
  echo "=== New commits since last run (${LAST_COMMIT:0:7}..${HEAD_SHORT}) ==="
else
  # First run or invalid watcher state — default to last 10
  log_args=(--oneline -n 10)
fi

# Get the commit range info
if [[ -z "$COMMIT_RANGE" ]]; then
  echo "=== Recent commits on $BRANCH ==="
  git log "${log_args[@]}"
else
  if [[ "$COMMIT_RANGE" != "${LAST_COMMIT}..HEAD" ]]; then
    echo "=== Commits on $BRANCH (vs $BASE_BRANCH) ==="
  fi
  git log "${log_args[@]}" "$COMMIT_RANGE"
fi
echo ""

# Get the diff summary, excluding bootstrap
diff_filter=""
if [[ -n "$FILE_FOCUS" ]]; then
  diff_filter="-- $FILE_FOCUS"
fi

DIFF_RANGE="${COMMIT_RANGE:-${BASE_BRANCH}..${BRANCH}}"
echo "=== Changed source files (non-bootstrap) [$DIFF_RANGE] ==="
eval git diff --stat "$DIFF_RANGE" -- \
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
DIFF RANGE: $DIFF_RANGE
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
- git diff ${DIFF_RANGE} -- src/std/MODULE.ss  (see specific file changes)
- git diff ${DIFF_RANGE} --name-only -- 'src/std/' ':(exclude)src/bootstrap/'  (list changed files)
- git log --oneline ${DIFF_RANGE} -- src/std/MODULE.ss  (commits touching a file)
PROMPT_EOF
)"

if $DRY_RUN; then
  echo "=== PROMPT (dry run) ==="
  echo "$prompt"
  echo ""
  echo "=== Watcher state ==="
  echo "HEAD: $HEAD_COMMIT"
  echo "Last: ${LAST_COMMIT:-<none>}"
  echo "File: $WATCHER_FILE"
  exit 0
fi

echo "=== Running claude -p (model: $MODEL) ==="

claude_args=(
  -p "$prompt"
  --model "$MODEL"
  --verbose
  --output-format stream-json
  --allowedTools "Bash(git:*),Bash(ls:*),Read,Glob,Grep,mcp__gerbil__*"
)
if [[ -n "$MAX_BUDGET" ]]; then
  claude_args+=(--max-budget-usd "$MAX_BUDGET")
fi

# Save watcher state after successful run
update_watcher() {
  local timestamp
  timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
  python3 -c "
import json, os
path = '$WATCHER_FILE'
data = {}
if os.path.exists(path):
    try:
        with open(path) as f:
            data = json.load(f)
    except: pass
data['last_commit'] = '$HEAD_COMMIT'
data['last_branch'] = '$BRANCH'
data['last_run'] = '$timestamp'
data['gerbil_dir'] = '$GERBIL_DIR'
with open(path, 'w') as f:
    json.dump(data, f, indent=2)
    f.write('\n')
"
  echo ""
  echo "=== Watcher updated ==="
  echo "Saved HEAD=$HEAD_SHORT to $WATCHER_FILE"
}

# Commit and push cookbooks.json if it has changes
commit_cookbooks() {
  local cookbook="$MCP_REPO_DIR/cookbooks.json"
  if [[ ! -f "$cookbook" ]]; then
    return
  fi
  # Check for uncommitted changes to cookbooks.json
  if git -C "$MCP_REPO_DIR" diff --quiet -- cookbooks.json && \
     git -C "$MCP_REPO_DIR" diff --cached --quiet -- cookbooks.json; then
    echo "=== cookbooks.json unchanged — nothing to commit ==="
    return
  fi
  echo "=== Committing cookbooks.json ==="
  git -C "$MCP_REPO_DIR" add cookbooks.json
  git -C "$MCP_REPO_DIR" commit -m "Update cookbooks.json with v0.19 discoveries (${HEAD_SHORT})"
  echo "=== Pushing to remote ==="
  git -C "$MCP_REPO_DIR" push
  echo "=== cookbooks.json committed and pushed ==="
}

# Stream output and show progress: tool calls, results, and assistant text
# Disable pipefail temporarily so we can capture PIPESTATUS reliably
set +o pipefail
claude "${claude_args[@]}" | while IFS= read -r line; do
  type=$(echo "$line" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('type',''))" 2>/dev/null || true)
  case "$type" in
    assistant)
      # Show assistant text as it arrives
      text=$(echo "$line" | python3 -c "
import sys,json
d=json.load(sys.stdin)
for c in d.get('message',{}).get('content',[]):
  if c.get('type')=='text': print(c['text'])
" 2>/dev/null || true)
      if [[ -n "$text" ]]; then
        echo "$text"
      fi
      ;;
    tool_use)
      # Show which tool is being called
      tool=$(echo "$line" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('tool',''))" 2>/dev/null || true)
      input_preview=$(echo "$line" | python3 -c "
import sys,json
d=json.load(sys.stdin)
inp=d.get('input',{})
# Show a short preview of the input
parts=[]
for k,v in list(inp.items())[:3]:
  s=str(v)
  if len(s)>60: s=s[:60]+'...'
  parts.append(f'{k}={s}')
print(', '.join(parts))
" 2>/dev/null || true)
      echo "  >> [tool] $tool($input_preview)"
      ;;
    tool_result)
      # Show a brief snippet of the result
      content=$(echo "$line" | python3 -c "
import sys,json
d=json.load(sys.stdin)
t=d.get('content','')
if isinstance(t,list):
  t=' '.join(c.get('text','') for c in t if isinstance(c,dict))
t=str(t).strip()
if len(t)>120: t=t[:120]+'...'
print(t)
" 2>/dev/null || true)
      is_error=$(echo "$line" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('is_error','false'))" 2>/dev/null || true)
      if [[ "$is_error" == "true" || "$is_error" == "True" ]]; then
        echo "  << [result] ERROR: $content"
      else
        echo "  << [result] $content"
      fi
      ;;
    result)
      # Final result — show full text
      text=$(echo "$line" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('result',''))" 2>/dev/null || true)
      if [[ -n "$text" ]]; then
        echo ""
        echo "=== Final Result ==="
        echo "$text"
      fi

      # Show cost info if present
      cost=$(echo "$line" | python3 -c "
import sys,json
d=json.load(sys.stdin)
c=d.get('cost_usd')
if c: print(f'Cost: \${c:.4f}')
" 2>/dev/null || true)
      if [[ -n "$cost" ]]; then
        echo "$cost"
      fi
      ;;
  esac
done
claude_exit=${PIPESTATUS[0]}
set -o pipefail

# Update watcher state and commit cookbooks after successful run
if [[ $claude_exit -eq 0 ]]; then
  update_watcher
  commit_cookbooks
else
  echo ""
  echo "=== claude exited with error (code $claude_exit) — watcher NOT updated ==="
fi
