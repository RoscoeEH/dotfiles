#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $(basename "$0") <branch-name>" >&2
    exit 1
fi

branch="$1"

# Find the repository's main worktree
repo_root="$(git rev-parse --path-format=absolute --show-toplevel)"
main_worktree="$(git worktree list --porcelain | sed -n '1s/^worktree //p')"

# Put all worktrees alongside the main repository
repo_name="$(basename "$main_worktree")"
worktree_dir="$(dirname "$main_worktree")/${repo_name}_worktrees"
target="${worktree_dir}/${branch}"

# Validate branch name
git check-ref-format --branch "$branch" >/dev/null

if [[ -e "$target" ]]; then
    echo "Worktree directory already exists: $target" >&2
    exit 1
fi

mkdir -p "$(dirname "$target")"

git -C "$repo_root" worktree add -b "$branch" "$target" master

echo "Created worktree: $target"
