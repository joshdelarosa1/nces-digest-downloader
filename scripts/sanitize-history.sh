#!/usr/bin/env bash
set -euo pipefail

# Scaffold only: review before running in a clean clone.
if ! command -v git-filter-repo >/dev/null 2>&1; then
  echo "git-filter-repo is required. Install it first." >&2
  exit 1
fi

PATTERN_FILE="scripts/secrets-patterns.txt"
if [ ! -f "${PATTERN_FILE}" ]; then
  echo "Missing ${PATTERN_FILE}" >&2
  exit 1
fi

cat <<'MSG'
This rewrites Git history and changes commit SHAs.
Run only after rotating/revoking exposed credentials.
Suggested dry-run workflow:
  1) Mirror clone the repository.
  2) Review scripts/secrets-patterns.txt.
  3) Run git filter-repo with --replace-text.
  4) Force-push only after team approval.
MSG

echo "Example command:"
echo "git filter-repo --replace-text scripts/secrets-patterns.txt"
