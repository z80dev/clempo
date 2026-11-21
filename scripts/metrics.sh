#!/usr/bin/env bash
# Print clempo's headline numbers: code size per layer, tests, and the latest score of
# every rung from FRONTIER.jsonl. Reads only the checkout; no network.
set -euo pipefail
cd "$(dirname "$0")/.."

lines() { find "$@" -name '*.lisp' -print0 2>/dev/null | xargs -0 cat 2>/dev/null | wc -l | tr -d ' '; }

echo "commit   $(git rev-parse --short HEAD)"
echo "src      $(lines src) lines of Lisp in $(find src -name '*.lisp' | wc -l | tr -d ' ') files"
echo "tests    $(lines tests) lines; $(grep -rh '^(deftest ' tests | wc -l | tr -d ' ') deftest forms"
echo
echo "layer        lines"
for d in $(sed -n 's/.*(:dir "\([a-z0-9]*\)".*/\1/p' layers.sexp); do
  [ -d "src/$d" ] && printf "%-12s %6s\n" "$d" "$(lines "src/$d")"
done
echo
echo "rung       latest recorded score (FRONTIER.jsonl)"
jq -r -s 'map(select(.score != null)) | group_by(.rung) | map(last) | .[] | "\(.rung)\t\(.score)"' FRONTIER.jsonl |
  awk -F'\t' '{printf "%-10s %s\n", $1, $2}'
