#!/usr/bin/env bash
# The gate. A rung counts only on a commit where this passes.
#   1. a cold compile of every layer from source; warnings are errors
#   2. every layer's tests
#   3. the import-graph and determinism check (layers.sexp)
set -euo pipefail
cd "$(dirname "$0")/.."

SBCL=(sbcl --dynamic-space-size "${CLEMPO_HEAP:-8192}" --noinform --non-interactive --no-userinit --no-sysinit)

rm -rf build/fasl/check
"${SBCL[@]}" --load scripts/build.lisp \
  --eval '(clempo-build:load-all :force t :strict t)' \
  --eval '(clempo-build:test-all :strict t)'

"${SBCL[@]}" --load scripts/build.lisp --load scripts/check-layers.lisp

echo "verify: ok"
