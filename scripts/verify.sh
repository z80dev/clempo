#!/usr/bin/env bash
# The gate. A rung counts only on a commit where this passes.
#   1. a cold compile of every layer from source; warnings are errors, and so is a false
#      compile-time bound proof (STATIC-ASSERT, DEFBOUND)
#   2. every layer's tests, each audited for threads it leaves running (*LONG-LIVED-THREADS*
#      aside) and, in this check build, for lock-order cycles (clempo.lock) and accesses that
#      break a shared slot's declared guard (clempo.guard)
#   3. fixed-seed fuzzing of every target plus tests/fuzz/cases.sexp (a few seconds)
#   4. the import-graph and determinism check (layers.sexp), including bound citations in
#      hot fixed-count loops, lock acquisition through clempo.lock, and declared guards for
#      shared structures and run-time-mutated globals
#   5. compiled-form specification matches tracked spec/ outputs
set -euo pipefail
cd "$(dirname "$0")/.."

SBCL=(sbcl --dynamic-space-size "${CLEMPO_HEAP:-8192}" --noinform --non-interactive --no-userinit --no-sysinit)

rm -rf build/fasl/check
"${SBCL[@]}" --load scripts/build.lisp \
  --eval '(clempo-build:load-all :strict t)' \
  --eval '(clempo-build:test-all :strict t)'

bin/clempo-dev fuzz -target all -seed 1 -cases 100

"${SBCL[@]}" --load scripts/build.lisp --load scripts/check-layers.lisp

bin/clempo-dev spec
if [[ -n "$(git status --porcelain=v1 --untracked-files=all -- spec/)" ]]; then
  echo "verify: generated spec/ differs from the tracked specification" >&2
  git diff -- spec/
  git diff --cached -- spec/
  git status --short --untracked-files=all -- spec/
  exit 1
fi

echo "verify: ok"
