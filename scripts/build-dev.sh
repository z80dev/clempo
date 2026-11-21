#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
export CLEMPO_BUILD=release
export CLEMPO_DEV_OUTPUT="${1:-$root/build/clempo-dev}"
mkdir -p "$(dirname "$CLEMPO_DEV_OUTPUT")"
exec sbcl --dynamic-space-size "${CLEMPO_HEAP:-16384}" --noinform --non-interactive \
  --no-userinit --no-sysinit --load "$root/scripts/build.lisp" \
  --eval '(asdf:load-system "clempo-dev")' \
  --eval '(clempo.chainspec:load-chainspec :mainnet)' \
  --eval '(sb-ext:save-lisp-and-die (clempo.io:getenv "CLEMPO_DEV_OUTPUT") :compression nil :executable t :save-runtime-options t :toplevel (lambda () (clempo.dev:main (rest sb-ext:*posix-argv*))))'
