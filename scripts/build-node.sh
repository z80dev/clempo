#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
export CLEMPO_BUILD=release
export CLEMPO_NODE_OUTPUT="${1:-$root/build/clempo}"
mkdir -p "$(dirname "$CLEMPO_NODE_OUTPUT")"
exec sbcl --dynamic-space-size "${CLEMPO_HEAP:-16384}" --noinform --non-interactive \
  --no-userinit --no-sysinit --load "$root/scripts/build.lisp" \
  --eval '(asdf:load-system "clempo-node")' \
  --eval '(sb-ext:save-lisp-and-die (clempo.io:getenv "CLEMPO_NODE_OUTPUT") :executable t :save-runtime-options t :toplevel (function clempo.node:main))'
