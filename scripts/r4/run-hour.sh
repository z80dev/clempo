#!/usr/bin/env bash
set -euo pipefail
[[ $# -eq 2 ]] || { echo "usage: $0 STOPPED_UNHEALED_DATADIR FRESH_RUN_DIR" >&2; exit 2; }
source_state="$(cd "$1" && pwd -P)"
[[ -f "$source_state/MANIFEST" ]] || { echo "Missing source MANIFEST" >&2; exit 2; }
mkdir "$2" # refuses to reuse a run directory
run="$(cd "$2" && pwd -P)"
runner="$(cd "$(dirname "$0")" && pwd -P)"
upstream="${CLEMPO_RPC:-http://127.0.0.1:8545}"
cert_rpc="${CLEMPO_CERT_RPC:-https://rpc.tempo.xyz}"
port="${CLEMPO_R4_PORT:-18545}"
seconds="${CLEMPO_R4_SECONDS:-3840}" # allow time to sample a full hour after catchup
archive="$(cd "${CLEMPO_MAINNET_ARCHIVE:-$HOME/cache/clempo/mainnet}" && pwd -P)"
if [[ $(uname -s) == Darwin ]]; then
    cp -cR "$source_state" "$run/state"
else
    cp -R --reflink=auto "$source_state" "$run/state"
fi
mkdir "$run/archive"
shopt -s nullglob
for chunk in "$archive"/chunk-*.blk; do
    ln -s "$chunk" "$run/archive/${chunk##*/}"
done
cd "$runner/../.."
git rev-parse HEAD > "$run/revision"
scripts/build-node.sh "$run/clempo" > "$run/build.log" 2>&1
args=(--datadir "$run/state" --upstream "$upstream" --archive "$run/archive" --rpc-port "$port" --bind 127.0.0.1 --certify --cert-rpc "$cert_rpc" --seconds "$seconds")
{ printf '%q ' "$run/clempo" "${args[@]}"; printf '\n'; } > "$run/command"
node_pid= probe_pid=
stop_probe() {
    if [[ -n "$probe_pid" ]]; then
        kill "$probe_pid" 2>/dev/null || true
        wait "$probe_pid" || true
        probe_pid=
    fi
}
cleanup() {
    stop_probe
    if [[ -n "$node_pid" ]]; then
        kill -TERM "$node_pid" 2>/dev/null || true
        wait "$node_pid" || true
    fi
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
"$run/clempo" "${args[@]}" > "$run/node.json" 2> "$run/node.log" &
node_pid=$!
python3 "$runner/probe.py" "http://127.0.0.1:$port" "$upstream" > "$run/rpc.jsonl" &
probe_pid=$!
status=0
wait "$node_pid" || status=$?
node_pid=
stop_probe
if [[ "$status" != 0 ]]; then
    echo "R4 node failed (status $status); see $run/node.log" >&2
    exit 2
fi
python3 "$runner/result.py" "$run"
