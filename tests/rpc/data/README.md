`number-params.jsonl`: JSON numbers (fractions, exponents, `-0`, values beyond u64 and
i64, beyond the double range) given to parameters of 16 method shapes, one request per
line with the reference's answer. Recorded on 2026-11-21 over HTTP from the local Tempo
mainnet reference node (`tempo node`, `~/dev/tempo` at 568b4fb425, `http://127.0.0.1:8545`).
Only the requests it answered with -32602 Invalid params are kept, so the answers do not
depend on chain state.
