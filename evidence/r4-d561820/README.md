# R4 live-tip hour, `d561820` (Linux x86-64, 2026-11-21)

Raw output of the passing R4 run that FRONTIER.md's R4 row reports.

- `r4.json`: the run's result (score, node summary, RPC sampling summary).
- `command`: the node command line.
- `node.log`: the node's log.
- `probe.jsonl`: the independent RPC probe, one line per attempt every 5 s: clempo's
  head, the upstream head, the lag, whether clempo's block is canonical upstream and
  whether its state answer matched. `r4.json`'s `rpc` summary is computed over the attempts
  after catch-up (`catchupHead`).

Edits from the files the run wrote: the run directory's absolute path is replaced by
`$RUN`, and `probe.jsonl` drops the connection-refused attempts before the node's RPC came
up and after it stopped (the 922 lines from the first answered attempt to the last are
unchanged; one of them is a `timed out` attempt). SHA-256 of the originals:

```
8bd68761b02a6d9a9b07c33d52de8d0516111440e307bac191bead06aa24416c  r4.json
ed6ee52a0d7b172fb28cd92eecbcccb18a61a51aebab336ef183b0435eb08131  rpc.jsonl (probe.jsonl before trimming)
3b817773b0a38c00af595f9dbebd5aae39db266b72ea697059ff9c747471bc6b  node.log
adbb0af693e50b298ec7cd62262dc862402e676607396afea7e3c9db68af5e0a  command
```
