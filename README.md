# clempo

clempo is a Tempo node written in Common Lisp (SBCL). Tempo's reference node is written in Rust ([tempoxyz/tempo](https://github.com/tempoxyz/tempo)); zempo (Zig) and [goempo](https://github.com/gakonst/goempo) (Go) came before it. clempo climbs the same ladder ([GOAL.md](GOAL.md)) and tries to win where Lisp is different:

- **Declarative protocol.** Opcodes, hardfork gates, wire shapes and precompile storage layouts are forms; macros turn them into code, tests and documentation.
- **Resumable divergences.** A replay that disagrees with mainnet stops in a condition with restarts, not a crash.
- **One image.** The node, the tools and the REPL share one running Lisp.
- **No borrowed code.** clempo implements the EVM, the trie, RLP, the crypto and Tempo's precompiles itself, using only SBCL.

Status: [FRONTIER.md](FRONTIER.md). Rules for contributors (human or agent): [AGENTS.md](AGENTS.md).

```sh
scripts/verify.sh        # the gate
bin/clempo-dev help      # tools
```

## License

Dual-licensed under either of MIT or Apache-2.0, at your option.
