# t1c

## Site 1

- Source: [src/tempo/precompiles/dex.lisp](../../src/tempo/precompiles/dex.lisp)
- Top-level form: 112 (1-based)
- Nested form path: `(4 2 4 7 2)`
- Definition: `clempo.dex::place-flip`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.dex::place-flip`

skip a redundant next-order-id load

## Site 2

- Source: [src/tempo/precompiles/fee-amm.lisp](../../src/tempo/precompiles/fee-amm.lisp)
- Top-level form: 20 (1-based)
- Nested form path: `(5 4)`
- Definition: `clempo.fee-manager:rebalance-swap`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.fee-manager:rebalance-swap`

## Site 3

- Source: [src/tempo/precompiles/fee-amm.lisp](../../src/tempo/precompiles/fee-amm.lisp)
- Top-level form: 23 (1-based)
- Nested form path: `(9 3 2 3 3)`
- Definition: `clempo.fee-manager:burn`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.fee-manager:burn`

## Site 4

- Source: [src/tempo/precompiles/fee-amm.lisp](../../src/tempo/precompiles/fee-amm.lisp)
- Top-level form: 3 (1-based)
- Nested form path: `(9)`
- Definition: `clempo.fee-manager:fee-manager`
- Kind: `:field`
- Relation: `:since`
- Where: `(clempo.fee-manager:fee-manager clempo.fee-manager::pending-fee-swap-reservation)`

Transient only (t-read/t-write): validator-token liquidity reserved by collect-fee-pre-tx.

## Site 5

- Source: [src/tempo/precompiles/fee-manager.lisp](../../src/tempo/precompiles/fee-manager.lisp)
- Top-level form: 22 (1-based)
- Nested form path: `(3 2 1)`
- Definition: `clempo.fee-manager::reserve-fee-liquidity`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.fee-manager::reserve-fee-liquidity`

## Site 6

- Source: [src/tempo/precompiles/tip20.lisp](../../src/tempo/precompiles/tip20.lisp)
- Top-level form: 128 (1-based)
- Nested form path: `(4 2)`
- Definition: `clempo.tip20:transfer-fee-post-tx`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.tip20:transfer-fee-post-tx`

