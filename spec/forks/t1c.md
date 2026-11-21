# t1c

## Site 1

- Source: [src/rpc/calls.lisp](../../src/rpc/calls.lisp)
- Top-level form: 70 (1-based)
- Nested form path: `(4 2 2 1 1 1)`
- Definition: `clempo.rpc.calls::mock-signature`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.rpc.calls::mock-signature`

## Site 2

- Source: [src/tempo/executor/aa.lisp](../../src/tempo/executor/aa.lisp)
- Top-level form: 24 (1-based)
- Nested form path: `(4 5 2 1 1 1)`
- Definition: `clempo.executor::execute-aa-transaction`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.executor::execute-aa-transaction`

## Site 3

- Source: [src/tempo/executor/aa.lisp](../../src/tempo/executor/aa.lisp)
- Top-level form: 24 (1-based)
- Nested form path: `(4 5 2 1 2 1)`
- Definition: `clempo.executor::execute-aa-transaction`
- Kind: `:branch`
- Relation: `:before`
- Where: `clempo.executor::execute-aa-transaction`

## Site 4

- Source: [src/tempo/executor/evm-glue.lisp](../../src/tempo/executor/evm-glue.lisp)
- Top-level form: 35 (1-based)
- Nested form path: `(4 2 2 2 1)`
- Definition: `clempo.executor:tempo-evm-spec`
- Kind: `:branch`
- Relation: `:before`
- Where: `clempo.executor:tempo-evm-spec`

## Site 5

- Source: [src/tempo/precompiles/dex.lisp](../../src/tempo/precompiles/dex.lisp)
- Top-level form: 112 (1-based)
- Nested form path: `(4 2 4 7 2)`
- Definition: `clempo.dex::place-flip`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.dex::place-flip`

skip a redundant next-order-id load

## Site 6

- Source: [src/tempo/precompiles/dex.lisp](../../src/tempo/precompiles/dex.lisp)
- Top-level form: 112 (1-based)
- Nested form path: `common-lisp:nil`
- Definition: `clempo.dex::place-flip`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.dex::place-flip`

## Site 7

- Source: [src/tempo/precompiles/dex.lisp](../../src/tempo/precompiles/dex.lisp)
- Top-level form: 113 (1-based)
- Nested form path: `common-lisp:nil`
- Definition: `clempo.dex::flip-in-place`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.dex::flip-in-place`

## Site 8

- Source: [src/tempo/precompiles/fee-amm.lisp](../../src/tempo/precompiles/fee-amm.lisp)
- Top-level form: 20 (1-based)
- Nested form path: `(5 4)`
- Definition: `clempo.fee-manager:rebalance-swap`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.fee-manager:rebalance-swap`

## Site 9

- Source: [src/tempo/precompiles/fee-amm.lisp](../../src/tempo/precompiles/fee-amm.lisp)
- Top-level form: 23 (1-based)
- Nested form path: `(9 3 2 3 3)`
- Definition: `clempo.fee-manager:burn`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.fee-manager:burn`

## Site 10

- Source: [src/tempo/precompiles/fee-amm.lisp](../../src/tempo/precompiles/fee-amm.lisp)
- Top-level form: 3 (1-based)
- Nested form path: `(9)`
- Definition: `clempo.fee-manager:fee-manager`
- Kind: `:field`
- Relation: `:since`
- Where: `(clempo.fee-manager:fee-manager clempo.fee-manager::pending-fee-swap-reservation)`

Transient only (t-read/t-write): validator-token liquidity reserved by collect-fee-pre-tx.

## Site 11

- Source: [src/tempo/precompiles/fee-manager.lisp](../../src/tempo/precompiles/fee-manager.lisp)
- Top-level form: 22 (1-based)
- Nested form path: `(3 2 1)`
- Definition: `clempo.fee-manager::reserve-fee-liquidity`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.fee-manager::reserve-fee-liquidity`

## Site 12

- Source: [src/tempo/precompiles/tip20.lisp](../../src/tempo/precompiles/tip20.lisp)
- Top-level form: 128 (1-based)
- Nested form path: `(4 2)`
- Definition: `clempo.tip20:transfer-fee-post-tx`
- Kind: `:branch`
- Relation: `:since`
- Where: `clempo.tip20:transfer-fee-post-tx`

