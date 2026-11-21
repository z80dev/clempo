# defcontract: precompile storage as data

Tempo's precompiles keep Solidity-compatible storage. The Rust reference declares each layout as a struct and the `#[contract]` proc macro (`crates/precompiles-macros`) computes slots. clempo declares the same layout as a form; `defcontract` computes the slot table at macroexpansion time, keeps it as data on the symbol, and generates accessors. Gas depends on the exact sequence of storage operations, so no accessor may hide, reorder or merge an SLOAD or SSTORE.

## Target: TIP20Token

The local [solc fixtures](../tests/tempo/contract/data/layouts/README.md) include `tip20.layout.json`; the layout tests compare the generated slot assignments with those fixtures.

| Field | Type | Slot | Offset | Access |
|---|---|---|---|---|
| roles | mapping address → (mapping b256 → bool) | 0 | 0 | full |
| role-admins | mapping b256 → b256 | 1 | 0 | full |
| name | string | 2 | 0 | full |
| symbol | string | 3 | 0 | full |
| currency | string | 4 | 0 | full |
| logo-uri | string | 5 | 0 | full |
| quote-token | address | 6 | 0 | full |
| next-quote-token | address | 7 | 0 | packed |
| transfer-policy-id | u64 | 7 | 20 | packed |
| total-supply | u256 | 8 | 0 | full |
| balances | mapping address → u256 | 9 | 0 | full |
| allowances | mapping address → (mapping address → u256) | 10 | 0 | full |
| permit-nonces | mapping address → u256 | 11 | 0 | full |
| paused | bool | 12 | 0 | full |
| supply-cap | u256 | 13 | 0 | full |
| salts (reserved) | mapping b256 → bool | 14 | 0 | full |
| global-reward-per-token | u256 | 15 | 0 | full |
| opted-in-supply | u128 | 16 | 0 | full |
| user-reward-info | mapping address → struct (3 slots) | 17 | 0 | full |

"Packed" matters for gas: a packed write is SLOAD then SSTORE, a full write is SSTORE only (`crates/precompiles/src/storage/types/mod.rs:312-320`). A field is packed only if it shares its slot with a neighbour, so `paused` is a full write despite being one byte.

## Forms

```lisp
(defstorable user-reward-info
  (reward-recipient address)     ; rel slot 0
  (reward-per-token u256)        ; rel slot 1
  (reward-balance   u256))       ; rel slot 2

(defcontract tip20-token (:prefix "TOKEN")
  (roles              (mapping address (mapping b256 bool)))
  (role-admins        (mapping b256 b256))
  (name               string)
  (symbol             string)
  (currency           string)
  (logo-uri           string :since :t5
                      :doc "TIP-1026. Reuses the never-written domain-separator slot.")
  (quote-token        address)
  (next-quote-token   address)
  (transfer-policy-id u64)
  (total-supply       u256)
  (balances           (mapping address u256))
  (allowances         (mapping address (mapping address u256)))
  (permit-nonces      (mapping address u256))
  (paused             bool)
  (supply-cap         u256)
  (salts              (mapping b256 bool) :reserved t)
  (global-reward-per-token u256)
  (opted-in-supply    u128)
  (user-reward-info   (mapping address user-reward-info)))
```

Contract options are `:address` for a fixed instance and `:prefix` for accessor names (`t` uses the contract name, or supply a string). Field options are `:since`, `:reserved`, `:doc`, `:label`, and `:slot n` / `:base-slot n` matching Rust's `#[slot]` and `#[base_slot]` (a manual slot does not advance the automatic chain; a base slot resets it).

## Expansion

- A layout object stored on the symbol: a vector of `(name type slot offset bytes access since)`. Everything else derives from it.
- Slot constants such as `+tip20-token/balances+` = 9.
- Inline accessors that return a place descriptor and touch no storage: `(token-balances self addr)`.
- A macroexpansion-time collision check over all fields, manual slots included.
- Exporters: `(layout-json 'tip20-token)` in solc `storageLayout` shape, diffed against the fixtures; a Markdown slot table for `spec/`.

Storage is touched only by explicit verbs, the same split as Rust's `.read()` / `.write()`:

```lisp
(read  (token-balances self from))               ; exactly one SLOAD
(write (token-balances self from) new-balance)   ; exactly one SSTORE (full slot)
(write (token-transfer-policy-id self) id)       ; SLOAD + SSTORE (packed)
```

No `setf`/`incf` on storage places: they hide a read. Common Lisp's left-to-right argument evaluation makes operation order a language guarantee.

Encoding rules (from the Rust source):

- **Packing:** a field joins the previous slot if `prev-offset + prev-bytes + bytes <= 32`; otherwise it starts the next slot at offset 0. Offset 0 is the low-order bytes.
- **Mapping key:** `keccak256(leftpad32(key) ‖ be32(slot))`, nested outer-first (`storage/types/mod.rs:343`: differs from Solidity's `abi.encode` for signed integers).
- **Strings:** ≤31 bytes inline, left-aligned, `length*2` in the low byte. Longer: `length*2+1` in the base slot, data at `keccak256(slot) + i`.
- **Structs:** start on a fresh slot and take whole slots, packed internally by the same rule.

`read`/`write` use the backend bound to `clempo.contract:*storage*`. The storage
protocol includes `backend-sload`, `backend-sstore`, `backend-tload`,
`backend-tstore`, `backend-emit-log`, `backend-deduct-gas` and `backend-refund-gas`.

- `metered-storage`: applies gas, static and warm/cold rules over a host that implements `host-sload`, `host-sstore` and related operations.
- `hashmap-storage`: a backend for unit tests.
- `recording-storage`: records the operation trace for differential comparison.

## Calls and fork gating

```lisp
(defcall tip20-token transfer* ((to address) (amount u256))
    (:sol "transfer(address to, uint256 amount)" :returns "bool" :mutates t)
  (transfer self sender to amount))

(defcall tip20-token logo-uri* ()
    (:sol "logoURI()" :returns "string" :since :t5)
  (logo-uri self))
```

`defcall` registers the selector, mutability and fork gate (`:since` inclusive, `:until` exclusive). The signature specifies the ABI input types; `:returns` is a type string or a list of them. The generated function binds `self`, `sender` and the decoded arguments. `run-calls` checks the selector and its fork window, decodes, rejects static calls to mutating methods, runs the body, then ABI-encodes its return values. A precompile can perform additional checks before calling `run-calls`.

Fork branches use one macro so they are queryable:

```lisp
;; Inside TIP-20 transfer, before reward updates:
(fork-if (:before :t8)
    (let ((balance (balance-of token from)))
      (when (> amount balance)
        (revert 'insufficient-balance balance amount token))
      balance))
```

`fork-if`, fork-gated fields, and `defcall` fork windows register source locations; `(fork-sites :t8)` lists the recorded T8 sites. `revert` signals a condition encoded as an ABI revert; out-of-gas is a separate halt condition.
