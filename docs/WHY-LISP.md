# Why Lisp, by example

clempo, the Rust reference node ([tempoxyz/tempo](https://github.com/tempoxyz/tempo), with its EVM [evm2](https://github.com/alloy-rs/evm2) at `e25a95f`) and [goempo](https://github.com/gakonst/goempo) (Go) implement the same protocol, so the same piece of it can be put side by side. Each section below takes one such piece, shows how clempo writes it, and quotes or summarizes the other two. Lisp paths are relative to this repository; Rust and Go paths are relative to their own repositories.

The comparison is not "Lisp can generate code and the others cannot". Rust has `macro_rules!`, derives and proc macros, and the reference uses all three. Go has `go generate`. The difference is where the generator lives and what is left once it has run:

- A Lisp macro is an ordinary function in the same file and package as the code it serves. It runs inside the compiler, with every function loaded so far available to it, and needs no separate crate, token-stream parser or build step.
- What a declarative form declares stays as data in the running image: a layout on a symbol, an op table in a vector, fork sites in a registry. Tools, tests and the REPL query the same data the code was generated from.
- A form can expand into code for several consumers at once: an interpreter and a compiler, a dispatcher and an ABI exporter, a branch and a documentation index.

Expansions shown below were printed by loading `clempo-tempo` into SBCL 2.6.8 and calling the functions named; package prefixes are dropped.

## 1. An opcode is one form

```lisp
;; src/evm/ops.lisp
(defop #x01 add (a b) (:gas 3 :pushes 1) (push-word (u+ a b)))
(defop #x08 addmod (a b n) (:gas 8 :pushes 1) (push-word (add-mod a b n)))
(defop #x5f push0 () (:gas 2 :pushes 1 :since :shanghai) (push-word 0))
(defop #x1e clz (x) (:gas 5 :pushes 1 :since :osaka :doc "EIP-7939") ...)
```

`defop` (`src/evm/defop.lisp`, 58 lines) does not generate code. It records the byte, name, arguments, gas, stack effect, fork and body as an `op-info` in `*op-table*`. `define-interpreter` later turns every entry into one clause of a single `case`, which SBCL compiles to a jump table. The clause for `ADD`, from `(op-clause (op-info 'add))`:

```lisp
(1 (charge 3)
   (when (< sp 2) (halt :stack-underflow))
   (let ((a (svref stack (- sp 1)))
         (b (svref stack (- sp 2))))
     (declare (type word a b) (ignorable a b))
     (decf sp 2)
     (push-word (u+ a b)))
   (incf pc))
```

and for `PUSH0`, where `:since :shanghai` becomes a fork check and `:pushes 1` an overflow check:

```lisp
(95 (unless (>= fork 4) (halt :invalid-opcode))
    (charge 2)
    (when (> sp 1023) (halt :stack-overflow))
    (let () (push-word 0))
    (incf pc))
```

The table is also data at the REPL: `(ops-since :cancun)` returns `(blobhash blobbasefee tload tstore mcopy)`.

`src/evm/interpreter.lisp` expands the same table three times:

```lisp
(define-interpreter interpret)                       ; no tracing code at all
(define-interpreter interpret-traced :traced t)      ; TRACE-STEP / TRACE-STEP-END around every op
(define-interpreter interpret-lean-traced :traced :lean) ; only the halting op is reported
```

The untraced loop contains no test of a tracer; `when-tracing` expands to `nil` there.

**Go** (goempo) spreads an opcode over four places: the constant (`internal/evm/opcodes.go`: `ADD OpCode = 0x01`), its name (`ADD: "ADD"` in the same file), its function (`internal/evm/ops_arith.go`), and its table entry with gas and stack effect (`internal/evm/table.go`):

```go
func opAdd(_ *EVM, f *Frame) Status {
	x := f.Pop()
	y := f.Peek(0)
	*y = x.Add(*y)
	return StatusContinue
}

t.Set(ADD, Operation{Exec: opAdd, Gas: g.VeryLow, In: 2, Out: 1})
```

**Rust** (evm2) also uses four places, two of them macros: the `opcodes!` table (`interpreter/opcode.rs`: `0x01 => ADD => add => stack_io(2, 1);`), the body behind the `#[instruction]` proc macro (`interpreter/instructions/arithmetic.rs`), the static gas in a per-version table (`version/mod.rs`: `ADD: VERYLOW`), and, for the JIT, a second translation of the opcode into IR (next section).

## 2. The compiler reuses the interpreter's semantics

clempo's optional EVM compiler (`src/evm/compile.lisp`) turns bytecode into Lisp and hands it to SBCL's `compile`. It has no opcode semantics of its own: each instruction becomes its `defop` body with the arguments bound to a symbolic stack. `PUSH` values become literals, `DUP`/`SWAP`/`POP` move symbols at compile time, and static gas is charged once per segment.

For `60026003015f5260205ff3` (`PUSH1 2 PUSH1 3 ADD PUSH0 MSTORE PUSH1 0x20 PUSH0 RETURN`) under Cancun, `(code-lambdas code :cancun)` produces, abridged:

```lisp
(when (or (> sp 1022)) (resume 0))
(let ((cost 14))                            ; PUSH1 PUSH1 ADD PUSH0 MSTORE: 3+3+3+2+3
  (when (< gas cost) (resume 0))            ; short of gas: the interpreter takes over at pc 0
  (decf gas cost))
(let ((#:v1 (symbol-macrolet ((pc 4))
              (let ((a 3) (b 2))            ; the ADD body, operands now constants
                (push-word (u+ a b))))))
  (symbol-macrolet ((pc 6))
    (let ((offset 0) (value #:v1))          ; the MSTORE body
      (mem-ensure offset 32)
      (write-word (frame-memory frame) offset value)))
  (let ((cost 5)) ...)                      ; PUSH1 PUSH0 before RETURN
  (symbol-macrolet ((pc 10))
    (let ((offset 0) (size 32))             ; the RETURN body
      (mem-ensure offset size)
      (halt :return (memory-slice frame offset size)))))
```

`push-word`, `halt` and `charge` are `macrolet`s, so the same body means "write the stack vector" in the interpreter and "name a value" in the compiler. When a segment cannot pay for itself, the compiled code writes its stack out and `resume`s the interpreter at the segment's first pc, so the halting op, status and gas left are the interpreter's own. A fix to an opcode reaches both engines; a divergence between them is a compiler bug in control flow or gas, never in an opcode's meaning.

evm2's JIT (`crates/jit/codegen/src/compiler/translate/mod.rs`) translates each opcode a second time, into IR for LLVM:

```rust
op::ADD => {
    let [a, b] = self.popn();
    let sum = self.bcx.iadd(a, b);
    self.push(sum);
    ...
```

That buys LLVM's optimizer, and it means every opcode has two implementations kept in agreement by tests. clempo's compiler gets agreement by construction and relies on SBCL's optimizer, which is weaker.

## 3. A storage layout is data

Tempo's precompiles keep Solidity-compatible storage, and gas depends on the exact sequence of SLOADs and SSTOREs, so the layout has to be right to the byte. [DEFCONTRACT.md](DEFCONTRACT.md) covers the design.

```lisp
;; src/tempo/precompiles/tip20.lisp (abridged)
(defcontract tip20-token (:prefix "TOKEN")
  (roles              (mapping address (mapping b256 bool)))
  (role-admins        (mapping b256 b256))
  (name               string)
  ...
  (logo-uri           string :since :t5
                      :doc "TIP-1026. Reuses the never-written domain-separator slot.")
  (quote-token        address)
  (next-quote-token   address)      ; packed with the next field
  (transfer-policy-id u64)
  (total-supply       u256)
  (balances           (mapping address u256))
  ...)

(read (token-balances token account))   ; one SLOAD
(read (token-transfer-policy-id token)) ; one SLOAD, then bytes 20..27 of slot 7
```

**Rust** is as declarative here. The `#[contract]` proc macro (`crates/precompiles-macros`) computes the same slots from a struct, and `self.balances[account].read()` reads like clempo's place. The two differ in what the slot table is after compilation:

```lisp
(let ((f (layout-field 'tip20-token 'transfer-policy-id)))
  (list :slot (field-slot f) :offset (field-offset f) :bytes (field-bytes f) :access (field-access f)))
;; => (:slot 7 :offset 20 :bytes 8 :access :packed)

(documentation 'token-transfer-policy-id 'function)
;; => "Place of transferPolicyId (slot 7, offset 20)."
```

The layout is a value on the symbol `tip20-token`, so `bin/clempo-dev spec` writes `spec/layouts/` from it, the layout tests compare it with solc's `storageLayout` output, and the gate fails if the checked-in copy drifts. The Rust macro emits a `slots` module of per-field constants and collision checks, and the reference's slot tests use a separate test-only proc macro (`gen_test_fields_layout`). In clempo the accessors, the exporter and the tests read one value. The whole of `defcontract` (allocator, `defstorable`, exporters) is the 305 lines of `src/tempo/contract/layout.lisp`.

**Go** (goempo) writes the layout by hand:

```go
// internal/tempo/precompiles/tip20.go
slotQuoteToken     = slotOf(6)
slotNextQuoteToken = slotOf(7)
slotTransferPolicy = slotOf(7)
slotTotalSupply    = slotOf(8)
...
fTransferPolicy = field{offset: 20, size: 8}
fNextQuote      = field{offset: 0, size: 20}

w, err := t.c.loadField(t.addr, slotTransferPolicy, fTransferPolicy)
```

That is explicit and easy to read, and every number is the author's to keep right. Adding a field in the middle of a struct renumbers everything after it by hand.

## 4. A precompile method is one form

```lisp
;; src/tempo/precompiles/tip20.lisp
(defcall tip20-token transfer* ((to address) (amount u256))
    (:sol "transfer(address to, uint256 amount)" :returns "bool" :mutates t)
  (transfer self sender to amount))

(defcall tip20-token logo-uri* () (:sol "logoURI()" :returns "string" :since :t5)
  (logo-uri self))
```

At macroexpansion `defcall` (`src/tempo/contract/calls.lisp`) parses the Solidity signature, computes the selector, checks that the Lisp parameter list matches the signature's arity (a mismatch fails the compile), and registers an entry with the ABI types, return types, mutability and fork window. The generic dispatcher `run-calls` then does what the reference's `dispatch!` does: unknown or not-yet-active selector, strict decoding from T11, static-call rejection for `:mutates`, call, encode. The same entries produce `spec/abi/*.json`.

**Rust** spreads a method over three places: the Solidity interface in a `sol!` block (`crates/contracts/src/precompiles/tip20.rs`), a match arm in `crates/precompiles/src/tip20/dispatch.rs`, and the method:

```rust
// dispatch.rs
#[schedule(since = T5)]
logoURI(call) => view(call, |_| self.logo_uri()),
transfer(call) => mutate(call, msg_sender, |sender, c| self.transfer(sender, c)),

// mod.rs
pub fn transfer(&mut self, msg_sender: Address, call: ITIP20::transferCall) -> Result<bool> {
```

The type checker connects them, but adding a method touches three files in two crates.

**Go** keeps one table entry per method, with the signature as a string and its argument types written again beside it:

```go
// internal/tempo/precompiles/tip20_abi.go
view("logoURI()", nil, stringView(slotLogoURI)).from(hardfork.T5),
view("balanceOf(address)", []abiType{tAddress}, func(c *Context, a []abiValue) ([]byte, error) {
	b, err := c.cur().balance(a[0].addr())
	return retWord(b), err
}),
```

Nothing checks that `[]abiType{tAddress}` agrees with `"balanceOf(address)"`, or that the handler reads `a[0]` and not `a[1]`.

## 5. A fork gate knows where it is

Much of the work of following Tempo is hardforks: T0 through T15 so far, plus T1a–T1c, each changing behavior at between 2 and 34 sites in clempo. A gate is a macro:

```lisp
;; src/tempo/precompiles/address-registry.lisp
(defun resolve-recipient (to)
  "AddressRegistry::resolve_recipient: TO, or from T3 the master of a virtual TO
(VirtualAddressUnregistered() when none)."
  (fork-if (:before :t3) (return-from resolve-recipient to))
  (if (virtual-address-p to)
      (or (get-master (master-id to)) (revert 'virtual-address-unregistered))
      to))
```

`fork-if` (`src/tempo/forks/forks.lisp`) expands into the comparison plus a `load-time-value` that records the fork, the enclosing definition and the source position (file, top-level form number, path inside the form) when the file loads. `spec>=`, `spec<`, `since` and `until` record their sites the same way through compiler macros, and `defcall :since`, `defcontract :since` and `defprecompile :since` record theirs. T5 has 26 sites:

```lisp
(first (fork-sites :t5))
;; => #S(fork-site :fork :t5 :kind :branch :where store-bytes-like :doc nil
;;                 :file "src/tempo/contract/types.lisp" :form-number 43 :form-path (3 2 1 2)
;;                 :definition store-bytes-like :relation :since)
```

`bin/clempo-dev spec` writes the registry to `spec/forks/<fork>.md`, one entry per site, and the gate fails if it drifts. When a new fork lands in the reference, the question "what did T5 change" has a checked answer in the tree.

The Rust and Go gates are plain conditionals:

```rust
// crates/precompiles/src/address_registry/mod.rs
if !self.storage.spec().is_t3() {
    return Ok(to);
}
```

```go
// internal/tempo/precompiles/channel_reserve_ops.go
if !c.at(hardfork.T12) {
```

They are just as clear at the site. Collecting them means grepping for `is_t5()`, `T5` and `.from(hardfork.T5)` and reading each hit to see whether it is a gate.

## 6. RLP: where Rust is even

```lisp
;; src/types/headers.lisp
(defrlp header
  (general-gas-limit :u64 :doc "Main block non-payment gas limit.")
  (shared-gas-limit :u64 :doc "Gas limit shared with the subblocks section.")
  (timestamp-millis-part :u64 :doc "Sub-second timestamp milliseconds.")
  (inner (:struct ethereum-header-from-items ethereum-header-to-items))
  (consensus-context (:struct consensus-context-from-items consensus-context-to-items)
                     :trailing t :doc "Consensus metadata; absent in earlier forks."))
```

`defrlp` generates the struct, a bounded decoder that reads fields straight from the input without intermediate vectors, an encoder that sizes the output first and writes it once, and the `:trailing` rule (a suffix of optional fields with no gaps).

Rust's derive says the same thing:

```rust
// crates/primitives/src/header.rs
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, RlpEncodable, RlpDecodable)]
#[rlp(trailing(no_gaps))]
pub struct TempoHeader { ... pub consensus_context: Option<TempoConsensusContext>, }
```

Here Lisp only adds that the schema stays queryable (`(rlp-layout 'consensus-context)` returns the field list with its codecs and doc strings) and that the generator is 170 lines in `src/types/defrlp.lisp`, not a dependency. goempo's header decoder is written by hand (`internal/types/header.go`, one `if h.X, err = list.Uint64(); err != nil` per field).

## 7. Recovery is decided by the caller, without unwinding

A replay that finds a block disagreeing with mainnet has several reasonable next steps, and the right one depends on who is running it. A survey wants to patch the state and keep going, so it can list every class of divergence in one run. A clean replay wants to stop. A developer at a REPL wants to fix the bug and run the same block again on the warm state.

The replay loop (`src/tempo/executor/replay.lisp`, abridged) signals a `block-mismatch` condition and offers all of them as restarts, at the point where each makes sense:

```lisp
(restart-case (progn (run block) (incf n))
  (retry-block ()
    :report (lambda (s) (format s "Undo block ~D and execute it again." n))
    (undo))
  (heal-from-reference ()
    :report (lambda (s) (format s "Apply the reference's post-state for block ~D and continue." n))
    :test (lambda (c) (declare (ignore c)) heal-fn)   ; offered only when a reference is configured
    (undo) (store:db-begin-block db)
    (funcall heal-fn db n block)
    ...                                              ; recheck the root after healing
    (commit block) (push n (replay-result-heals result)) (incf n))
  (skip-block ()
    :report (lambda (s) (format s "Keep block ~D's state as executed and continue." n))
    ...)
  (abort-replay (&optional condition)
    :report (lambda (s) (format s "Undo block ~D and stop the replay." n))
    (undo) ... (loop-finish)))
```

The command-line driver (`src/dev/replay.lisp`, abridged) chooses by flags. `handler-bind` runs the handler on top of the signalling frame, before anything unwinds, so the restarts are still there to invoke:

```lisp
(choose (c)
  (cond ((and (< heals heal) (find-restart 'heal-from-reference c))
         (incf heals) (invoke-restart 'heal-from-reference))
        ((and (typep c 'block-mismatch) (< skips skip))
         (incf skips) (invoke-restart 'skip-block))
        (t (invoke-restart 'abort-replay c))))
...
(if interactive
    (run block-fn senders-fn)                                  ; conditions reach the debugger
    (handler-bind ((error #'choose)) (run block-fn senders-fn)))
```

With `-interactive`, or from a REPL, the condition reaches the debugger, which lists these four restarts under their `:report` strings. Fixing the bug is then: edit the function, recompile it into the running image, and invoke `retry-block`. The datadir, the open store and the warm caches are untouched.

In Rust or Go an error return has already unwound the loop by the time the caller sees it. The same policy needs a strategy passed into the loop. A sketch, not code from either project:

```go
type MismatchPolicy func(err *BlockMismatch) Action // Heal, Skip, Abort, Retry

for n <= to {
	err := run(block)
	if mm, ok := err.(*BlockMismatch); ok {
		switch policy(mm) { case Heal: ...; case Skip: ...; case Retry: continue; default: return err }
	}
	n++
}
```

That works for the batch cases. The interactive case, where a person changes the code between the failure and the retry and the process keeps its state, has no equivalent without a debugger that can redefine functions and resume.

## 8. Context is dynamically bound

The executor, RPC threads, tests and simulator share code that needs a world state, a chain spec, a tracer, a clock and a source of randomness. clempo passes these in special variables: `*world*`, `*spec*`, `*block*`, `*tracer*`, `clempo.io:*clock*`, `clempo.io:*entropy*`. A `let` binds one for its dynamic extent, on the current thread only, and restores the old value on any exit, including a non-local one.

The deterministic simulator (`src/sim/run.lisp`) runs the real Simplex and DKG code on a manual clock and a seeded entropy source:

```lisp
(defun run-scenario (name-or-config &key (max-ms 600000))
  "Run a scenario deterministically; returns a SIM-RESULT."
  (let* ((config ...)
         (io:*entropy* (io:make-seeded-entropy (getf config :seed))))
    (multiple-value-bind (clock handle) (io:make-manual-clock 0)
      (let* ((io:*clock* clock)
             (sim (make-simulation config))
             ...
             (status (run-loop sim handle target max-ms)))
        (sim-result sim status)))))
```

and a scenario is a few lines of data:

```lisp
;; src/sim/scenarios.lisp
(defscenario "r6-5"
  :validators 5 :seed 5 :epochs 3 :epoch-length 25 :latency (5 60) :drop 1/50
  :faults ((:outage 1 :from 1000 :to 3000)
           (:partition ((0 1) (2 3 4)) :from 5000 :to 6500)))
```

Nothing between `run-scenario` and the consensus code takes a clock parameter.

**Go** threads the clock through configuration structs (`internal/cwp2p/network.go`: `Clock gio.Clock` in the config, then `n.cfg.Clock.MonotonicNanos()` at each use). **Rust**, through commonware, threads a context type whose trait bounds name every capability (`crates/consensus/src/alias.rs`: `TContext: Clock + Metrics + Spawner + Storage + Rng + CryptoRng`), and runs tests under `deterministic::Runner`. Both make the dependency visible in every signature it passes through, which is a real advantage when reading one function in isolation.

The cost of the Lisp approach is that the dependency is invisible. clempo pays it with a check: `scripts/check-layers.lisp` refuses raw clock, entropy, file, socket and thread operations outside the I/O layers named in `layers.sexp`, so `*clock*` is the only way to the time.

## 9. Tracers are generic functions with no-op defaults

```lisp
;; src/evm/spec.lisp
(defmacro define-trace-hook (name (&rest args) doc)
  `(defgeneric ,name (tracer ,@args)
     (:documentation ,doc)
     (:method (tracer ,@args) (declare (ignore tracer ,@args)))))

(define-trace-hook trace-call-start (frame)
  "FRAME, a message call or creation, starts with FRAME-GAS gas; ...")
(define-trace-hook trace-step (frame pc op gas sp stack)
  "Before the opcode OP at PC; GAS is the gas left, the stack is STACK[0..SP).")
```

A tracer is a class that defines methods only for the hooks it cares about. Every hook already has a default, so a call tracer needs no empty `trace-step` method, and `trace-steps-p` lets a tracer that ignores steps run on the lean interpreter (section 1).

goempo's `evm.Tracer` is a four-method interface (`Enter`, `Exit`, `Step`, `StepEnd`), so every tracer implements all four, and the EVM selects a traced path with `if e.tracer != nil`. Go has no default methods. The usual workaround, embedding a no-op struct, is available, but the interface itself cannot supply them.

## 10. A 256-bit word is an integer

```lisp
;; src/evm/words.lisp
(defun u+ (a b)
  (declare (type word a b))
  (if (and (typep a 'fixnum) (typep b 'fixnum))
      (+ a b)
      (let ((r (+ a b))) (if (>= r +mod+) (- r +mod+) r))))

(defun add-mod (a b n)
  (declare (type word a b n))
  (if (zerop n) 0 (mod (+ a b) n)))

(defun mul-mod (a b n)
  (declare (type word a b n))
  (if (zerop n) 0 (mod (* a b) n)))
```

`word` is `(unsigned-byte 256)`. Common Lisp integers are arbitrary-precision, so `MULMOD`'s 512-bit intermediate product is just `(* a b)`, and the definition reads like the Yellow Paper. Many words on a real stack are small (counters, offsets, lengths, booleans), and those stay fixnums with no allocation, which is why each operation tests for the fixnum case first.

goempo's `internal/u256` is 717 lines of non-test Go over `[4]uint64` limbs, with a separate `Wide` type for the 512-bit intermediates:

```go
func (x Int) Add(y Int) Int {
	var z Int
	var c uint64
	z[0], c = bits.Add64(x[0], y[0], 0)
	z[1], c = bits.Add64(x[1], y[1], c)
	z[2], c = bits.Add64(x[2], y[2], c)
	z[3], _ = bits.Add64(x[3], y[3], c)
	return z
}

*m = x.MulFull(y).Mod(*m) // MULMOD
```

evm2 uses `ruint` (`a.wrapping_add(*b)`, `a.mul_mod(*b, *n)`), which is as short at the call site and moves the limb code into a dependency.

Fixed limbs never allocate; a Lisp bignum does. clempo pays that only for large values, and its replay and RPC numbers in the [README](../README.md#status) include the cost. Where allocation must be zero (secp256k1, P-256, BLS12-381), clempo does not use bignums at all: the field code runs on typed limb arrays.

## 11. Limits carry their proofs

```lisp
;; src/evm/frame.lisp
(defbound +max-memory+ (ash 1 32)
  "Memory beyond 4 GiB is out of gas for any reachable gas limit."
  ;; MEMORY-COST squares the word count in fixnum arithmetic.
  (typep (* (ceiling +max-memory+ 32) (ceiling +max-memory+ 32)) 'fixnum)
  ;; GROW-MEMORY at most doubles a buffer of this size.
  (typep (* 2 +max-memory+) 'fixnum)
  ;; Out of gas indeed: this much memory costs over 2^40 gas.
  (> (+ (* 3 (ceiling +max-memory+ 32)) (floor (expt (ceiling +max-memory+ 32) 2) 512))
     (ash 1 40)))
```

`defbound` (`src/base/bounds.lisp`, 42 lines including the comments) defines the constant and evaluates each proof with `static-assert` when the file is compiled. A false proof stops the build; `(static-assert (typep (* 2 (ash 1 62)) 'fixnum) "toy")` fails with:

```
static assertion failed at compile time: (typep (* 2 (ash 1 62)) 'fixnum) (toy)
```

Rust's `const _: () = assert!(...)` and Go's `const _ = uint(a - b)` do the same job within what each language's constant evaluator accepts. The Lisp version can call any function already loaded (`ceiling`, `expt`, `integer-length`, `logcount`), keeps each proof beside the constant it justifies, and lets the source gate require that every fixed-count loop in hot code take its count from a `defbound`.

## 12. Assembly is generated by Lisp, inside the compiler

Keccak-f[1600] on arm64 uses the FEAT_SHA3 instructions (`EOR3`, `RAX1`, `XAR`, `BCAX`) through SBCL's `define-vop`, which lets a function compile to hand-chosen instructions with SBCL's register allocator assigning the registers. The VOP's generator is Lisp running at compile time, so the 24 rounds and the θ, ρ, π and χ steps are loops over lane indices and the rotation table, not a hand-unrolled listing (`src/crypto/keccak/keccak-arm64.lisp`, abridged):

```lisp
(dotimes (round +rounds+)
  (dotimes (x +plane-lanes+)                                ; theta: column parities with EOR3
    (let ((p (aref c x)))
      (op #xce000000 p (lane x 0) (lane x 1) (sb-c:tn-offset (lane x 2)))
      (op #xce000000 p p (lane x 3) (sb-c:tn-offset (lane x 4)))))
  (dotimes (x +plane-lanes+)
    ;; RAX1 computes C[x-1] XOR ROL1(C[x+1]). XAR applies the correction and rotates.
    (op #xce608c00 delta (aref c (mod (+ x 4) 5)) (aref c (mod (1+ x) 5)))
    (dotimes (y +plane-lanes+)
      (let ((r (lane x y)))
        (op #xce800000 r r delta (mod (- (aref rotations (+ x (* +plane-lanes+ y)))) 64))
        ;; pi is a renaming: the lane's register moves to its new position
        (setf (aref next (+ y (* +plane-lanes+ (mod (+ (* 2 x) (* 3 y)) 5)))) r))))
  (rotatef layout next)
  ...)                                                      ; chi with BCAX, iota with EOR
```

π costs no instructions: it permutes which register the generator calls lane (x, y). The result runs at 799 MB/s, with portable Lisp as the fallback when the CPU lacks the extension.

The Rust reference reaches the same kind of code through a build step: `alloy-primitives/asm-keccak` pulls in `sha3-asm`, whose `build.rs` runs a CRYPTOGAMS Perl script to emit assembly and compiles that with `cc`. That is also a generator in another language. clempo's is in the same language, the same file and the same compile as the code that calls it. goempo wraps `golang.org/x/crypto/sha3`; native code of its own in Go would mean a Plan 9 `.s` file.

## 13. The node is a live image

```sh
bin/clempo --datadir "$STATE" --rpc-port 18545 \
  --repl-port 4005 --repl-token-file "$HOME/.clempo-repl-token"
```

The authenticated loopback REPL ([NODE.md](NODE.md#local-lisp-repl)) runs forms inside the following node:

```lisp
(let ((b clempo.node:*node-backend*))
  (clempo.rpc.chain:with-backend-read (b)
    (clempo.rpc.chain:backend-head b)))
```

The same session can call `(fork-sites :t12)`, inspect a layout or the head state, or redefine a function that is not inlined into its callers and have the next block run through the new definition. With section 7, this changes how divergences get fixed: on a 50-million-block replay the warm state is the expensive part, and it never has to be rebuilt to try a fix. A compiled Rust or Go binary has to restart to change code.

## What it costs

- **Types are checked late.** Declarations make SBCL check types in hot code, and check builds keep `safety 1`, but a wrong argument type elsewhere is a run-time error that Rust or Go would reject at compile time. The differential tests (61,903 state tests, the RPC corpus, full mainnet replay) are what catch these.
- **Each form is a small language.** A `defop` body uses `sp`, `gas`, `frame`, `push-word` and `halt` without binding them; `defcall` binds `self` and `sender`. A reader has to learn each form's conventions, which `src/evm/defop.lisp` and each macro's doc string spell out.
- **Dynamic binding hides dependencies.** Section 8 trades visible parameters for a layer check.
- **The fast paths are SBCL's.** VOPs, `sb-thread`, `sb-ext` and the compiler's type inference are not portable Common Lisp. clempo targets one implementation on purpose.
- **There is no ecosystem to lean on.** clempo's rule against outside libraries is a choice, but Common Lisp offers little to choose from anyway: TLS 1.3, the HTTP client, the JSON codec and every curve are clempo's own code.
