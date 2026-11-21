;;; Limits for audited non-I/O iteration sites that the structural checker cannot
;;; prove finite or order-independent. A new site requires its own review;
;;; source code has no mechanism for disabling the checks.
(:loops
 (("src/base/octets.lisp"
   ("INTEGER-INTO-OCTETS" 3 "Bounded by the destination width; wider paths consume whole digits first.")
   ("OCTETS=" 2 "Typed LOOP bindings precede finite bounds on the word comparison and byte tail.")
   ("OCTETS->INTEGER" 2 "Bounded by the requested source slice."))
  ("src/codec/abi.lisp"
   ("UTF8-DECODE" 1 "Cursor consumes one or more input bytes on each iteration.")
   ("UTF8-VALID-P" 1 "Cursor advances to the fixed input end, or rejects invalid UTF-8."))
  ("src/codec/commonware.lisp"
   ("READ-VARINT" 1 "Each continuation consumes a byte and advances its bit offset toward the width limit.")
   ("WRITE-VARINT" 1 "Each continuation shifts seven bits out of a nonnegative integer."))
  ("src/codec/json.lisp" ("PARSE-JSON" 6 "Cursor advances through a fixed input, or rejects; nesting has its own limit."))
  ("src/codec/rlp.lisp" ("DECODE-AT" 1 "A child consumes at least its prefix byte within the parent list slice."))
  ("src/consensus/dkg/field.lisp"
   ("FR-EXPT" 1 "Right shifts consume exponent bits.")
   ("SCALAR-FROM-IKM" 1 "Hash-to-scalar retries only the zero result, per the key-generation algorithm."))
  ("src/consensus/dkg/manager.lisp"
   ("DKG-CEREMONY-OUTPUT" 1 "Consumes a finite dealer-log payload; undecodable entries stop parsing."))
  ("src/consensus/simplex/voter.lisp"
   ("ASSEMBLE-VERIFIED" 1 "Failed reconstruction clears at least one bad share from finite participant slots.")
   ("PUMP" 1 "Drains pending voter events and reconciles state until no internal transition remains.")
   ("RESOLVER-FETCH-MISSING" 1 "Moves the resolver anchor toward its current view."))
  ("src/crypto/ec/bls12-381-field.lisp" ("FP-POW!" 1 "Consumes exponent bits."))
  ("src/crypto/ec/bls12-381-group.lisp" ("DEFINE-LIMB-GROUP" 4 "Generated scalar-multiply loops consume fixed-size scalars or bounded windows."))
  ("src/crypto/ec/bls12-381-pairing.lisp" ("MILLER-PRODUCT!" 1 "Runs over bits of the fixed BLS parameter."))
  ("src/crypto/ec/bls12-381-signatures.lisp" ("BLS-SCALAR-POWER" 1 "Consumes scalar exponent bits."))
  ("src/crypto/ec/bls12-381-tower.lisp" ("F12-POWER!" 1 "Consumes exponent bits."))
  ("src/crypto/ec/bls12-381.lisp" ("F2-POW" 1 "Consumes exponent bits."))
  ("src/crypto/ec/bn254.lisp"
   ("FP-INVERSE" 1 "Euclidean remainders decrease to zero.")
   ("FP12-POWER" 2 "Consumes exponent bits, including the fixed arithmetic-sequence specialization."))
  ("src/crypto/ec/modexp.lisp" ("MODULAR-EXPT" 3 "Finite exponent width and digit/window cursors bound the square-and-multiply paths."))
  ("src/crypto/ec/p256-native.lisp"
   ("DEFINE-P256-RESIDUE" 1 "Generated fixed-width residue arithmetic.")
   ("P256-JOINT!" 1 "Traverses the finite joint scalar digits.")
   ("P256-TABLE!" 3 "Traverses the fixed-size scalar window table."))
  ("src/crypto/ec/p384-cache.lisp" ("P384-CACHE-HIT-P" 1 "Compares the finite selected cache entry."))
  ("src/crypto/ec/p384-field.lisp" ("DEFINE-P384-RESIDUE" 3 "Generated fixed-width residue arithmetic."))
  ("src/crypto/ec/p384-native.lisp"
   ("P384-JOINT!" 1 "Traverses the finite joint scalar digits.")
   ("P384-ODD-TABLE!" 3 "Traverses the fixed-size odd-multiple table.")
   ("P384-WNAF!" 1 "Consumes bounded scalar bits."))
  ("src/crypto/ec/prime-curve.lisp"
   ("DEFINE-PRIME-CURVE" 1 "Generated scalar multiplication scans scalar digits.")
   ("INVERT" 1 "Euclidean remainders decrease to zero.")
   ("WNAF" 1 "Consumes scalar bits after removing signed window digits."))
  ("src/crypto/ec/secp256k1-native-group.lisp" ("SN-JOINT!" 5 "Fixed-width joint scalar and window-table scans."))
  ("src/crypto/ec/secp256k1-native-scalar.lisp"
   ("SC-INV!" 3 "Inversion consumes fixed exponent windows.")
   ("SG-WNAF!" 3 "Finite-width signed window decomposition."))
  ("src/crypto/ec/secp256k1.lisp" ("SIGN" 1 "RFC6979 nonce rejection must retry unusable nonces."))
  ("src/crypto/hash/aes.lisp"
   ("DEFINE-BLOCK-ENCRYPTION" 1 "Macro loop covers the configured fixed round count.")
   ("GF-MUL" 1 "Consumes finite field multiplier bits.")
   ("S-BOX-ENTRY" 1 "Consumes exponent bits during GF(2^8) inversion."))
  ("src/crypto/hash/blake.lisp"
   ("DEFINE-BLAKE2F" 1 "Compression runs the declared round count.")
   ("HASH-TREE" 2 "Consumes input chunks or reduces the pending subtree stack."))
  ("src/crypto/hash/chacha.lisp"
   ("DEFINE-POLY1305" 1 "Traverses fixed-size blocks of the supplied message.")
   ("XOR-INTO" 1 "Consumes bytes of the supplied slice."))
  ("src/crypto/hash/sha2.lisp"
   ("(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)" 2 "Compile-time generation searches for a fixed count of primes.")
   ("DEFINE-SHA" 1 "Streaming hash loop consumes input blocks."))
  ("src/crypto/keccak/keccak.lisp" ("KECCAK-UPDATE" 1 "Consumes bytes up to the supplied end offset."))
  ("src/crypto/pki/der.lisp" ("DER-CHILDREN" 1 "Each DER child advances the cursor inside the enclosing length."))
  ("src/crypto/pki/gcm.lisp"
   ("CTR-XOR" 2 "Processes finite input blocks and bytes within each counter block.")
   ("GHASH-BLOCKS" 3 "Visits fixed-size words or a final partial block of the supplied bytes.")
   ("MAKE-GCM" 1 "Prepares the fixed-size table for multiplying GHASH field elements."))
  ("src/crypto/pki/x509.lisp"
   ("IPV4-OCTETS" 1 "Scans a finite numeric address string, rejecting malformed octets.")
   ("PEM-CERTIFICATES" 1 "Advances through a finite PEM bundle one certificate at a time."))
  ("src/evm/compile.lisp"
   ("CODE-BLOCKS" 1 "Disassembly advances the program counter past each instruction.")
   ("RUN-COMPILED" 1 "The EVM gas schedule bounds executed steps, including cycles.")
   ("SDEEP" 1 "Scans the compiler's finite list of stack depths."))
  ("src/evm/frame.lisp" ("ANALYZE-CODE" 2 "Instruction decoding advances across a finite bytecode vector."))
  ("src/evm/interpreter.lisp" ("DEFINE-INTERPRETER" 1 "Every executable control-flow cycle incurs positive EVM gas."))
  ("src/evm/tracing.lisp"
   ("EFFECTIVE-P" 1 "Traversal follows a finite chain of trace parents.")
   ("PARITY-PTRACES" 1 "Consumes a finite trace list.")
   ("STRUCT-LOGS" 1 "Traverses the tracer's finite recorded steps.")
   ("UTF8-LOSSY" 1 "Advances through input bytes, substituting for invalid sequences.")
   ("VM-TRACE" 1 "Traverses the finite tracer event sequence."))
  ("src/evm/tx.lisp" ("FAKE-EXPONENTIAL" 1 "Integer series stops when the successively divided term truncates to zero."))
  ("src/evm/words.lisp"
   ("READ-WORD" 1 "Consumes a fixed-width EVM word or the available input prefix.")
   ("U-EXP" 1 "Consumes the bits of a 256-bit exponent.")
   ("WRITE-WORD" 3 "Emits bytes from one fixed-width EVM word."))
  ("src/p2p/cw-stream.lisp" ("UINT" 1 "Unsigned varint generation shifts out seven bits per iteration."))
  ("src/p2p/snappy.lisp"
   ("COMPRESS" 4 "Varint and copy loops consume finite input lengths and matches.")
   ("DECOMPRESS" 1 "Consumes compressed tags up to the finite input length, checking the declared output bound."))
  ("src/rpc/defrpc.lisp" ("VALUE-SPANS" 1 "Advances through values of a fixed JSON slice."))
  ("src/rpc/peers.lisp"
   ("FORM-DECODE" 1 "Each step advances the index through the finite query string by one or three characters.")
   ("PARSE-ENR" 1 "Each key/value pair advances the position through the finite decoded record."))
  ("src/rpc/revert.lisp" ("SPLIT-SEQUENCE" 1 "Scans the finite signature string from one separator to the next."))
  ("src/rpc/serde.lisp"
   ("DO-ARRAY" 1 "Each element consumes input, or invalid JSON stops parsing.")
   ("DO-OBJECT" 1 "Each member consumes input, or invalid JSON stops parsing.")
   ("F64-FROM-PARTS" 1 "Exponent processing is bounded by the parsed floating-point range.")
   ("PARSE-SERDE-NUMBER" 3 "Each numeric scan consumes digits of the finite JSON token.")
   ("READ-STRING" 1 "Consumes characters or escapes from a bounded JSON input.")
   ("SKIP-NUMBER" 1 "Consumes characters of one number token.")
   ("SKIP-WS" 1 "Stops at end of supplied input."))
  ("src/rpc/stubs.lisp" ("SPLIT" 1 "Splits a finite string, advancing past the separator each time."))
  ("src/rpc/tempo-request.lisp"
   ("RLP-VEC" 1 "Traverses finite RLP items in the supplied list.")
   ("SEQ-TRAILING" 1 "Scans finite trailing transaction fields."))
  ("src/rpc/witness.lisp" ("OWNER-WEIGHT" 1 "Scans the finite configured owner list."))
  ("src/sim/network.lisp"
   ("HEAP-POP" 1 "Descends a finite heap.")
   ("HEAP-PUSH" 1 "Ascends a finite heap.")
   ("NETWORK-POP-DUE" 1 "Drains due messages from the finite pending queue."))
  ("src/sim/run.lisp" ("RUN-LOOP" 1 "Simulation stops at its configured time limit or target height."))
  ("src/sim/sim.lisp"
   ("BOUNDARY-EXTRA" 1 "Scans the finite finalized ancestry until the epoch boundary.")
   ("ON-FINALIZATION" 1 "Walks finalized ancestry to the local head."))
  ("src/state/world.lisp" ("ROLLBACK" 1 "Pops entries until the requested journal marker."))
  ("src/sync/finalizations.lisp"
   ("DESCEND-HEADER-CHAIN" 1 "Walks a fixed height interval in batches, checking the expected hash of each header.")
   ("DISCOVER-CERTIFY-FROM" 1 "Binary search narrows the certificate-retention height range."))
  ("src/store/backed.lisp"
   ("NODE-VALUE-SPLIT" 1 "Consumes the count prefix, bounded by the stored value and overlong-varint guard.")
   ("WRITE-NODES" 1 "Each removed trie node is marked dead once; pending edge release traverses a finite trie."))
  ("src/tempo/builder/builder.lisp" ("BUILD-PAYLOAD" 1 "Discards or consumes candidates from the finite pool snapshot subject to payload limits."))
  ("src/tempo/contract/backends.lisp" ("HOST-REVERT" 1 "Pops finite memory-host journal entries to the snapshot marker."))
  ("src/tempo/contract/layout.lisp" ("SPLIT-DASHES" 1 "Advances past separators in a finite field name."))
  ("src/tempo/executor/block.lisp" ("RLP-LIST-P" 2 "The length scan is bounded by the RLP length-field size, and each valid child advances within its enclosing slice."))
  ("src/tempo/executor/call.lisp" ("ESTIMATE-GAS" 1 "Rounded binary search narrows the gas interval."))
  ("src/tempo/executor/replay.lisp" ("REPLAY-BLOCKS" 1 "The outer range is finite; only its explicitly requested retry restart repeats a block."))
  ("src/tempo/precompiles/dex.lisp"
   ("FIND-PATH-TO-ROOT" 1 "Follows a token path with its protocol depth bound.")
   ("GET-PRICE-LEVEL" 1 "Follows resting orders to the requested result or end of the level.")
   ("NEXT-INITIALIZED-ASK-TICK" 1 "Advances within the finite tick range.")
   ("NEXT-INITIALIZED-BID-TICK" 1 "Descends within the finite tick range.")
   ("QUOTE-EXACT-IN-PER-TICK" 1 "Consumes liquidity or terminates at the end of the tick/order chain.")
   ("QUOTE-EXACT-OUT-PER-TICK" 1 "Consumes liquidity or terminates at the end of the tick/order chain.")
   ("WALK-RESTING-ORDERS" 1 "Follows the level's order links; metered reads on the execution path also bound the walk."))
  ("src/tempo/precompiles/tip20.lisp" ("COMPLETE-QUOTE-TOKEN-UPDATE" 1 "Follows the quote-token links under metered storage and stops on the base token or a rejected cycle."))
  ("src/tempo/precompiles/validator-config-v2-helpers.lisp" ("%PARSE-VALIDATOR-V6" 1 "Consumes finite address octets/groups, or rejects the endpoint."))
  ("src/tempo/precompiles/zone-verifier-cbor.lisp"
   ("NITRO-PARSE-CABUNDLE" 1 "Consumes entries of a bounded CBOR bundle.")
   ("NITRO-PARSE-PAYLOAD" 1 "Consumes key/value items of the definite-length payload.")
   ("NITRO-PARSE-PCR-MAP" 1 "Consumes the finite CBOR map or rejects a bad PCR index."))
  ("src/tempo/precompiles/zone-verifier-pcr.lisp" ("PCR-SPLIT" 1 "Scans separators in a finite string."))
  ("src/tempo/precompiles/zone-verifier-x509.lisp"
   ("NITRO-DER-EXTENSIONS" 1 "DER field cursor advances to the finite extension sequence end.")
   ("NITRO-DER-NAME" 2 "DER cursors advance through nested finite name and attribute data."))
  ("src/tempo/txpool/pool.lisp"
   ("HEAP-POP" 1 "Descends a finite heap.")
   ("HEAP-PUSH" 1 "Ascends a finite heap.")
   ("POOL-BEST-TRANSACTIONS" 1 "Consumes the finite pending candidate heap."))
  ("src/trie/trie.lisp"
   ("BUILD-SUBTRIE" 1 "Advances through the bounded input item partition.")
   ("COMMON-LENGTH" 1 "Stops at key divergence or the shorter key length.")
   ("DECODE-NODE-AT" 1 "Consumes a finite RLP list with fixed branch arity.")
   ("PREHASH-ROOT-CHILDREN" 1 "Traverses at most 16 branch children.")
   ("RLP-LIST-BUFFER" 1 "Consumes the finite node element sequence.")
   ("TRIE-NODE-AT" 1 "Consumes key nibbles on descent or terminates at a leaf/mismatch.")
   ("WALK-KEY" 1 "Consumes key nibbles on descent or terminates at a leaf/mismatch."))
  ("src/types/blocks.lisp" ("DECODE-BLOCK" 2 "Each decoded transaction or ommer advances the cursor within its bounded RLP list."))
  ("src/types/signatures.lisp" ("WEBAUTHN-CLIENT-DATA" 7 "Token, whitespace, string and container scans advance within the bounded input."))
  ("src/validator/keys-age.lisp"
   ("DECRYPT-PAYLOAD" 1 "Processes bounded AGE ciphertext chunks, advancing on every chunk.")
   ("PARSE-HEADER" 2 "Parses stanzas and lines within the bounded AGE envelope.")
   ("SPLIT-SPACES" 1 "Advances past each separator in a finite line."))
  ("src/types/validation.lisp" ("CALLDATA-TOKENS" 1 "Traverses the finite calldata bytes.")))
 :hash-order
 (("src/consensus/dkg/manager.lisp" ("PRUNE" 1 "Builds an unordered set of cached epochs to remove."))
  ("src/consensus/simplex/journal.lisp"
   ("JOURNAL-PRUNE" 1 "Removes a set of unrelated sections; ordering is immaterial.")
   ("JOURNAL-REPLAY" 1 "Sorts the selected view numbers before replay."))
  ("src/consensus/simplex/voter.lisp"
   ("ADD-FINALIZATION" 1 "Marks independently obsolete rounds, with no per-round emitted effects.")
   ("PRUNE" 1 "Removes a set of unrelated rounds, with no per-round emitted effects."))
  ("src/evm/compile-cache.lisp"
   ("CLEAR-JIT-CACHE" 1 "Clears independent compiled entry states before clearing the cache.")
   ("JIT-CANCEL" 1 "Drops pending markers without emitting per-entry effects.")
   ("JIT-EVICT" 1 "Selects the entry with the oldest recency stamp."))
  ("src/evm/spec.lisp" ("PRECOMPILE-ADDRESSES" 1 "Produces the set used for initial warm addresses."))
  ("src/evm/tracing.lisp"
   ("CAPTURE-POST-STATE" 2 "Captures independent per-account and per-slot values; rendering sorts the keys.")
   ("FOUR-BYTE-JSON" 1 "Sorts the resulting signatures before rendering.")
   ("OPCODE-GAS-JSON" 1 "Sorts the opcode keys before rendering.")
   ("SORTED-RECORDS" 1 "Sorts records by address.")
   ("SORTED-SLOTS" 1 "Sorts slots by numeric key.")
   ("STEP-JSON" 1 "Sorts keys before rendering storage."))
  ("src/p2p/cw-limit.lisp" ("KEYED-GCRA-RETAIN-RECENT" 1 "Drops independent expired limiter entries."))
  ("src/rpc/bal.lisp"
   ("BALANCE-CHANGES" 1 "Sorts changes by index before rendering.")
   ("BLOCK-ACCESS-LIST" 1 "Sorts addresses before encoding the access list.")
   ("CLOSE-INDEX" 3 "Clears temporary sets while merging independent values by key.")
   ("SLOT-CHANGES" 1 "Orders slot changes by key before encoding.")
   ("SLOT-READS" 1 "Orders the read keys before encoding."))
  ("src/rpc/calls.lisp" ("EXECUTION-WITNESS" 1 "Canonicalizes witness output after collecting keyed entries."))
  ("src/rpc/defrpc.lisp" ("METHOD-NAMES" 1 "Sorts the method names before returning them."))
  ("src/sim/run.lisp" ("SCENARIO-NAMES" 1 "Sorts names before exposing them."))
  ("src/state/root.lisp"
   ("BACKEND-STATE-ROOT" 1 "Distinct-key trie inserts give the same canonical root in any order.")
   ("STORAGE-ROOT" 1 "Distinct-key trie inserts give the same canonical root in any order."))
  ("src/state/world.lisp"
   ("DRAIN-CHANGES" 1 "Represents independent storage writes by key; trie/durable encoding canonicalizes order.")
   ("FINALIZE-TX" 1 "Copies independent keys without emitting effects.")
   ("STORAGE-NONEMPTY-P" 2 "Existence predicates have no iteration-dependent result.")
   ("TAKE-TRANSIENT-STORAGE" 1 "Produces an alist of independent transient keys."))
  ("src/store/backed.lisp"
   ("DB-FLUSH-ENTRIES" 2 "Collects independent account and slot records into a sorted flush batch.")
   ("EVICT" 3 "Drops clean objects according to usage bits or clears whole caches.")
   ("SORTED-ENTRIES" 1 "Sorts by the store key before serializing entries.")
   ("WRITE-NODES" 2 "Applies independent node values/counts to a batch keyed by node hash."))
  ("src/tempo/chainspec/chainspec.lisp" ("GENESIS-STORAGE-ROOT" 1 "Distinct-key trie inserts give the same canonical root in any order."))
  ("src/tempo/contract/backends.lisp" ("MEMORY-HOST-COMMITMENTS" 1 "Collects a keyed set of configurable-account commitments in the memory host."))
  ("src/tempo/contract/calls.lisp" ("CONTRACT-ABI" 1 "Sorts declarations before exporting the ABI."))
  ("src/tempo/executor/call.lisp"
   ("ACCESS-LIST-RESULT" 2 "Sorts addresses and storage slots before encoding.")
   ("TRACE-CALL-START" 1 "Collects independent precompile addresses for tracing."))
  ("src/tempo/executor/genesis.lisp" ("LOAD-GENESIS" 1 "Distinct storage keys can be inserted in any order."))
  ("src/tempo/executor/lanes.lisp" ("COLLECT-STORAGE-CHANGES" 1 "Changes are keyed by unique storage slots."))
  ("src/tempo/forks/forks.lisp" ("FORK-SITES" 1 "Sorts the recorded source sites before rendering."))
  ("src/tempo/precompiles/oracle.lisp"
   ("COMPARE-ACCOUNTS" 1 "Comparison uses independent account keys.")
   ("COMPARE-STORAGE" 4 "Compares keyed values and sorts printed differences."))
  ("src/tempo/precompiles/storage-credits.lisp" ("FLUSH-CREDIT-DELTAS" 1 "Sorts account keys before performing metered storage writes."))
  ("src/tempo/txpool/pool.lisp"
   ("POOL-BEST-TRANSACTIONS" 2 "Collects keyed candidates into a heap whose comparator breaks ties.")
   ("POOL-REVALIDATE" 1 "Sorts candidates before revalidating them.")
   ("POOL-SWEEP-EXPIRED" 1 "Sorts expired entries before eviction."))
  ("src/tempo/txpool/service.lisp"
   ("POOL-CLEAR" 1 "Removes all transactions, independent of removal order.")
   ("POOL-SNAPSHOT" 1 "Sorts by submission sequence before returning transactions.")))
 :assertions
 (("src/consensus/certify.lisp" 2 "Rejects bad certificates and reports the lowest failing height.")
  ("src/consensus/scheme.lisp" 6 "Checks scheme dimensions, unique public keys and signer inputs.")
  ("src/consensus/simplex/voter.lisp" 8 "Checks committee configuration and prevents conflicting persisted voting decisions.")
  ("src/consensus/dkg/manager.lisp" 1 "Validates cached acknowledgements while processing duplicate deals.")
  ("src/consensus/dkg/protocol.lisp" 2 "Validates the construction and reconstruction inputs.")
  ("src/validator/engine.lisp" 1 "Checks the validator engine's signing/finalization invariants."))
 :threads
 (("src/p2p/eth.lisp" "CALL-WITH-ETH-LISTENER" 2 "CALL-WITH-ETH-LISTENER"
   "Listener and peer sessions join in the listener's cleanup.")
  ("src/sync/archive.lisp" "FETCH-RANGE" 1 "FETCH-RANGE" "Joins the archive worker group before returning.")
  ("src/store/kv.lisp" "KV-MERGE-TIERS" 1 "KV-WAIT-MERGES" "Waits for the tracked segment merge worker before compaction or close.")
  ("src/sync/metrics.lisp" "START-METRICS-SERVER" 1 "STOP-METRICS-SERVER" "Metrics service joins its listener.")
  ("src/rpc/chain.lisp" "BUILD-INDEX" 1 "BUILD-INDEX" "Joins index worker group before publishing the index.")
  ("src/rpc/sender.lisp" "BUILD-SENDER-INDEX" 1 "BUILD-SENDER-INDEX" "Joins sender-index workers before writing the index.")
  ("src/rpc/server.lisp" "SERVE-WEBSOCKET" 1 "SERVE-CONNECTION" "Connection owns its notification writer.")
  ("src/rpc/server.lisp" "ACCEPT-LOOP" 1 "STOP-SERVER" "RPC server owns each accepted connection.")
  ("src/rpc/server.lisp" "START-SERVER" 1 "STOP-SERVER" "RPC server owns its acceptor.")
  ("src/validator/network.lisp" "SPAWN-LOCKED" 1 "CALL-WITH-VALIDATOR-NETWORK" "Network shutdown joins tracked workers; link cleanup joins its writer.")
  ("src/node/node.lisp" "START-FORWARDER" 1 "STOP-FORWARDER" "Node shutdown joins the relay worker.")
  ("src/node/node.lisp" "START-JIT-WORKER" 1 "STOP-JIT-WORKER" "Node shutdown retires and joins its JIT worker.")
  ("src/node/repl.lisp" "ACCEPT-CLIENT" 1 "STOP-REPL" "REPL shutdown joins tracked client sessions.")
  ("src/node/repl.lisp" "START-REPL" 1 "STOP-REPL" "REPL shutdown joins its acceptor.")
  ("src/node/workers.lisp" "CALL-WITH-WORKERS" 1 "CALL-WITH-WORKERS" "The scoped worker group joins during unwind.")
  ("src/dev/auth-workers.lisp" "%CALL-WITH-BLOCK-PIPELINE" 1 "%CALL-WITH-BLOCK-PIPELINE" "Signals the pipeline producer to stop and joins it in unwind.")
  ("src/dev/consensus.lisp" "MAP-ON-THREADS" 1 "MAP-ON-THREADS" "Joins the measurement worker group.")
  ("src/dev/evm-compile.lisp" "START-BACKGROUND-COMPILER" 1 "STOP-BACKGROUND-COMPILER" "Stops and joins the development compiler.")
  ("src/dev/r1-archive.lisp" "R1-ARCHIVE-RUN" 1 "R1-ARCHIVE-RUN" "Joins the archive worker group before reporting.")
  ("src/dev/rpc-corpus.lisp" "SCAN-ARCHIVE-SHAPES" 1 "SCAN-ARCHIVE-SHAPES" "Joins workers before collecting the record scan.")
  ("src/dev/rpc-corpus.lisp" "PARALLEL-MAP" 1 "PARALLEL-MAP" "Joins the bounded worker group.")
  ("src/dev/rpc-load.lisp" "LOAD-RUN" 1 "LOAD-RUN" "Joins the load-generator workers before aggregating results.")
  ("src/dev/statetest.lisp" "RUN-STATETESTS" 1 "RUN-STATETESTS" "Joins EEST workers before returning results.")))
