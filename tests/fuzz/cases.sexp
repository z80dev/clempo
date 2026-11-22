;;; Fuzz entries read by `clempo-dev fuzz` (src/dev/fuzz.lisp).
;;; (:known :target T :match S :owner A :note N) -- a routed, unfixed finding: failures of
;;;   T whose description contains S are reported as KNOWN and do not fail the run.
;;;   Delete the entry when the owner's fix lands.
;;; (:replay :target T :seed S :case N :note N) -- a case that failed before its fix;
;;;   every run replays it and it must pass. Case inputs depend on the generators in
;;;   src/dev/fuzz.lisp: a generator change re-derives these from the old codec.
(
 ;; JSON and RLP findings fixed in 23fffe0; each case failed on the
 ;; codec of eade5dc with the current generators.
 (:replay :target "json" :seed 1 :case 3 :note "CONTROL-STACK-EXHAUSTED: deep nesting killed rpc-serve")
 (:replay :target "json" :seed 1 :case 0 :note "INVALID-UTF8-STARTER-BYTE")
 (:replay :target "json" :seed 1 :case 121 :note "INVALID-UTF8-CONTINUATION-BYTE")
 (:replay :target "json" :seed 1 :case 33 :note "END-OF-INPUT-IN-CHARACTER")
 (:replay :target "json" :seed 1 :case 21 :note "SIMPLE-PARSE-ERROR from parse-integer")
 (:replay :target "json" :seed 1 :case 79 :note "INVALID-ARRAY-INDEX-ERROR: backslash at the end")
 (:replay :target "json" :seed 2 :case 1205 :note "SIMPLE-READER-ERROR from the Lisp reader")
 (:replay :target "json" :seed 2 :case 1373 :note "writer printed an infinity as #.SB-EXT:...")
 (:replay :target "json" :seed 2 :case 2087 :note "TYPE-ERROR: code-char out of range")
 (:replay :target "rlp" :seed 1 :case 8 :note "CONTROL-STACK-EXHAUSTED: deep nesting")
 (:replay :target "abi" :seed 3 :case 2214
  :note "TYPE-ERROR: a huge offset word reached DECODER-NEXT as a bignum; fixed in c740f3c")
)
