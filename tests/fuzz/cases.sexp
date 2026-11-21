;;; Fuzz entries read by `clempo-dev fuzz` (src/dev/fuzz.lisp).
;;; (:known :target T :match S :owner A :note N) -- a routed, unfixed finding: failures of
;;;   T whose description contains S are reported as KNOWN and do not fail the run.
;;;   Delete the entry when the owner's fix lands.
;;; (:replay :target T :seed S :case N :note N) -- a case that once failed; every run
;;;   replays it and it must pass.
(
 (:known :target "rlp" :match "CONTROL-STACK-EXHAUSTED" :owner "SecurityAgent"
  :note "rlp-decode recurses per nesting level; 30,000 nested lists exhaust the stack")
 (:known :target "json" :match "CONTROL-STACK-EXHAUSTED" :owner "SecurityAgent"
  :note "parse-json recurses per level; one POST of 20,000 '[' kills rpc-serve")
 (:known :target "json" :match "UTF8" :owner "SecurityAgent"
  :note "invalid UTF-8 octets signal SBCL decoding errors, not JSON-ERROR")
 (:known :target "json" :match "PARSE-ERROR" :owner "SecurityAgent"
  :note "\\uZZZZ and a lone - reach parse-integer")
 (:known :target "json" :match "TYPE-ERROR" :owner "SecurityAgent"
  :note "\\u-001 and \\udbff\\uffff reach code-char out of range")
 (:known :target "json" :match "INDEX-ERROR" :owner "SecurityAgent"
  :note "a backslash at the end of input reads past the string")
 (:known :target "json" :match "writer output does not parse" :owner "SecurityAgent"
  :note "1e999 parses to an infinity that the writer prints as #.SB-EXT:...")
 (:known :target "json" :match "round trip differs" :owner "SecurityAgent"
  :note "lenient numbers and infinities")
)
