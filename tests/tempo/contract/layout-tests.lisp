(defpackage #:clempo.contract.layout-tests
  (:use #:cl #:clempo.base #:clempo.contract)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:export #:layout-fixture-mismatches))

(in-package #:clempo.contract.layout-tests)

;;; Every crates/precompiles/tests/storage_tests/solidity/testdata/*.layout.json (copied to
;;; data/layouts, see data/README.md) against the clempo layout of the same contract,
;;; compared as the reference's storage_conformance.rs does: (slot, offset, bytes) per
;;; label, and struct members per label relative to the struct's slot.

(defun fixture-json (name)
  (clempo.json:parse-json
   (clempo.io:read-file-string
    (asdf:system-relative-pathname "clempo-tempo-contract/tests"
                                   (format nil "../../../tests/tempo/contract/data/layouts/~A.layout.json" name)))))

(defun solc-layout (json)
  (let ((contract (cdr (first (cdr (clempo.json:jget json "contracts"))))))
    (clempo.json:jget contract "storage-layout")))

(defun solc-type (layout id) (clempo.json:jget (clempo.json:jget layout "types") id))

(defun struct-type-of (layout id)
  "The struct type behind ID (itself, a mapping's value or a dynamic array's base)."
  (let ((ty (solc-type layout id)))
    (cond ((string= (clempo.json:jget ty "encoding") "mapping")
           (struct-type-of layout (clempo.json:jget ty "value")))
          ((and (string= (clempo.json:jget ty "encoding") "dynamic_array"))
           (struct-type-of layout (clempo.json:jget ty "base")))
          ((clempo.json:jget ty "members") ty))))

(defun struct-spec-of (spec)
  "The DEFSTORABLE name behind a storable SPEC."
  (cond ((and (symbolp spec) (get spec 'clempo.contract::storable-layout)) spec)
        ((and (consp spec) (string= (symbol-name (first spec)) "MAPPING")) (struct-spec-of (third spec)))
        ((and (consp spec) (member (symbol-name (first spec)) '("VEC" "ARRAY") :test #'string=))
         (struct-spec-of (second spec)))))

(defun compare-entries (layout entries fields context)
  (let ((errors '()))
    (loop for e across entries
          for label = (clempo.json:jget e "label")
          for f = (find label fields :key #'field-label :test #'string=)
          do (if (null f)
                 (push (format nil "~A.~A exists in Solidity but not in clempo" context label) errors)
                 (let ((slot (parse-integer (clempo.json:jget e "slot")))
                       (offset (clempo.json:jget e "offset"))
                       (bytes (parse-integer (clempo.json:jget (solc-type layout (clempo.json:jget e "type"))
                                                               "numberOfBytes"))))
                   (unless (equal (list slot offset bytes)
                                  (list (field-slot f) (field-offset f) (type-bytes (field-type f))))
                     (push (format nil "~A.~A: solc (~D ~D ~D), clempo (~D ~D ~D)" context label
                                   slot offset bytes (field-slot f) (field-offset f)
                                   (type-bytes (field-type f)))
                           errors))
                   (let ((sty (struct-type-of layout (clempo.json:jget e "type")))
                         (sname (struct-spec-of (field-type f))))
                     (when (and sty sname)
                       (setf errors
                             (append (compare-entries layout (clempo.json:jget sty "members")
                                                      (layout-fields sname)
                                                      (format nil "~A.~A" context label))
                                     errors)))))))
    (loop for f in fields
          unless (find (field-label f) entries :key (lambda (e) (clempo.json:jget e "label"))
                                               :test #'string=)
            do (push (format nil "~A.~A exists in clempo but not in Solidity" context (field-label f))
                     errors))
    errors))

(defun layout-fixture-mismatches (fixture contract)
  "Differences between the solc layout FIXTURE (a name under data/layouts) and CONTRACT."
  (let ((layout (solc-layout (fixture-json fixture))))
    (compare-entries layout (clempo.json:jget layout "storage") (layout-fields contract)
                     (symbol-name contract))))

;;; The reference's generic layout tests (tests/storage_tests/solidity/primitives.rs).

(defcontract basic-types (:prefix t)
  (field-a u256) (field-b address) (field-c bool) (field-d u64))

(defcontract mixed-slots (:prefix t)
  (field-a u256) (field-c u256))

(defcontract arrays (:prefix t)
  (field-a u256)
  (large-array (array u256 5))
  (field-b u256)
  (nested-array (array (array u8 4) 8))
  (another-nested-array (array (array u16 2) 6))
  (u96-array5 (array u96 5) :label "u96Array5")
  (i96-array5 (array i96 5) :label "i96Array5"))

(defcontract fixed-bytes (:prefix t)
  (field-a u256)
  (bytes4-field b4 :label "bytes4Field")
  (bytes16-field b16 :label "bytes16Field")
  (bytes10-field b10 :label "bytes10Field")
  (field-b u256))

(defcontract mappings (:prefix t)
  (field-a u256)
  (address-mapping (mapping address u256))
  (uint-mapping (mapping u64 u256)))

(defstorable test-block (field1 u256) (field2 u256) (field3 u64))

(defcontract structs (:prefix t)
  (field-a u256)
  (block-data test-block)
  (field-b u256))

(defenum policy-type whitelist blacklist compound)

(defcontract enums (:prefix t)
  (field-a u16) (field-b policy-type) (field-c address))

(defcontract double-mappings (:prefix t)
  (field-a u256)
  (account-role (mapping address (mapping b256 bool)))
  (allowances (mapping address (mapping address u256))))

(defstorable packed-two-slot (value u256) (timestamp u64) (nonce u32) (owner address))
(defstorable packed-three-slot
  (value u256) (timestamp u64) (start-time u64) (end-time u64) (nonce u64) (owner address) (active bool))

(defcontract multi-slot-arrays (:prefix t)
  (dyn-two-slot (vec packed-two-slot))
  (dyn-three-slot (vec packed-three-slot)))

(defmacro deflayout-test (name fixture contract)
  `(deftest ,name ()
     (let ((errors (layout-fixture-mismatches ,fixture ',contract)))
       (is (null errors) "~{~A~^~%~}" errors))))

(deflayout-test basic-types-layout "basic_types" basic-types)
(deflayout-test mixed-slots-layout "mixed_slots" mixed-slots)
(deflayout-test arrays-layout "arrays" arrays)
(deflayout-test fixed-bytes-layout "fixed_bytes" fixed-bytes)
(deflayout-test mappings-layout "mappings" mappings)
(deflayout-test structs-layout "structs" structs)
(deflayout-test enums-layout "enum" enums)
(deflayout-test double-mappings-layout "double_mappings" double-mappings)
(deflayout-test multi-slot-arrays-layout "multi_slot_arrays" multi-slot-arrays)

(deftest manual-and-base-slots ()
  ;; tests/storage_tests/layouts.rs: a manual slot leaves the automatic chain alone; a
  ;; base slot restarts it.
  (let ((fields (clempo.contract::allocate-fields
                 '((a u8) (b u256 :slot 10) (c u8) (d u8 :base-slot 5) (e u8)) :contract t)))
    (is= '((0 0) (10 0) (0 1) (5 0) (5 1))
         (mapcar (lambda (f) (list (field-slot f) (field-offset f))) fields))
    ;; access looks at declaration neighbours only (gen_layout_ctx_expr), so A and C share
    ;; slot 0 yet both write it whole
    (is= '(:full :full :full :packed :packed) (mapcar #'field-access fields))))
