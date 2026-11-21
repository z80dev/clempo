(defpackage #:clempo.dev.spec-tests
  (:use #:cl #:clempo.base))

(in-package #:clempo.dev.spec-tests)

(clempo.contract:defcontract spec-generator-fixture (:prefix t)
  (value clempo.contract:u256))

(defun source-contracts ()
  (remove-if-not
   (lambda (contract)
     (clempo.dev::spec-source-p (clempo.contract:contract-source-file contract)
                              "src/tempo/precompiles/"))
   (clempo.contract:contracts)))

(defun output-content (outputs path)
  (or (cdr (assoc path outputs :test #'string=))
      (error "Missing generated output ~A" path)))

(defun occurrences (needle string)
  (loop for start = 0 then (+ position (length needle))
        for position = (search needle string :start2 start)
        while position count position))

(deftest spec-deterministic-print-context ()
  (let ((expected (clempo.dev::spec-outputs)))
    (is (source-contracts))
    (is (equal expected
               (let ((*package* (find-package :cl-user))
                     (*print-case* :upcase) (*print-pretty* t)
                     (*print-length* 1) (*print-level* 1))
                 (clempo.dev::spec-outputs))))
    (is (equal expected
               (let ((*package* (find-package :clempo.dev))
                     (*print-case* :capitalize))
                 (clempo.dev::spec-outputs))))))

(deftest spec-source-contracts-and-fixture-filter ()
  (let ((outputs (clempo.dev::spec-outputs)))
    (is (member 'spec-generator-fixture (clempo.contract:contracts)))
    (is (not (assoc "layouts/spec-generator-fixture.md" outputs :test #'string=)))
    (is (not (assoc "abi/spec-generator-fixture.json" outputs :test #'string=)))
    (dolist (contract (source-contracts))
      (let ((stem (clempo.dev::spec-file-stem contract)))
        (is (plusp (length (output-content outputs (format nil "layouts/~A.md" stem)))))
        (is (clempo.json:json-equal
             (clempo.contract:contract-abi contract)
             (clempo.json:parse-json
              (output-content outputs (format nil "abi/~A.json" stem)))))))))

(deftest spec-all-source-fork-sites ()
  (let ((outputs (clempo.dev::spec-outputs))
        (*package* (find-package :keyword))
        (*print-case* :downcase) (*print-pretty* nil))
    (dolist (fork clempo.forks:+forks+)
      (let* ((sites (remove-if-not
                     (lambda (site)
                       (clempo.dev::spec-source-p (clempo.forks:fork-site-file site) "src/"))
                     (clempo.forks:fork-sites fork)))
             (page (output-content outputs
                                   (format nil "forks/~A.md" (clempo.dev::spec-file-stem fork)))))
        (is= (length sites) (occurrences "## Site " page))
        (dolist (site sites)
          (is (search (clempo.dev::spec-fork-site-markdown site) page))
          (is (search (format nil "Relation: `~S`" (clempo.forks:fork-site-relation site)) page)))))))

(deftest spec-distinct-branch-positions-and-relations ()
  (let ((template (find-if (lambda (site)
                             (and (eq :branch (clempo.forks:fork-site-kind site))
                                  (clempo.dev::spec-source-p
                                   (clempo.forks:fork-site-file site) "src/")))
                           (clempo.forks:fork-sites))))
    (is template)
    (let ((clempo.forks::*fork-sites* (make-hash-table :test 'equal)))
      ;; Copies retain real definition metadata but exercise distinct branch positions.
      (loop for relation in '(:since :before :until) for index from 1
            for site = (copy-structure template)
            do (setf (clempo.forks:fork-site-relation site) relation
                     (clempo.forks:fork-site-form-path site) (list index)
                     (gethash index clempo.forks::*fork-sites*) site))
      (let ((fixture (copy-structure template)))
        (setf (clempo.forks:fork-site-file fixture) "tests/dev/spec-tests.lisp"
              (gethash :fixture clempo.forks::*fork-sites*) fixture))
      (let* ((outputs (clempo.dev::spec-outputs))
             (page (output-content
                    outputs (format nil "forks/~A.md"
                                    (clempo.dev::spec-file-stem
                                     (clempo.forks:fork-site-fork template))))))
        (is= 3 (occurrences "## Site " page))
        (loop for relation in '(":since" ":before" ":until") for index from 1
              do (is (search (format nil "Relation: `~A`" relation) page))
                 (is (search (format nil "Nested form path: `(~D)`" index) page)))
        (is (not (search "tests/dev/" page)))))))

(defun file-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defun put-test-file (root name)
  (let ((path (merge-pathnames name root)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-line "preserve unrelated content" out))))

(deftest spec-reconciles-owned-files-and-repeats-bytes ()
  (uiop:with-temporary-file (:pathname anchor :stream stream)
    (declare (ignore stream))
    (let ((root (uiop:ensure-directory-pathname
                 (concatenate 'string (namestring anchor) "-spec/")))
          (stale '("layouts/stale-output.md" "forks/stale-output.md" "abi/stale-output.json"))
          (unrelated '("README.md" "layouts/keep.txt" "abi/keep.md"
                       "forks/keep.json" "layouts/nested/keep.md")))
      (unwind-protect
           (progn
             (dolist (name (append stale unrelated)) (put-test-file root name))
             (let ((preserved (mapcar (lambda (name) (file-bytes (merge-pathnames name root)))
                                      unrelated)))
               ;; A root without its final slash must still be treated as a directory.
               (clempo.dev::generate-spec (string-right-trim "/" (namestring root)))
               (dolist (name stale) (is (not (probe-file (merge-pathnames name root)))))
               (loop for name in unrelated for bytes in preserved
                     do (is (equalp bytes (file-bytes (merge-pathnames name root)))))
               (let* ((outputs (clempo.dev::spec-outputs))
                      (first (mapcar (lambda (entry) (file-bytes (merge-pathnames (car entry) root)))
                                     outputs)))
                 (clempo.dev::generate-spec root)
                 (loop for entry in outputs for bytes in first
                       do (is (equalp bytes (file-bytes (merge-pathnames (car entry) root))))))))
        (when (probe-file root)
          (uiop:delete-directory-tree root :validate t))))))

(deftest spec-source-call-abi-gates ()
  (let ((outputs (clempo.dev::spec-outputs))
        (gated 0))
    (dolist (contract (source-contracts))
      (let* ((json (clempo.json:parse-json
                    (output-content outputs
                                    (format nil "abi/~A.json"
                                            (clempo.dev::spec-file-stem contract)))))
             (calls (clempo.json:jget json "calls")))
        (maphash
         (lambda (selector entry)
           (declare (ignore selector))
           (let* ((signature (clempo.contract::call-entry-signature entry))
                  (call (find signature calls :test #'string=
                              :key (lambda (call) (clempo.json:jget call "signature")))))
             (is call)
             (loop for key in '("since" "until")
                   for fork in (list (clempo.contract::call-entry-since entry)
                                     (clempo.contract::call-entry-until entry))
                   do (when fork (incf gated))
                      (is= (if fork (string-downcase (symbol-name fork)) :null)
                           (clempo.json:jget call key)))))
         (clempo.contract::contract-calls contract))))
    (is (plusp gated))))

(deftest spec-reflects-changed-contract-abi ()
  (let* ((contract (find-if
                    (lambda (contract)
                      (plusp (hash-table-count (clempo.contract::contract-calls contract))))
                    (source-contracts))))
    (is contract)
    (let* ((calls (clempo.contract::contract-calls contract))
           (selector (loop for key being the hash-keys of calls minimize key))
           (entry (gethash selector calls))
           (path (format nil "abi/~A.json" (clempo.dev::spec-file-stem contract)))
           (before (output-content (clempo.dev::spec-outputs) path)))
      ;; Exercise the actual contract registry, restoring it even if an assertion fails.
      (unwind-protect
           (progn
             (remhash selector calls)
             (let* ((after (output-content (clempo.dev::spec-outputs) path))
                    (json (clempo.json:parse-json after)))
               (is (not (string= before after)))
               (is (not (find (clempo.contract::call-entry-signature entry)
                              (clempo.json:jget json "calls") :test #'string=
                              :key (lambda (call) (clempo.json:jget call "signature")))))))
        (setf (gethash selector calls) entry))
      (is= before (output-content (clempo.dev::spec-outputs) path)))))

(deftest spec-prints-addresses-as-hex ()
  (is (string= "(0xaacc000000000000000000000000000000000000 3 \"x\")"
               (format nil "~S" (clempo.dev::spec-hex-addresses
                                 (list #xAACC000000000000000000000000000000000000 3 "x")))))
  ;; Every generated fork page names addresses in hex, never as long decimal integers.
  (let ((outputs (clempo.dev::spec-outputs)))
    (dolist (entry outputs)
      (when (search "forks/" (car entry))
        (is (not (search "configurable-account 9" (cdr entry))))))
    (is (search "0xaacc000000000000000000000000000000000000" (output-content outputs "forks/t14.md")))
    ;; T1B's executor gates are listed (the page once read "No recorded source sites").
    (is (search "src/tempo/executor/aa.lisp" (output-content outputs "forks/t1b.md")))))
(defun run-score-test (rung &rest options)
  (let ((*standard-output* (make-string-output-stream))
        (*error-output* (make-string-output-stream)))
    (let ((code (clempo.dev:run-command "score" (list* "-rung" rung "-json" options))))
      (values code (get-output-stream-string *standard-output*)
              (get-output-stream-string *error-output*)))))

(deftest score-missing-inputs-are-not-zero-scores ()
  (dolist (example '(("R1" "fetch-r1" "-chain" "clempo-test-missing-r1")
                     ("R3" "replay -result")
                     ("R3e" "fixtures_develop" "-fixtures" "/nonexistent/clempo-fixtures/")
                     ("R4" "-run-dir")
                     ("R6-devnet" "scripts/devnet")
                     ("R7" "-range")))
    (destructuring-bind (rung guidance &rest args) example
      (multiple-value-bind (code output diagnostic) (apply #'run-score-test rung args)
        (is= 2 code)
        (is= "" output)
        (is (search guidance diagnostic))))))

(deftest score-replay-checks-stored-head-against-archive ()
  (uiop:with-temporary-file (:pathname anchor :stream out)
    (declare (ignore out))
    (let* ((directory (uiop:ensure-directory-pathname (concatenate 'string (namestring anchor) "-score/")))
           (datadir (merge-pathnames "store/" directory))
           (archive (namestring (merge-pathnames "archive/" directory)))
           (report (namestring (merge-pathnames "replay.json" directory))))
      (unwind-protect
           (let ((store (clempo.store:open-store datadir :genesis #'clempo.executor:load-genesis)))
             (let ((root (clempo.store:db-root-hash (clempo.store:store-db store))))
               (clempo.store:close-store store)
               (labels ((write-header (state-root)
                          (clempo.archive:write-chunk
                           0 (vector (clempo.rlp:rlp-encode
                                      (list (clempo.chainspec:genesis-header :mainnet :state-root state-root)
                                            nil nil)))
                           archive))
                        (score ()
                          (run-score-test "R3" "-result" report "-datadir" (namestring datadir) "-archive" archive)))
                 (with-open-file (stream report :direction :output)
                   (write-line "{\"r3_height\":0,\"last\":0,\"stop\":null}" stream))
                 (write-header root)
                 (multiple-value-bind (code output) (score)
                   (is= 0 code)
                   (is= 0 (clempo.json:jget (clempo.json:parse-json output) "verified_head")))
                 (write-header (make-octets 32))
                 (multiple-value-bind (code output diagnostic) (score)
                   (is= 2 code)
                   (is= "" output)
                   (is (search "does not match" diagnostic))))))
        (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(deftest score-r7-runs-root-checked-bench ()
  (uiop:with-temporary-file (:pathname anchor :stream out)
    (declare (ignore out))
    (let* ((dir (uiop:ensure-directory-pathname (concatenate 'string (namestring anchor) "-bench/")))
           (archive (namestring (merge-pathnames "archive/" dir)))
           (report (namestring (merge-pathnames "bench.json" dir)))
           (fixture (asdf:system-relative-pathname "clempo-dev" "../../tests/tempo/executor/data/block-1.hex"))
           (raw (hex->octets (string-trim '(#\Space #\Tab #\Return #\Newline)
                                          (clempo.io:read-file-string fixture))))
           (genesis (clempo.rlp:rlp-encode (list (clempo.chainspec:genesis-header :mainnet) nil nil)))
           (clempo.archive::*archive-dir* archive)
           (clempo.dev::*bench-ranges* '(("early" 1 1))))
      (unwind-protect
           (progn
             (clempo.archive:write-chunk 0 (vector genesis raw) archive)
             (multiple-value-bind (code output)
                 (run-score-test "R7" "-range" "early" "-result" report "-sender-threads" "1")
               (let ((result (clempo.json:parse-json output)))
                 (is= 0 code)
                 (is (eq :true (clempo.json:jget result "complete")))
                 (is= 1 (clempo.json:jget result "blocks"))
                 (is (plusp (clempo.json:jget result "score"))))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))
