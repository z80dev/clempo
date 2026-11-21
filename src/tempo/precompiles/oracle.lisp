(in-package #:clempo.oracle)

;;; One oracle case: seed a MEMORY-HOST with the case's accounts, storage, commitments and
;;; transient storage, warm the caller, run the call through CALL-PRECOMPILE on a
;;; METERED-STORAGE with the case's gas, then compare with the reference's outcome the way
;;; the corpus defines it: status always; output and gas for success and revert; refund,
;;; logs, changed slots and changed accounts for success.

(defun hexi (s) (if (or (null s) (eq s :null)) 0 (hex->integer s)))
(defun jget (o k) (clempo.json:jget o k))
(defun hexo (s) (if (or (null s) (eq s :null)) (make-octets 0) (hex->octets s)))

(defun seed-host (case)
  (let* ((block (jget case "block"))
         (host (make-memory-host
                :block (make-block-info :number (jget block "number")
                                        :timestamp (jget block "timestamp")
                                        :beneficiary (hexi (jget block "beneficiary"))
                                        :chain-id +mainnet-chain-id+)))
         (pre-storage (make-hash-table :test 'equal))
         (pre-code (make-hash-table))
         (pre-commitment (make-hash-table)))
    (loop for a across (jget case "accounts")
          for address = (hexi (jget a "address"))
          for code = (let ((c (jget a "code"))) (and (stringp c) (hexo c)))
          for commitment = (hexi (jget a "commitment"))
          do (memory-host-put-account host address :code code :commitment commitment)
             (setf (gethash address pre-code) (or code (make-octets 0))
                   (gethash address pre-commitment) commitment)
             (loop for kv across (or (jget a "storage") #())
                   for slot = (hexi (aref kv 0))
                   for value = (hexi (aref kv 1))
                   do (memory-host-put-storage host address slot value)
                      (setf (gethash (cons address slot) pre-storage) value)))
    (loop for tr across (or (jget case "transient") #())
          do (memory-host-put-transient host (hexi (jget tr "address")) (hexi (jget tr "slot"))
                                        (hexi (jget tr "value"))))
    (memory-host-warm-address host (hexi (jget (jget case "call") "caller")))
    (values host pre-storage pre-code pre-commitment)))

(defun status-name (status)
  (case status
    (:success "success") (:revert "revert") (:halt "halt") (:fatal "fatal")
    (t "unimplemented")))

(defun compare-logs (got want)
  (unless (= (length got) (length want))
    (return-from compare-logs (format nil "~D logs, expected ~D" (length got) (length want))))
  (loop for (address topics data) in got
        for w across want
        for i from 0
        unless (and (= address (hexi (jget w "address")))
                    (equal topics (map 'list #'hexi (jget w "topics")))
                    (octets= data (hexo (jget w "data"))))
          do (return (format nil "log ~D differs: got ~X ~{~X~^ ~} ~A, expected ~A ~A ~A" i address
                             topics (octets->hex data) (jget w "address") (jget w "topics")
                             (jget w "data")))))

(defun compare-storage (host pre want)
  (let ((post (make-hash-table :test 'equal))
        (changed (make-hash-table :test 'equal)))
    (maphash (lambda (address table)
               (maphash (lambda (slot value) (setf (gethash (cons address slot) post) value)) table))
             (memory-host-storage host))
    (maphash (lambda (k v) (unless (= v (gethash k pre 0)) (setf (gethash k changed) v))) post)
    (loop for w across want
          for key = (cons (hexi (jget w "address")) (hexi (jget w "slot")))
          for got = (multiple-value-bind (v found) (gethash key changed)
                      (if found v (gethash key post 0)))
          do (unless (= got (hexi (jget w "value")))
               (return-from compare-storage
                 (format nil "slot ~A/~A = ~X, expected ~A" (jget w "address") (jget w "slot") got
                         (jget w "value"))))
             (remhash key changed))
    (when (plusp (hash-table-count changed))
      (format nil "~D unexpected slot changes: ~{~A~^ ~}" (hash-table-count changed)
              (let ((l '()))
                (maphash (lambda (k v) (push (format nil "~X/~X=~X" (car k) (cdr k) v) l)) changed)
                (subseq l 0 (min 4 (length l))))))))

(defun compare-accounts (host pre-code pre-commitment want)
  (let ((got '()))
    (maphash (lambda (address info)
               (when (or (not (octets= (account-info-code info) (gethash address pre-code (make-octets 0))))
                         (/= (account-info-commitment info) (gethash address pre-commitment 0)))
                 (push address got)))
             (memory-host-accounts host))
    (unless (= (length got) (length want))
      (return-from compare-accounts
        (format nil "~D accounts changed, expected ~D" (length got) (length want))))
    (loop for w across want
          for address = (hexi (jget w "address"))
          for info = (memory-host-account host address)
          unless (and (member address got)
                      (= (account-info-code-hash info) (hexi (jget w "code_hash")))
                      (= (account-info-commitment info) (hexi (jget w "commitment"))))
            do (return (format nil "account ~A differs" (jget w "address"))))))

(defun run-oracle-case (case expected)
  "NIL when CASE (parsed JSON) reproduces EXPECTED, else a description of the difference."
  (let* ((fork (parse-fork (jget case "spec")))
         (call (jget case "call"))
         (code (hexi (jget call "code_address")))
         (want (jget expected "status")))
    (with-spec (fork)
      (let ((found (precompile-contract code)))
        (when (or (string= want "not-a-precompile") (null found))
          (return-from run-oracle-case
            (unless (eq (string= want "not-a-precompile") (null found))
              (format nil "precompile present ~A, expected ~A" (and found t) want))))
        (multiple-value-bind (host pre-storage pre-code pre-commitment) (seed-host case)
          (let ((backend (make-metered-storage host :gas-limit (jget call "gas")
                                                    :static (eq (jget call "static") :true))))
            (multiple-value-bind (status output gas refund)
                (with-storage (backend)
                  (call-precompile (hexi (jget call "target")) (hexo (jget call "data"))
                                   (hexi (jget call "caller")) :code-address code))
              (let ((got (status-name status)))
                (cond
                  ((string/= got want)
                   (format nil "status ~A (~A gas ~D), expected ~A (output ~A gas ~A)" got
                           (if (octets-p output) (octets->hex output) output) gas want
                           (jget expected "output") (jget expected "gas_used")))
                  ((member got '("halt" "fatal") :test #'string=) nil)
                  ((not (octets= output (hexo (jget expected "output"))))
                   (format nil "output ~A, expected ~A" (octets->hex output) (jget expected "output")))
                  ((/= gas (jget expected "gas_used"))
                   (format nil "gas ~D, expected ~D (delta ~D)" gas (jget expected "gas_used")
                           (- gas (jget expected "gas_used"))))
                  ((string/= got "success") nil)
                  ((/= refund (jget expected "gas_refunded"))
                   (format nil "refund ~D, expected ~D" refund (jget expected "gas_refunded")))
                  (t (or (compare-logs (host-logs host) (jget expected "logs"))
                         (compare-storage host pre-storage (jget expected "storage"))
                         (compare-accounts host pre-code pre-commitment (jget expected "accounts")))))))))))))

(defun octets-p (x) (typep x 'octets))

(defun run-oracle-lines (case-lines expected-lines &key (filter (constantly t)))
  "Run the cases of CASE-LINES (JSON strings) against EXPECTED-LINES. Returns
(values passed total failures) where FAILURES is a list of (name spec diff). A Lisp error
in a case counts as a failure. FILTER selects case names."
  (let ((passed 0) (total 0) (failures '()))
    (loop for cl in case-lines
          for el in expected-lines
          do (let* ((case (clempo.json:parse-json cl))
                    (expected (clempo.json:parse-json el))
                    (name (jget case "name")))
               (invariant (string= name (jget expected "name")) "case order ~A" name)
               (when (funcall filter name)
                 (incf total)
                 (let ((diff (handler-case (run-oracle-case case expected)
                               (error (e) (format nil "lisp error: ~A" e)))))
                   (if diff
                       (push (list name (jget case "spec") diff) failures)
                       (incf passed))))))
    (values passed total (nreverse failures))))
