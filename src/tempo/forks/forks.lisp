(defpackage #:clempo.forks
  (:use #:cl #:clempo.base)
  (:documentation "Tempo hardforks as ordered keywords, the active fork in *SPEC*, FORK-IF
and the fork-site registry that answers \"what changed in T8\", and the per-fork gas
schedule Tempo layers over Osaka (crates/hardfork: constants.rs, gas_params.rs).")
  (:export
   ;; the enum
   #:+forks+ #:fork #:fork-p #:fork-index #:fork-name #:parse-fork #:latest-fork
   #:fork>= #:fork> #:fork< #:fork<= #:fork-max
   #:*spec* #:spec>= #:spec< #:with-spec
   ;; gating
   #:fork-if #:since #:until
   #:fork-site #:fork-site-fork #:fork-site-kind #:fork-site-where #:fork-site-doc
   #:record-fork-site #:fork-sites #:*fork-site-context* #:with-fork-site-context
   ;; gas schedule
   #:gas-schedule #:gas #:schedule-for #:with-gas-parameters-only
   #:+sstore-create-cost+ #:+sstore-set-cost+ #:+storage-credit-value+
   #:+cold-sload-cost+ #:+warm-sload-cost+ #:+cold-account-access-cost+
   #:+log-gas+ #:+keccak256-base-gas+ #:+keccak256-word-gas+ #:keccak-gas
   #:+ecrecover-gas+ #:input-cost #:dedup-cost
   #:+t0-base-fee+ #:+t1-base-fee+ #:+t7-base-fee-cap+ #:+t7-base-fee-floor+
   #:+t7-base-fee-gas-target+ #:t7-next-block-base-fee
   #:+t1-general-gas-limit+ #:+t1-tx-gas-limit-cap+ #:+osaka-tx-gas-limit-cap+
   #:general-gas-limit #:shared-gas-limit #:tx-gas-limit-cap
   #:gas-existing-nonce-key #:gas-new-nonce-key
   #:expiring-nonce-set-capacity #:expiring-nonce-max-expiry-secs
   #:+l1-commitment-read-gas+
   #:+mainnet-chain-id+ #:+moderato-chain-id+))

(in-package #:clempo.forks)

;;; The enum. Order is TempoHardfork's declaration order (crates/hardfork/src/lib.rs).

(eval-when (:compile-toplevel :load-toplevel :execute)
(define-constant +forks+
  '(:genesis :t0 :t1 :t1a :t1b :t1c :t2 :t3 :t4 :t5 :t6 :t7 :t8 :t9 :t10 :t11 :t12 :t13
    :t14 :t15)
  "Every Tempo hardfork, oldest first.")

(deftype fork () `(member ,@+forks+))

(defun fork-p (x) (and (member x +forks+) t))

(macrolet ((def-index ()
             `(progn
                (declaim (inline fork-index))
                (defun fork-index (fork)
                  "Position of FORK in +FORKS+."
                  (ecase fork
                    ,@(loop for f in +forks+ for i from 0 collect `(,f ,i)))))))
  (def-index))

(defun latest-fork () (car (last +forks+))))

(defun fork-name (fork)
  "The reference's display name: Genesis, T0, T1A ..."
  (if (eq fork :genesis) "Genesis" (string-upcase (symbol-name fork))))

(defun parse-fork (name)
  "\"T1A\", \"t1a\" or :t1a -> :t1a. Signals on an unknown name."
  (let ((k (find (string name) +forks+ :key #'symbol-name :test #'string-equal)))
    (or k (error "unknown Tempo hardfork ~S" name))))

(defun fork-max (a b) (if (>= (fork-index a) (fork-index b)) a b))

(macrolet ((def-compare (name op)
             `(progn
                (declaim (inline ,name))
                (defun ,name (a b) (,op (fork-index a) (fork-index b)))
                (define-compiler-macro ,name (&whole w a b)
                  (if (keywordp b)
                      `(,',op (fork-index ,a) ,(fork-index b))
                      w)))))
  (def-compare fork>= >=)
  (def-compare fork> >)
  (def-compare fork< <)
  (def-compare fork<= <=))

(defvar *spec* :t13
  "The hardfork rules in force. Rebound per block by the executor, per case by tests.")
(declaim (type fork *spec*))

(defmacro spec>= (fork) `(fork>= *spec* ,fork))
(defmacro spec< (fork) `(fork< *spec* ,fork))

(defmacro with-spec ((fork) &body body)
  `(let ((*spec* ,fork)) ,@body))

;;; Fork sites. Every FORK-IF and every :since field or call records where it is, so
;;; (fork-sites :t8) lists what T8 changed.

(defstruct (fork-site (:constructor make-fork-site (fork kind where doc)))
  (fork nil :type fork)
  (kind nil :type keyword)            ; :branch, :field, :call, :precompile, ...
  (where nil)                         ; the enclosing definition, e.g. (tip20 transfer)
  (doc nil))

(defvar *fork-sites* (make-hash-table :test 'equal)
  "(fork kind where doc) -> fork-site; reloading a file replaces rather than duplicates.")

(defun record-fork-site (fork kind where &optional doc)
  (check-type fork fork)
  (setf (gethash (list fork kind where doc) *fork-sites*) (make-fork-site fork kind where doc))
  fork)

(defun fork-sites (&optional fork)
  "Recorded sites for FORK (all when NIL), sorted by fork then where."
  (let ((sites (loop for s being the hash-values of *fork-sites*
                     when (or (null fork) (eq (fork-site-fork s) fork)) collect s)))
    (sort sites (lambda (a b)
                  (let ((fa (fork-index (fork-site-fork a))) (fb (fork-index (fork-site-fork b))))
                    (if (/= fa fb)
                        (< fa fb)
                        (string< (prin1-to-string (fork-site-where a))
                                 (prin1-to-string (fork-site-where b)))))))))

(defvar *fork-site-context* nil
  "Names the definition a FORK-IF sits in when no lexical context does.")

(defmacro with-fork-site-context ((where) &body body)
  "Make FORK-IFs lexically inside BODY record WHERE (e.g. (tip20-token transfer))."
  `(symbol-macrolet ((%fork-site% ',where)) ,@body))

(defun site-context (env)
  (multiple-value-bind (expansion expanded-p) (macroexpand-1 '%fork-site% env)
    (or (and expanded-p (second expansion))
        *fork-site-context*
        (and *compile-file-truename* (pathname-name *compile-file-truename*)))))

(defmacro fork-if ((relation fork &key doc) then &optional (else nil else-p) &environment env)
  "Run THEN when the active fork satisfies RELATION (:since or :before) FORK, else ELSE.
Records the site at load time so FORK-SITES finds it."
  (check-type fork fork)
  (let ((test (ecase relation
                (:since `(fork>= *spec* ,fork))
                (:before `(fork< *spec* ,fork)))))
    `(progn
       (load-time-value (record-fork-site ,fork :branch ',(site-context env) ,doc) t)
       (if ,test ,then ,@(when else-p (list else))))))

(defmacro since (fork) `(fork>= *spec* ,fork))
(defmacro until (fork) `(fork< *spec* ,fork))

;;; Constants (crates/hardfork/src/constants.rs, evm2 interpreter/gas.rs).

(defconstant +mainnet-chain-id+ 4217)
(defconstant +moderato-chain-id+ 42431)

(defconstant +cold-sload-cost+ 2100)
(defconstant +warm-sload-cost+ 100)
(defconstant +cold-account-access-cost+ 2600)
(defconstant +log-gas+ 375)
(defconstant +keccak256-base-gas+ 30)
(defconstant +keccak256-word-gas+ 6)
(defconstant +ecrecover-gas+ 3000)

(defconstant +sstore-create-cost+ 250000 "TIP-1000 zero-to-nonzero SSTORE creation cost.")
(defconstant +sstore-set-cost+ 5000 "TIP-1060 SSTORE residual.")
(defconstant +storage-credit-value+ (- +sstore-create-cost+ +sstore-set-cost+))
(defconstant +l1-commitment-read-gas+ 10000)

(defconstant +t0-base-fee+ 10000000000)
(defconstant +t1-base-fee+ 20000000000)
(defconstant +t7-base-fee-cap+ 12000000000)
(defconstant +t7-base-fee-floor+ (floor +t7-base-fee-cap+ 20))
(defconstant +t7-base-fee-gas-target+ 10000000)
(defconstant +t1-general-gas-limit+ 30000000)
(defconstant +t1-tx-gas-limit-cap+ 30000000)
(defconstant +osaka-tx-gas-limit-cap+ 16777216)

(defun t7-next-block-base-fee (parent-base-fee parent-gas-used)
  "TIP-1067: EIP-1559's update (denominator 8, elasticity 1) against a fixed 10M target,
clamped to [floor, cap]."
  (let* ((target +t7-base-fee-gas-target+)
         (next (cond ((= parent-gas-used target) parent-base-fee)
                     ((> parent-gas-used target)
                      (+ parent-base-fee
                         (max 1 (floor (* parent-base-fee (- parent-gas-used target)) (* target 8)))))
                     (t (- parent-base-fee
                           (floor (* parent-base-fee (- target parent-gas-used)) (* target 8)))))))
    (min +t7-base-fee-cap+ (max +t7-base-fee-floor+ next))))

(defun general-gas-limit (fork) (when (fork>= fork :t1) +t1-general-gas-limit+))
(defun shared-gas-limit (fork block-gas-limit) (if (fork>= fork :t4) 0 (floor block-gas-limit 10)))
(defun tx-gas-limit-cap (fork) (if (fork>= fork :t1a) +t1-tx-gas-limit-cap+ +osaka-tx-gas-limit-cap+))

(defun gas-existing-nonce-key (fork)
  (+ +cold-sload-cost+ (- 5000 +cold-sload-cost+) (if (fork>= fork :t2) (* 2 +warm-sload-cost+) 0)))
(defun gas-new-nonce-key (fork)
  (+ +cold-sload-cost+ 20000 (if (fork>= fork :t2) (* 2 +warm-sload-cost+) 0)))
(defun expiring-nonce-set-capacity (fork) (if (fork>= fork :t11) 3000000 300000))
(defun expiring-nonce-max-expiry-secs (fork) (if (fork>= fork :t11) 300 30))

(declaim (inline keccak-gas))
(defun keccak-gas (len)
  (declare (type (integer 0 #.most-positive-fixnum) len))
  (+ +keccak256-base-gas+ (* +keccak256-word-gas+ (ceiling len 32))))

(defun input-cost (fork calldata-length)
  "Gas a Tempo precompile charges per 32-byte word of calldata (6, 30 from T11)."
  (* (ceiling calldata-length 32) (if (fork>= fork :t11) 30 6)))

(defun dedup-cost (fork item-count)
  "T11+ cost of a duplicate check over ITEM-COUNT items."
  (if (fork>= fork :t11) (* 20 item-count) 0))

;;; The gas schedule: evm2's Osaka GasParams with Tempo's overrides (gas_params.rs).
;;; amsterdam_eip8037_enabled (TIP-1016's regular/state split) is off on every Tempo network;
;;; the schedule here is the one with it off.

(defvar *gas-parameters* '()
  "Names of the GAS-SCHEDULE parameters, for GAS's compile-time check.")

(defmacro define-gas-schedule (&rest params)
  "PARAMS: (name osaka-default) pairs. Defines the GAS-SCHEDULE struct with one slot per
parameter, defaulting to Osaka's value."
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf *gas-parameters* ',(mapcar #'first params)))
     (defstruct (gas-schedule (:constructor %make-gas-schedule))
       ,@(loop for (name default) in params
               collect `(,name ,default :type (unsigned-byte 32))))))

(define-gas-schedule
  (warm-storage-read-cost 100)
  (cold-storage-additional-cost 2000)
  (cold-storage-cost 2100)
  (cold-account-additional-cost 2500)
  (sstore-static 100)
  (sstore-set-without-load-cost 19900)
  (sstore-reset-without-cold-load-cost 2800)
  (sstore-set-refund 19900)
  (sstore-reset-refund 2800)
  (sstore-clearing-slot-refund 4800)
  (call-stipend 2300)
  (max-refund-quotient 5)
  (logdata 8)
  (logtopic 375)
  (keccak256-per-word 6)
  (create 32000)
  (tx-create-cost 32000)
  (new-account-cost 25000)
  (new-account-cost-for-selfdestruct 25000)
  (code-deposit-cost 200)
  (tx-eip7702-per-empty-account-cost 25000)
  (tx-eip7702-auth-refund 12500))

(defun make-schedule (fork)
  (let ((g (%make-gas-schedule)))
    (when (fork>= fork :t1)
      ;; TIP-1000: storage and account creation in regular gas.
      (setf (gas-schedule-sstore-set-without-load-cost g) +sstore-create-cost+
            (gas-schedule-tx-create-cost g) 500000
            (gas-schedule-create g) 500000
            (gas-schedule-new-account-cost g) 250000
            (gas-schedule-new-account-cost-for-selfdestruct g) 250000
            (gas-schedule-code-deposit-cost g) 1000
            (gas-schedule-tx-eip7702-per-empty-account-cost g) 12500
            (gas-schedule-tx-eip7702-auth-refund g) 0))
    (when (fork>= fork :t7)
      ;; TIP-1060: the 245k creditable part moves to the storage-credit hook.
      (setf (gas-schedule-sstore-set-without-load-cost g) +sstore-set-cost+
            (gas-schedule-sstore-set-refund g) +sstore-set-cost+
            (gas-schedule-sstore-clearing-slot-refund g) 0
            (gas-schedule-max-refund-quotient g) 1))
    g))

(defvar *schedules*
  (let ((v (make-array (length +forks+))))
    (loop for f in +forks+ for i from 0 do (setf (aref v i) (make-schedule f)))
    v))
(declaim (type simple-vector *schedules*))

(declaim (inline schedule-for))
(defun schedule-for (fork)
  (the gas-schedule (svref *schedules* (fork-index fork))))

(defmacro gas (param &optional (fork '*spec*))
  "The gas parameter PARAM (a keyword naming a GAS-SCHEDULE slot) under FORK."
  (unless (member (symbol-name param) *gas-parameters* :key #'symbol-name :test #'string=)
    (error "unknown gas parameter ~S" param))
  `(,(intern (format nil "GAS-SCHEDULE-~A" (symbol-name param)) '#:clempo.forks)
    (schedule-for ,fork)))

(defmacro with-gas-parameters-only ((&rest keep-names) &body body)
  "Run BODY with copied fork schedules, charging only the named gas parameters.
KEEP-NAMES are literal keywords, validated at macroexpansion. The original schedules
are never changed; nested uses restrict the dynamically enclosing schedules."
  (dolist (name keep-names)
    (unless (and (keywordp name)
                 (member (symbol-name name) *gas-parameters*
                         :key #'symbol-name :test #'string=))
      (error "unknown gas parameter ~S" name)))
  (let ((schedule (gensym "SCHEDULE")))
    `(let ((*schedules*
             (map 'simple-vector
                  (lambda (,schedule)
                    (let ((,schedule (copy-gas-schedule ,schedule)))
                      (setf ,@(loop for name in *gas-parameters*
                                    unless (member (symbol-name name) keep-names
                                                   :key #'symbol-name :test #'string=)
                                      append `((,(intern (format nil "GAS-SCHEDULE-~A" name)
                                                         '#:clempo.forks)
                                                ,schedule) 0)))
                      ,schedule))
                  *schedules*)))
       ,@body)))
