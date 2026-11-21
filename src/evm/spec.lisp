(in-package #:clempo.evm)

;;; Forks, the gas schedule as data, the spec object and the execution context.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *forks* '(:istanbul :berlin :london :paris :shanghai :cancun :prague :osaka)
    "Supported Ethereum forks, oldest first. A fork is represented by its index here.")

  (defun fork-index (fork)
    (etypecase fork
      (integer fork)
      (keyword (or (position fork *forks*) (error "unknown fork ~S" fork))))))

(defun fork-name (index) (nth index *forks*))

(defun parse-fork (name)
  "Fork index for a name such as \"Cancun\" (case-insensitive; \"Merge\" is Paris), or NIL."
  (let ((k (intern (string-upcase name) :keyword)))
    (position (if (eq k :merge) :paris k) *forks*)))

(defmacro current-fork ()
  "The running fork index; the interpreter shadows this with its local copy."
  '(spec-fork *spec*))

(defmacro fork>= (fork)
  "True when the running fork is FORK or later: (fork>= :berlin)."
  `(>= (the fixnum (current-fork)) ,(fork-index fork)))

(defmacro fork-if (fork then else)
  `(if (fork>= ,fork) ,then ,else))

;;; Gas parameters. Each is a slot of GAS-PARAMS with its Istanbul value and the forks
;;; that change it. A spec carries one GAS-PARAMS; the tempo layer copies and overrides.

(defmacro define-gas-params (&body params)
  (let ((names (mapcar #'first params)))
    `(progn
       (defstruct (gas-params (:copier copy-gas-params))
         ,@(loop for (name default) in params
                 collect `(,name ,default :type fixnum)))
       (defun gas-params-for-fork (fork)
         "The Ethereum gas schedule of FORK (index or keyword)."
         (let ((f (fork-index fork)))
           (declare (ignorable f))
           (make-gas-params
            ,@(loop for (name default . changes) in params
                    nconc (list (intern (symbol-name name) :keyword)
                                (let ((form default))
                                  (loop for (fork value) in changes
                                        do (setf form `(if (>= f ,(fork-index fork)) ,value ,form)))
                                  form))))))
       (defparameter *gas-param-names* ',names))))

(define-gas-params
  ;; state access
  (sload 800 (:berlin 100))               ; Istanbul SLOAD; warm SLOAD from Berlin
  (account-read 700 (:berlin 100))        ; BALANCE, EXTCODE*, CALL base; warm from Berlin
  (warm-read 100)
  (cold-sload 2100)
  (cold-account 2600)
  (sstore-set 20000)
  (sstore-reset 5000 (:berlin 2900))
  (sstore-set-refund 19200 (:berlin 19900))       ; 0 -> x -> 0 restore: set - warm read
  (sstore-reset-refund 4200 (:berlin 2800))       ; y -> x -> y restore: reset - warm read
  (sstore-clear-refund 15000 (:london 4800))
  (sstore-sentry 2300)
  ;; calls and accounts
  (call-value 9000)
  (call-stipend 2300)
  (new-account 25000)
  (new-account-selfdestruct 25000)
  (selfdestruct 5000)
  (selfdestruct-refund 24000 (:london 0))
  (create 32000)
  (code-deposit 200)
  (initcode-word 0 (:shanghai 2))
  (max-code-size 24576)
  (max-initcode-size 49152)
  ;; computation
  (exp-byte 50)
  (keccak 30)
  (keccak-word 6)
  (copy-word 3)
  (log 375)
  (log-topic 375)
  (log-data 8)
  (memory-word 3)
  (memory-quad-divisor 512)
  (blockhash 20)
  ;; transactions
  (tx-base 21000)
  (tx-create 32000)
  (tx-data-zero 4)
  (tx-data-nonzero 16)
  (tx-access-address 2400)
  (tx-access-slot 1900)
  (tx-floor-token 0 (:prague 10))
  (auth-empty-account 25000)                     ; EIP-7702 PER_EMPTY_ACCOUNT_COST
  (auth-refund 12500)                            ; refund when the authority exists
  (refund-quotient 2 (:london 5))
  (tx-gas-cap 0 (:osaka 16777216)))          ; EIP-7825; 0 = no cap

;;; Precompile providers: something that maps an address to a precompile.

(defstruct (precompile (:constructor make-precompile (name address fn)))
  (name nil :type symbol)
  (address 0 :type integer)
  (fn nil :type function))

(defgeneric find-precompile (provider address)
  (:documentation "The precompile at ADDRESS under PROVIDER, or NIL."))

(defgeneric precompile-addresses (provider)
  (:documentation "Every address PROVIDER answers for; warm at transaction start (EIP-2929)."))

(defmethod find-precompile ((provider hash-table) address)
  (values (gethash address provider)))

(defmethod precompile-addresses ((provider hash-table))
  (loop for k being the hash-keys of provider collect k))

(defmethod find-precompile ((provider null) address)
  (declare (ignore address))
  nil)

(defmethod precompile-addresses ((provider null)) '())

;;; The spec: everything that parameterises execution by chain and fork.

(defstruct (spec (:constructor %make-spec))
  (fork 0 :type fixnum)
  (gas (gas-params-for-fork 0) :type gas-params)
  (precompiles nil)
  (extra-opcodes (make-array 256 :initial-element nil) :type simple-vector)
  (sstore-hook nil :type (or null function))
  (blob-target 0 :type fixnum)
  (blob-max 0 :type fixnum)
  (blob-update-fraction 1 :type fixnum)
  (ext nil))

(defun make-spec (fork &key (gas (gas-params-for-fork fork))
                         (precompiles (ethereum-precompiles fork))
                         blob-target blob-max blob-update-fraction ext)
  "The Ethereum spec of FORK (keyword or index). EXTRA-OPCODES of the result can be set
for opcodes Ethereum leaves unassigned; SSTORE-HOOK replaces nothing but runs after the
host write (see the SSTORE op)."
  (let* ((f (fork-index fork))
         (bt (or blob-target (cond ((>= f (fork-index :prague)) 6) ((>= f (fork-index :cancun)) 3) (t 0))))
         (bm (or blob-max (cond ((>= f (fork-index :prague)) 9) ((>= f (fork-index :cancun)) 6) (t 0))))
         (bf (or blob-update-fraction
                 (cond ((>= f (fork-index :prague)) 5007716) (t 3338477)))))
    (%make-spec :fork f :gas gas :precompiles precompiles
                :blob-target bt :blob-max bm :blob-update-fraction bf :ext ext)))

;;; Block and transaction environment.

(defstruct (block-env (:constructor make-block-env
                          (&key (number 0) (timestamp 0) (coinbase 0) (gas-limit 0)
                             (base-fee 0) (prevrandao 0) (difficulty 0) (chain-id 1)
                             (excess-blob-gas 0) (blob-base-fee 1) (block-hash-fn nil)
                             ext)))
  (number 0 :type integer)
  (timestamp 0 :type integer)
  (coinbase 0 :type integer)
  (gas-limit 0 :type integer)
  (base-fee 0 :type integer)
  (prevrandao 0 :type integer)
  (difficulty 0 :type integer)
  (chain-id 1 :type integer)
  (excess-blob-gas 0 :type integer)
  (blob-base-fee 1 :type integer)
  (block-hash-fn nil :type (or null function))
  ext)

(defstruct (tx-env (:constructor make-tx-env (&key (origin 0) (gas-price 0) (blob-hashes #()))))
  (origin 0 :type integer)
  (gas-price 0 :type integer)
  (blob-hashes #() :type simple-vector))

(defvar *world* nil "The CLEMPO.STATE:WORLD being executed against.")
(defvar *spec* (%make-spec) "The running SPEC.")
(defvar *block* (make-block-env) "The running BLOCK-ENV.")
(defvar *tx-env* (make-tx-env) "The running TX-ENV.")

;;; Tracing. *TRACER* NIL selects the interpreter compiled without hooks.

(defvar *tracer* nil "A tracer object, or NIL for no tracing (no overhead).")

(defgeneric trace-step (tracer frame pc op gas depth sp stack)
  (:documentation "Called before each opcode with the gas left before it.")
  (:method (tracer frame pc op gas depth sp stack)
    (declare (ignore tracer frame pc op gas depth sp stack))))

(defgeneric trace-call-start (tracer kind from to value input gas depth)
  (:documentation "A message call or creation begins.")
  (:method (tracer kind from to value input gas depth)
    (declare (ignore tracer kind from to value input gas depth))))

(defgeneric trace-call-end (tracer status gas-left output depth)
  (:documentation "The innermost message call or creation ends.")
  (:method (tracer status gas-left output depth)
    (declare (ignore tracer status gas-left output depth))))
