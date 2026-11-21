(in-package #:clempo.executor)

(defvar *chain* :mainnet
  "The chain spec (or its name) blocks are executed under: genesis, fork schedule, chain id.")

(define-condition block-mismatch (error)
  ((number :initarg :number :reader block-mismatch-number)
   (tx-index :initarg :tx-index :initform nil :reader block-mismatch-tx-index)
   (field :initarg :field :reader block-mismatch-field
          :documentation "The first field that differs: :gas-used, :receipts-root, :logs-bloom,
:state-root, :execution, or a receipt field (:status, :cumulative-gas-used, :logs) when the
reference receipts are known.")
   (expected :initarg :expected :initform nil :reader block-mismatch-expected)
   (actual :initarg :actual :initform nil :reader block-mismatch-actual)
   (detail :initarg :detail :initform nil :reader block-mismatch-detail)
   (fields :initarg :fields :initform '() :reader block-mismatch-fields
           :documentation "Every differing header field as (field expected actual).")
   (result :initarg :result :initform nil :reader block-mismatch-result
           :documentation "The BLOCK-RESULT when execution ran to the end, else NIL."))
  (:report (lambda (c s)
             (flet ((show (v) (if (typep v 'octets) (octets->hex v) v)))
               (if (eq (block-mismatch-field c) :execution)
                   (format s "block ~D~@[ tx ~D~]: execution failed (~(~A~)): ~A"
                           (block-mismatch-number c) (block-mismatch-tx-index c)
                           (block-mismatch-actual c) (block-mismatch-detail c))
                   (format s "block ~D~@[ tx ~D~]: ~(~A~) is ~A, expected ~A~@[ (~A)~]"
                           (block-mismatch-number c) (block-mismatch-tx-index c) (block-mismatch-field c)
                           (show (block-mismatch-actual c)) (show (block-mismatch-expected c))
                           (block-mismatch-detail c)))))))

(define-condition execution-failure (error)
  ((tx-index :initarg :tx-index :initform nil :reader execution-failure-tx-index)
   (reason :initarg :reason :reader execution-failure-reason))
  (:report (lambda (c s)
             (format s "~@[tx ~D: ~]~A" (execution-failure-tx-index c) (execution-failure-reason c))))
  (:documentation "A block could not be executed as the reference executes it."))

(define-condition invalid-transaction (execution-failure)
  ((kind :initarg :kind :initform :invalid :reader invalid-transaction-kind
         :documentation "A keyword: :nonce-too-low and :expiring-nonce-replay are the ones a
builder skips without marking the sender's lane invalid; everything else is :invalid or a
more specific keyword."))
  (:documentation "The reference rejects the transaction, which makes the block invalid."))

(define-condition insufficient-amm-liquidity (invalid-transaction)
  ((user-token :initarg :user-token :initform nil :reader insufficient-amm-liquidity-user-token)
   (validator-token :initarg :validator-token :initform nil
                    :reader insufficient-amm-liquidity-validator-token)
   (required :initarg :required :reader insufficient-amm-liquidity-required))
  (:documentation "FeePaymentError::InsufficientAmmLiquidity: fee collection found no FeeAMM
liquidity to swap REQUIRED (the collected fee) of USER-TOKEN into VALIDATOR-TOKEN. Both tokens
are NIL when the validator token could not be read (the reference's Option::None)."))

(define-condition unsupported-transaction (execution-failure) ()
  (:documentation "Execution needs a piece clempo does not have yet."))

(defun fail-tx (type index fmt &rest args)
  (error type :tx-index index :reason (apply #'format nil fmt args)))
