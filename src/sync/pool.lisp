(defpackage #:clempo.pool-service
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:pool #:clempo.txpool) (#:ex #:clempo.executor))
  (:documentation "A transaction pool attached to a live chain, shared by the follower node
and the validator. Admission validates against the canonical head's state and block-env.
Every committed block records its beneficiary's fee-token preference (the window of the
last ten heads Rust's AMM liquidity cache keeps) and revalidates every entry, dropping
mined, stale, replaced, expired and no longer payable transactions. All calls are locked.")
  (:export #:pool-service #:make-pool-service #:pool-service-block #:pool-service-head #:pool-service-submit
           #:pool-service-transactions #:pool-service-clear #:pool-service-count
           #:pool-service-removed #:pool-service-remove #:with-pool-service))

(in-package #:clempo.pool-service)

(defconstant +validator-window+ 10)

(defstruct (pool-service (:constructor %make-pool-service (pool chain)))
  (pool nil :read-only t)
  (chain :mainnet :read-only t)
  (lock (sb-thread:make-mutex :name "transaction pool"))
  (tokens '())          ; validator fee tokens of the last ten heads, newest first
  (env nil)             ; block-env of the canonical head
  (removed 0 :type integer))

(defun make-pool-service (&rest pool-options &key (chain :mainnet) &allow-other-keys)
  "A locked pool for CHAIN. The other POOL-OPTIONS go to CLEMPO.TXPOOL:MAKE-POOL."
  (let ((options (loop for (k v) on pool-options by #'cddr unless (eq k :chain) append (list k v))))
    (%make-pool-service (apply #'pool:make-pool options) chain)))

(defun reader (service db)
  (pool:make-state-reader db (pool-service-env service)
                          :recent-validator-tokens (pool-service-tokens service)))

(defun pool-service-block (service db header)
  "Note the committed head HEADER whose post-state is DB. Returns the removals (hash . reason)."
  (pool-service-head service db (ex:header-block-env header :chain (pool-service-chain service))))

(defun pool-service-head (service db env)
  "Note a head by its block-env ENV (its post-state is DB): record the beneficiary's fee
token in the ten-head window and revalidate every entry. Returns the removals."
  (let ((token (pool:validator-fee-token db env (ex:block-env-beneficiary env))))
    (clempo.lock:with-lock ((pool-service-lock service))
      (setf (pool-service-env service) env
            (pool-service-tokens service)
            (let ((tokens (remove token (pool-service-tokens service))))
              (cons token (subseq tokens 0 (min (1- +validator-window+) (length tokens))))))
      (let ((pool (pool-service-pool service)))
        (if (zerop (pool:pool-count pool))
            '()
            (let ((removed (pool:pool-revalidate pool (reader service db))))
              (incf (pool-service-removed service) (length removed))
              removed))))))

(defun pool-service-submit (service tx db)
  "Validate TX against DB (the state of the last noted head) and admit it. Returns
\(values hash status) with STATUS :PENDING or :QUEUED; signals POOL:POOL-ERROR on rejection."
  (clempo.lock:with-lock ((pool-service-lock service))
    (unless (pool-service-env service) (error 'pool:pool-error :code :not-ready))
    (multiple-value-bind (admitted status)
        (pool:submit-transaction (pool-service-pool service) tx (reader service db))
      (values (clempo.transactions:transaction-hash admitted) status))))

(defun pool-service-transactions (service)
  "Every pooled transaction as (tx sender pending-p), in submission order."
  (clempo.lock:with-lock ((pool-service-lock service))
    (pool:pool-snapshot (pool-service-pool service))))

(defun pool-service-count (service)
  (clempo.lock:with-lock ((pool-service-lock service))
    (pool:pool-count (pool-service-pool service))))

(defun pool-service-clear (service)
  "Remove every pooled transaction; returns how many."
  (clempo.lock:with-lock ((pool-service-lock service))
    (pool:pool-clear (pool-service-pool service))))

(defmacro with-pool-service ((pool service) &body body)
  "Run BODY with POOL bound to SERVICE's pool under its lock (for payload building)."
  (let ((s (gensym)))
    `(let ((,s ,service))
       (clempo.lock:with-lock ((pool-service-lock ,s))
         (let ((,pool (pool-service-pool ,s))) ,@body)))))


(defun pool-service-remove (service hash)
  "Drop one transaction by hash; returns it or NIL."
  (clempo.lock:with-lock ((pool-service-lock service))
    (pool:pool-remove-transaction (pool-service-pool service) hash)))