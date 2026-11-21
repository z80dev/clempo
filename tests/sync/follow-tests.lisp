(defpackage #:clempo.follow.tests
  (:use #:cl #:clempo.base #:clempo.follow)
  (:local-nicknames (#:blocks #:clempo.blocks) (#:hdr #:clempo.headers)
                    (#:trie #:clempo.trie) (#:archive #:clempo.archive)))

(in-package #:clempo.follow.tests)

(defun test-block (height parent &key (withdrawals-present t))
  (let* ((header (hdr:make-ethereum-header
                  :parent-hash parent :state-root trie:+empty-trie-root+
                  :transactions-root trie:+empty-trie-root+ :receipts-root trie:+empty-trie-root+
                  :ommers-hash (clempo.keccak:keccak256 (octets-of #xc0))
                  :beneficiary 0 :logs-bloom (make-octets 256) :difficulty 0 :number height
                  :gas-limit 500000000 :gas-used 0 :timestamp height :extra-data (make-octets 0)
                  :mix-hash (make-octets 32) :nonce (make-octets 8)
                  :base-fee-per-gas 10000000000 :withdrawals-root trie:+empty-trie-root+))
         (block (blocks:make-block :header (hdr:make-header :inner header :general-gas-limit 0
                                                            :shared-gas-limit 0 :timestamp-millis-part 0)
                                   :withdrawals-present-p withdrawals-present)))
    (values (blocks:encode-block block) (blocks:block-hash block))))

(defun verification-error (fn)
  (handler-case (progn (funcall fn) nil)
    (follow-error (e) (list (follow-error-reason e) (follow-error-height e)))))

(deftest authenticate-follow-batch ()
  (let ((anchor (make-octets 32)))
    (multiple-value-bind (one hash-one) (test-block 1 anchor)
      (multiple-value-bind (two hash-two) (test-block 2 hash-one)
        (let ((target (make-head :number 2 :hash hash-two)))
          (multiple-value-bind (blocks heads)
              (clempo.follow::verify-follow-batch 1 anchor target (vector one two))
            (is= 2 (length blocks))
            (is-equalp hash-one (head-hash (aref heads 0)))
            (is-equalp hash-two (head-hash (aref heads 1))))
          (flet ((check-batch (raw &optional (expected target))
                   (verification-error
                    (lambda () (clempo.follow::verify-follow-batch 1 anchor expected raw)))))
            (is-equalp '(:block-number 1) (check-batch (vector two one)))
            (is-equalp '(:parent-hash 2) (check-batch (vector one (test-block 2 anchor))))
            (is-equalp '(:body-root 2)
                       (check-batch (vector one (test-block 2 hash-one :withdrawals-present nil))))
            (is-equalp '(:canonical-hash 2)
                       (check-batch (vector one two) (make-head :number 2 :hash anchor)))))))))

(defstruct collecting-executor (heights nil) (certificates nil))

(defmethod apply-follow-block ((executor collecting-executor) block &key certificate)
  (push certificate (collecting-executor-certificates executor))
  (push (hdr:ethereum-header-number (hdr:header-inner (blocks:block-header block)))
        (collecting-executor-heights executor)))

(defstruct prefix-certifier available)

(defmethod certify-follow-batch ((certifier prefix-certifier) blocks previous-hash)
  (declare (ignore previous-hash))
  (let ((count (min (length blocks) (prefix-certifier-available certifier))))
    (values count (map 'simple-vector (lambda (block) (octets->hex (blocks:block-hash block)))
                       (subseq blocks 0 count)))))

(deftest follow-from-archive-through-verified-prefix ()
  (let* ((dir (format nil "/tmp/clempo-follow-test-~A/" (octets->hex (clempo.io:random-octets 8))))
         (anchor (make-octets 32))
         (executor (make-collecting-executor))
         (certifier (make-prefix-certifier :available 1))
         (follower (make-follower (make-head :number 0 :hash anchor) :archive-dir dir
                                 :use-archive t :executor executor :certifier certifier)))
    (unwind-protect
         (multiple-value-bind (one hash-one) (test-block 1 anchor)
           (multiple-value-bind (two hash-two) (test-block 2 hash-one)
             (let ((raw (make-array archive:+chunk-blocks+ :initial-element one))
                   (target (make-head :number 2 :hash hash-two)))
               (setf (aref raw 2) two)
               (archive:write-chunk 0 raw dir)
               (multiple-value-bind (head caught-up) (follow-to follower target)
                 (is= 1 (head-number head))
                 (is (not caught-up))
                 (is-equalp '(1) (collecting-executor-heights executor))
                 (is-equalp (list (octets->hex hash-one)) (collecting-executor-certificates executor))
                 (is= 1 (follower-certified follower)))
               (multiple-value-bind (head caught-up) (follow-to follower target)
                 (is caught-up)
                 (is-equalp hash-two (head-hash head))
                 (is-equalp '(2 1) (collecting-executor-heights executor))
                 (is-equalp (mapcar #'octets->hex (list hash-two hash-one))
                            (collecting-executor-certificates executor))
                 (is= 2 (follower-blocks follower))
                 (is= 2 (follower-certified follower)))
               (is-equalp '(:canonical-hash 2)
                          (verification-error
                           (lambda () (follow-to follower (make-head :number 2 :hash hash-one)))))
               (is-equalp '(2 1) (collecting-executor-heights executor)))))
      (when (probe-file dir) (sb-ext:delete-directory dir :recursive t)))))

(deftest follow-from-custom-block-source ()
  (let ((anchor (make-octets 32))
        (executor (make-collecting-executor))
        (served nil) (requests nil))
    (multiple-value-bind (one hash-one) (test-block 1 anchor)
      (multiple-value-bind (two hash-two) (test-block 2 hash-one)
        (let ((follower
                (make-follower (make-head :number 0 :hash anchor)
                               :executor executor
                               :block-source (lambda (from count)
                                               (push (list from count) requests)
                                               served)))
              (target (make-head :number 2 :hash hash-two)))
          (setf served (vector one))
          (is-equalp '(:block-source 1)
                     (verification-error (lambda () (follow-to follower target))))
          (setf served (vector two one))
          (is-equalp '(:block-number 1)
                     (verification-error (lambda () (follow-to follower target))))
          (is= 0 (head-number (follower-head follower)))
          (is (null (collecting-executor-heights executor)))
          (setf served (vector one two))
          (multiple-value-bind (head caught-up) (follow-to follower target)
            (is caught-up)
            (is-equalp hash-two (head-hash head)))
          (is-equalp '((1 2) (1 2) (1 2)) requests)
          (is-equalp '(2 1) (collecting-executor-heights executor)))))))

;;; Live following survives upstream failures, never retries an execution failure, and
;;; returns when asked to stop.

(defun head-json (number hash)
  (clempo.json:jobject "number" (quantity->hex number) "hash" (octets->hex hash :prefix t)
                       "stateRoot" (octets->hex trie:+empty-trie-root+ :prefix t)))

(defun call-with-two-block-chain (fail-polls function)
  "Call FUNCTION with (url source hash-two retries-cell): an upstream whose first FAIL-POLLS
tip requests fail with a JSON-RPC error, then report block 2."
  (let ((anchor (make-octets 32)) (polls 0))
    (multiple-value-bind (one hash-one) (test-block 1 anchor)
      (multiple-value-bind (two hash-two) (test-block 2 hash-one)
        (clempo.sync.fake-upstream:call-with-fake-upstream
         (lambda (method params)
           (declare (ignore method))
           (if (equal (first params) "latest")
               (if (<= (incf polls) fail-polls)
                   (error 'clempo.sync.fake-upstream:rpc-failure :code -32000 :message "upstream down")
                   (head-json 2 hash-two))
               (head-json 2 hash-two)))
         (lambda (url)
           (funcall function url (lambda (from count) (subseq (vector one two) (1- from) (+ from count -1)))
                    anchor hash-two)))))))

(deftest follow-live-retries-upstream-failures ()
  (call-with-two-block-chain
   3 (lambda (url source anchor hash-two)
       (let* ((executor (make-collecting-executor))
              (retries '())
              (follower (make-follower (make-head :number 0 :hash anchor) :rpc-url url
                                       :executor executor :block-source source)))
         (let ((head (follow-live follower :poll-seconds 0.001 :seconds 0
                                           :on-retry (lambda (n c) (push (cons n (princ-to-string c)) retries)))))
           (is-equalp hash-two (head-hash head))
           (is-equalp '(3 2 1) (mapcar #'car retries))
           (is (search "upstream down" (cdr (first retries))))
           (is-equalp '(2 1) (collecting-executor-heights executor)))))))

(deftest follow-live-bounded-gives-up ()
  (call-with-two-block-chain
   1000 (lambda (url source anchor hash-two)
          (declare (ignore hash-two))
          (let ((follower (make-follower (make-head :number 0 :hash anchor) :rpc-url url
                                         :block-source source)))
            (signals clempo.jsonrpc:rpc-error
              (follow-live follower :poll-seconds 0.001 :once t))))))

(defstruct failing-executor (calls 0))

(defmethod apply-follow-block ((executor failing-executor) block &key certificate)
  (declare (ignore block certificate))
  (incf (failing-executor-calls executor))
  (error "state root mismatch"))

(deftest follow-live-never-retries-execution ()
  (call-with-two-block-chain
   0 (lambda (url source anchor hash-two)
       (declare (ignore hash-two))
       (let* ((executor (make-failing-executor))
              (follower (make-follower (make-head :number 0 :hash anchor) :rpc-url url
                                       :executor executor :block-source source)))
         (signals execution-failed (follow-live follower :poll-seconds 0.001))
         (is= 1 (failing-executor-calls executor))))))

(deftest follow-live-stops-on-request ()
  (call-with-two-block-chain
   1000 (lambda (url source anchor hash-two)
          (declare (ignore hash-two))
          (let* ((asked 0)
                 (follower (make-follower (make-head :number 0 :hash anchor) :rpc-url url
                                          :block-source source)))
            ;; Live (no ONCE/SECONDS) and failing forever: only STOP ends it.
            (is= 0 (head-number (follow-live follower :poll-seconds 0.001
                                                      :stop (lambda () (> (incf asked) 5)))))))))
