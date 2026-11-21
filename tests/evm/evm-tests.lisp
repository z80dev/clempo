(defpackage #:clempo.evm.tests
  (:use #:cl #:clempo.base #:clempo.state #:clempo.evm))

(in-package #:clempo.evm.tests)

(defun run-code (code &key (fork :osaka) (gas 100000) (input (make-octets 0)))
  "Run CODE (hex) as the code of account #xc0de called by #xca11; returns
\(values status gas-left output world)."
  (let* ((b (make-memory-backend)))
    (memory-backend-put-account b #xc0de :code (hex->octets code))
    (memory-backend-put-account b #xca11 :balance (expt 10 18))
    (let ((*world* (make-world b))
          (*spec* (make-spec fork))
          (*block* (make-block-env :number 1 :gas-limit 30000000)))
      (multiple-value-bind (status gas-left output)
          (call-message :call #xca11 #xc0de #xc0de 0 input gas 0 nil)
        (values status gas-left output *world*)))))

(deftest arithmetic-and-return ()
  ;; PUSH1 2 PUSH1 3 ADD PUSH1 0 MSTORE PUSH1 32 PUSH1 0 RETURN
  (multiple-value-bind (status gas-left output) (run-code "600260030160005260206000f3")
    (is= :return status)
    (is= 5 (octets->integer output))
    (is= (- 100000 (+ 3 3 3 3 3 3 3 3)) gas-left)))

(deftest wrapping-and-signed ()
  ;; 0 - 1 = 2^256-1 ; SDIV(-1, 1) ; store both
  (multiple-value-bind (status gas-left output)
      (run-code "600160000360005260016000190560205260406000f3")
    (declare (ignore gas-left))
    (is= :return status)
    (is= (1- (ash 1 256)) (octets->integer output :end 32))
    (is= (1- (ash 1 256)) (octets->integer output :start 32))))

(deftest storage-and-refund ()
  ;; SSTORE(0, 1) then SSTORE(0, 0): set then clear in one tx
  (multiple-value-bind (status gas-left output world)
      (run-code "60016000556000600055")
    (declare (ignore output))
    (is= :stop status)
    ;; cold set 22100 + warm reset-to-original 100, plus 4 pushes
    (is= (- 100000 (+ 12 22100 100)) gas-left)
    (is= 19900 (clempo.state:world-refund world))))

(deftest invalid-jump-consumes-gas ()
  (multiple-value-bind (status gas-left) (run-code "600356")
    (is= :invalid-jump status)
    (is= 0 gas-left)))

(deftest clz-only-in-osaka ()
  (is= :invalid-opcode (run-code "60011e" :fork :prague))
  (multiple-value-bind (status gas-left output) (run-code "60011e60005260206000f3")
    (declare (ignore gas-left))
    (is= :return status)
    (is= 255 (octets->integer output))))

;;; Halt order, from evm2 (tempo's interpreter, crates/evm2/src/interpreter): an op's
;;; static gas is charged first, then its stack is checked, then its body runs; CREATE and
;;; LOG check the static context before their stack, every other writer after it, and
;;; each before any dynamic gas or warming. Every case runs interpreted and compiled.

(defun run-frame-status (code gas &key static (fork :osaka) (cache (make-jit-cache)))
  "The status of CODE (hex) run at #xc0de with GAS, in a STATICCALL frame when STATIC,
interpreted and compiled (through CACHE); both must agree."
  (flet ((run (jit)
           (let ((b (make-memory-backend)))
             (memory-backend-put-account b #xc0de :code (hex->octets code) :balance 1000)
             (memory-backend-put-account b #xca11 :balance (expt 10 18))
             (let ((*world* (make-world b))
                   (*spec* (make-spec fork))
                   (*block* (make-block-env :number 1 :gas-limit 30000000))
                   (*jit* (and jit #'jit-run-frame))
                   (*jit-compiler* (and jit #'jit-compile-synchronously))
                   (*jit-threshold* 0)
                   (*jit-cache* cache))
               (values (call-message (if static :staticcall :call) #xca11 #xc0de #xc0de 0
                                     (make-octets 0) gas 0 static))))))
    (let ((interpreted (run nil)) (compiled (run t)))
      (is= interpreted compiled "~A at ~D gas: interpreted ~S, compiled ~S" code gas
           interpreted compiled)
      interpreted)))

(defun order-holds (code pushes-gas limit expect &rest keys)
  "True when CODE halts with (funcall EXPECT g) at every G from 0 to LIMIT gas left after
its leading pushes (PUSHES-GAS); the code is compiled once."
  (loop with cache = (make-jit-cache)
        for g from 0 to limit
        for want = (funcall expect g)
        for got = (apply #'run-frame-status code (+ pushes-gas g) :cache cache keys)
        unless (eq want got)
          do (is= want got "~A with ~D gas after its pushes" code g)
             (return nil)
        finally (return t)))

(deftest static-violation-before-gas-sstore ()
  ;; PUSH1 1 PUSH1 0 SSTORE: static context first, before the EIP-2200 sentry (2300) and
  ;; the cold slot (2100), at any gas
  (is (order-holds "6001600055" 6 2500 (constantly :static-violation) :static t))
  ;; the stack is checked before the context (static gas 0)
  (is (order-holds "55" 0 50 (constantly :stack-underflow) :static t))
  ;; and the sentry still applies outside a static frame
  (is= :out-of-gas (run-frame-status "6001600055" (+ 6 2300)))
  (is= :stop (run-frame-status "6001600055" (+ 6 22100))))

(deftest static-violation-order-of-writers ()
  (flet ((after (static-gas)
           (lambda (g) (if (< g static-gas) :out-of-gas :static-violation))))
    ;; TSTORE: 100 static, then stack, then context
    (is (order-holds "600160005d" 6 150 (after 100) :static t))
    (is (order-holds "5d" 0 150 (lambda (g) (if (< g 100) :out-of-gas :stack-underflow)) :static t))
    ;; LOG0..LOG4: 375 static, then context, then stack (a short stack is a violation too)
    (is (order-holds "60006000a0" 6 450 (after 375) :static t))
    (is (order-holds "a0" 0 450 (after 375) :static t))
    (is (order-holds "60006000a4" 6 450 (after 375) :static t))
    ;; CREATE and CREATE2: no static gas; the context before the stack and before the
    ;; 32,000, the initcode and the memory gas
    (is (order-holds "600060006000f0" 9 400 (constantly :static-violation) :static t))
    (is (order-holds "f0" 0 400 (constantly :static-violation) :static t))
    (is (order-holds "6000600060006000f5" 12 400 (constantly :static-violation) :static t))
    (is (order-holds "f5" 0 400 (constantly :static-violation) :static t))
    ;; CALL with value: 100 static (the warm access), then stack, then the context,
    ;; before memory, the cold surcharge and the 9,000 for value
    (is (order-holds "6000600060006000600161beef6000f1" 21 3000 (after 100) :static t))
    ;; without value it runs: a STATICCALL frame may CALL
    (is= :stop (run-frame-status "6000600060006000600061beef6000f100" (+ 21 2600 10)
                                 :static t))
    ;; SELFDESTRUCT: 5,000 static, then stack, then context, before the cold account
    (is (order-holds "61beefff" 3 5200 (after 5000) :static t))
    (is (order-holds "ff" 0 5200 (lambda (g) (if (< g 5000) :out-of-gas :stack-underflow))
                     :static t))))

(deftest static-gas-before-stack ()
  ;; evm2 charges an op's static gas before it checks its stack, for every op
  (is (order-holds "600101" 3 10 (lambda (g) (if (< g 3) :out-of-gas :stack-underflow))))
  (is (order-holds "31" 0 150 (lambda (g) (if (< g 100) :out-of-gas :stack-underflow))))
  (is (order-holds "6000f1" 3 150 (lambda (g) (if (< g 100) :out-of-gas :stack-underflow)))))

(deftest create-initcode-limit-before-memory ()
  ;; PUSH3 49153 PUSH1 0 PUSH1 0 CREATE: from Shanghai, initcode over the limit halts with
  ;; its own status before memory is expanded or anything is charged
  (is (order-holds "6200c00160006000f0" 9 100 (constantly :create-init-code-size-limit)))
  (is= :create-init-code-size-limit (run-frame-status "6200c00160006000f0" 10000000))
  (is= "max initcode size exceeded"
       (clempo.tracing::stop-message :create-init-code-size-limit :geth))
  ;; at the limit it runs: 49,152 zero bytes of initcode STOP and deploy nothing
  (is= :stop (run-frame-status "6200c00060006000f000" 10000000))
  ;; CREATE2 too
  (is= :create-init-code-size-limit (run-frame-status "60006200c00160006000f5" 10000000)))
