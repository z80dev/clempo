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
