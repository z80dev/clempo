(defpackage #:clempo.dev.evmbench
  (:use #:cl #:clempo.base #:clempo.dev)
  (:documentation "clempo-dev evm-bench: interpreter throughput on synthetic loops."))

(in-package #:clempo.dev.evmbench)

;;; Each program loops on a counter left on the stack; the body differs.
;;;   PUSH4 n  JUMPDEST  <body>  PUSH1 1 SWAP1 SUB  DUP1 PUSH1 5 JUMPI  STOP
(defparameter *bodies*
  '(("arith"   "808002800181900450" "small-word MUL/ADD/DIV")
    ("bignum"  "7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8002807f7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0950"
     "256-bit MUL/MULMOD")
    ("memory"  "806040526040518060205250602060402050" "MSTORE/MLOAD/KECCAK256")
    ("storage" "8060005560005450" "SSTORE/SLOAD on one warm slot")))

(defun program (body n)
  (hex->octets (format nil "63~8,'0x5b~A600190038060055700" n (remove #\Space body))))

(defun run-program (code gas)
  (let ((b (clempo.state:make-memory-backend)))
    (clempo.state:memory-backend-put-account b #xc0de :code code)
    (clempo.state:memory-backend-put-account b #xca11 :balance (expt 10 18))
    (let ((clempo.evm:*world* (clempo.state:make-world b))
          (clempo.evm:*spec* (clempo.evm:make-spec :osaka))
          (clempo.evm:*block* (clempo.evm:make-block-env :number 1 :gas-limit gas)))
      (multiple-value-bind (status gas-left)
          (clempo.evm:call-message :call #xca11 #xc0de #xc0de 0 (make-octets 0) gas 0 nil)
        (values status (- gas gas-left))))))

(defcommand "evm-bench" (args)
  "EVM throughput: evm-bench [-n iterations] [-only name] [-jit [N] | -jit-bg | -interp]"
  (let ((n (arg-int args :n 1000000))
        (only (arg args :only)))
    (clempo.dev.evm-compile:with-jit-option (jit args)
      (dolist (entry *bodies*)
        (destructuring-bind (name body doc) entry
          (when (or (null only) (string= only name))
            (let ((code (program body n)))
              (run-program code 100000000)        ; warm up
              (let ((start (clempo.io:monotonic-ticks)))
                (multiple-value-bind (status used) (run-program code most-positive-fixnum)
                  (let ((secs (/ (- (clempo.io:monotonic-ticks) start) internal-time-units-per-second)))
                    (format t "~8A ~A: ~,1F Mgas/s (~:D gas in ~,3F s, ~A) - ~A~%"
                            name status (/ used secs 1d6) used (float secs) status doc)))))))))
    0))
