(defpackage #:clempo.base.tests
  (:use #:cl #:clempo.base))

(in-package #:clempo.base.tests)

(deftest hex-round-trip ()
  (is= "0x00ff10" (octets->hex (hex->octets "0x00FF10")))
  (is= "0abc" (octets->hex (hex->octets "abc") :prefix nil))
  (is-equalp (octets-of 1 2) (hex->octets "0102")))

(deftest integer-octets ()
  (is= 258 (octets->integer (integer->octets 258 2)))
  (is-equalp (octets-of) (integer->octets-minimal 0))
  (is-equalp (octets-of 1 0) (integer->octets-minimal 256))
  (signals invariant-violation (integer->octets 256 1)))

(deftest signed-views ()
  (is= -1 (u256->s256 +u256-max+))
  (is= +u256-max+ (s256->u256 -1))
  (is= 0 (wrap256 +u256-modulus+)))

(deftest octets=-compares-every-byte ()
  ;; word-at-a-time comparison: every length around a word boundary, every differing position
  (dotimes (n 20)
    (let ((a (make-octets n)))
      (dotimes (i n) (setf (aref a i) (mod (* 37 (1+ i)) 256)))
      (is (octets= a (copy-seq a)))
      (is (not (octets= a (subseq (octets-concat a (octets-of 0)) 0 (1+ n)))))
      (dotimes (i n)
        (let ((b (copy-seq a)))
          (setf (aref b i) (logxor (aref b i) 1))
          (is (not (octets= a b)) "length ~D, differing at ~D" n i))))))

(defun compile-quietly (form)
  "(values function warnings-p failure-p) of compiling FORM, its diagnostics muffled."
  (let ((*error-output* (make-broadcast-stream)))
    (handler-bind ((warning #'muffle-warning))
      (compile nil form))))

(deftest static-assertions-fail-the-compilation ()
  (is (null (macroexpand-1 '(static-assert (= 4 (* 2 2))))))
  (signals static-assertion-failed (macroexpand-1 '(static-assert (< most-positive-fixnum 0))))
  (is (search "too small: 3"
              (handler-case (progn (macroexpand-1 '(static-assert (> 3 4) "too small: ~D" (+ 1 2))) "")
                (static-assertion-failed (c) (princ-to-string c)))))
  ;; in a file or a COMPILE, the compiler reports the failure and the compilation fails
  (is (nth-value 2 (compile-quietly '(lambda () (static-assert (= 1 2)) 1))))
  (is (not (nth-value 2 (compile-quietly '(lambda () (static-assert (= 1 1)) 1))))))

(deftest bounds-prove-their-relations-when-compiled ()
  (signals static-assertion-failed
           (eval '(defbound +base-tests-too-small+ 8 "A buffer" (>= +base-tests-too-small+ 16))))
  (is (eq '+base-tests-bound+ (eval '(defbound +base-tests-bound+ 32 "A buffer" (= +base-tests-bound+ 32)))))
  (is (eql 32 (symbol-value '+base-tests-bound+))))

(deftest the-test-audit-names-leaked-threads ()
  (let* ((before (clempo.base::live-threads))
         (gate (sb-thread:make-semaphore))
         (thread (sb-thread:make-thread (lambda () (sb-thread:wait-on-semaphore gate))
                                        :name "base-tests leaker")))
    (is (equal (list thread) (clempo.base::leaked-threads before)))
    (let ((*long-lived-threads* (list "base-tests leaker")))
      (is (null (clempo.base::leaked-threads before))))
    (sb-thread:signal-semaphore gate)
    (sb-thread:join-thread thread)
    (is (null (clempo.base::leaked-threads before)))))
