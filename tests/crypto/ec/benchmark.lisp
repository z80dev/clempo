;;; Run from the repository root:
;;; CLEMPO_BUILD=release sbcl --dynamic-space-size 8192 --non-interactive \
;;;   --load scripts/build.lisp --eval '(asdf:load-system "clempo-crypto-ec")' \
;;;   --load tests/crypto/ec/benchmark.lisp
(defpackage #:clempo.ec.benchmark
  (:use #:cl #:clempo.base))
(in-package #:clempo.ec.benchmark)

(defun measure (label count function)
  (assert (funcall function))
  (sb-ext:gc :full t)
  (format t "~&~A: ~D calls~%" label count)
  (let ((real (get-internal-real-time)) (cpu (get-internal-run-time))
        (bytes (sb-ext:get-bytes-consed)))
    (time (dotimes (i count) (funcall function)))
    (format t "  per call: ~,2F us wall, ~,2F us CPU, ~,1F bytes~%"
            (/ (* 1d6 (- (get-internal-real-time) real)) internal-time-units-per-second count)
            (/ (* 1d6 (- (get-internal-run-time) cpu)) internal-time-units-per-second count)
            (/ (- (sb-ext:get-bytes-consed) bytes) (float count 1d0)))))

(defun run-benchmarks ()
  (let ((hash (clempo.keccak:keccak256 (string->octets "benchmark"))))
    (multiple-value-bind (r s recid)
        (clempo.secp256k1:sign hash (hex->octets "4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318"))
      (measure "secp256k1 ecrecover" 10000
               (lambda () (clempo.secp256k1:ecrecover hash r s recid)))))
  (let* ((gx #x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296)
         (gy #x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5)
         (r (mod gx clempo.p256:+p256-n+)) (s (mod (1+ r) clempo.p256:+p256-n+))
         (hash (integer->octets 1 32)))
    (measure "P256 verify" 1000 (lambda () (clempo.p256:p256-verify hash r s gx gy))))
  (flet ((words (&rest ns) (apply #'octets-concat (mapcar (lambda (n) (integer->octets n 32)) ns))))
    (let* ((q (words 11559732032986387107991004021392285783925812861821192530917403151452391805634
                     10857046999023057135944570762232829481370756359578518086990519993285655852781
                     4082367875863433681332203403145435568316851327593401208105741076214120093531
                     8495653923123431417604973247489272438418190587263600148770280649306958101930))
           (input (octets-concat (words 1 2) q
                                 (words 1 (- clempo.bn254:+field-modulus+ 2)) q)))
      (measure "BN254 pairing, 2 pairs (byte API)" 100
               (lambda () (clempo.bn254:bn254-pairing input)))))
  (let* ((g1 (clempo.bls12-381:g1-generator)) (g2 (clempo.bls12-381:g2-generator))
         (input (octets-concat (clempo.bls12-381:g1-encode g1) (clempo.bls12-381:g2-encode g2)
                               (clempo.bls12-381:g1-encode (clempo.bls12-381:g1-neg g1))
                               (clempo.bls12-381:g2-encode g2)))
         (msm (apply #'octets-concat
                     (loop for i from 1 to 128 collect
                       (octets-concat
                        (clempo.bls12-381:g1-encode (clempo.bls12-381:g1-mul g1 i))
                        (clempo.keccak:keccak256 (integer->octets i 32))))))
         (namespace (string->octets "tempo")) (message (string->octets "benchmark"))
         (public (clempo.bls12-381:bls-public-key :min-sig (integer->octets 11 32)))
         (signature (clempo.bls12-381:bls-sign :min-sig (integer->octets 11 32) message
                                               :namespace namespace)))
    (measure "BLS12-381 pairing, 2 pairs (byte API)" 30
             (lambda () (clempo.bls12-381:pairing-precompile input)))
    (measure "BLS12-381 G1 MSM, 128 points (byte API)" 10
             (lambda () (clempo.bls12-381:g1-msm-precompile msm)))
    (measure "BLS MinSig verification (compressed key/signature)" 30
             (lambda () (clempo.bls12-381:bls-verify :min-sig public message signature
                                                                 :namespace namespace)))))

(run-benchmarks)
