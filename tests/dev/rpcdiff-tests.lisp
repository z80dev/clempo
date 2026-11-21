(defpackage #:clempo.dev.rpcdiff-tests
  (:use #:cl #:clempo.base))

(in-package #:clempo.dev.rpcdiff-tests)

(defun parse (s) (clempo.json:parse-json s))

(deftest head-relative-cases-check-every-other-value ()
  ;; an eth_capabilities answer recorded at the reference's head 0x10 (hash 0xaa)
  (let ((c (parse "{\"suite\":\"gaps\",\"method\":\"eth_capabilities\",\"status\":200,\"class\":\"stable\",\"headRelative\":true,\"response\":{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"head\":{\"number\":\"0x10\",\"hash\":\"0xaa\"},\"state\":{\"disabled\":false,\"oldestBlock\":\"0x0\"},\"stateproofs\":{\"disabled\":false,\"oldestBlock\":\"0x10\"}}}}")))
    (flet ((answer (head hash oldest proofs)
             (parse (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"head\":{\"number\":~S,\"hash\":~S},\"state\":{\"disabled\":false,\"oldestBlock\":~S},\"stateproofs\":{\"disabled\":false,\"oldestBlock\":~S}}}"
                            head hash oldest proofs))))
      (is (clempo.dev::case-matches c 200 (answer "0x20" "0xbb" "0x0" "0x20")))
      (is (not (clempo.dev::case-matches c 200 (answer "0x20" "0xbb" "0x1" "0x20"))))
      (is (not (clempo.dev::case-matches c 200 (answer "0x20" "0xbb" "0x0" "0x10"))))
      ;; without the flag the recorded head must match too
      (let ((plain (remove "headRelative" (cdr c) :key #'car :test #'equal)))
        (is (not (clempo.dev::case-matches (cons :object plain) 200 (answer "0x20" "0xbb" "0x0" "0x20"))))))))
