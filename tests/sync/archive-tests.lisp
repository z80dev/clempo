(defpackage #:clempo.archive.tests
  (:use #:cl #:clempo.base #:clempo.archive))

(in-package #:clempo.archive.tests)

(deftest chunk-round-trip ()
  (let* ((dir (format nil "/tmp/clempo-archive-test-~D/" (random 1000000 (make-random-state t))))
         (*archive-dir* dir)
         (blocks (coerce (loop for i below 5 collect (make-array i :element-type '(unsigned-byte 8)
                                                                   :initial-element i))
                         'vector)))
    (unwind-protect
         (progn
           (write-chunk 20000 blocks)
           (let ((c (read-chunk 20000)))
             (is= 5 (chunk-count c))
             (dotimes (i 5)
               (is-equalp (aref blocks i) (chunk-block c (+ 20000 i))))
             (signals invariant-violation (chunk-block c 20005))))
      (sb-ext:delete-directory dir :recursive t))))
