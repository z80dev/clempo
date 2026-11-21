;;; Run from the checkout root:
;;; CLEMPO_BUILD=release sbcl --dynamic-space-size 8192 --non-interactive --load scripts/build.lisp --eval '(asdf:load-system "clempo-trie/tests")' --load tests/trie/benchmark.lisp --eval '(clempo.trie.tests::benchmark-trie)'
(in-package #:clempo.trie.tests)

(defun benchmark-trie (&optional (count 1000000))
  "Measure a raw in-memory trie with independent seeded 32-byte keys and
32-byte values. Key and value construction is outside timing."
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 4217))
         (keys (make-array count))
         (value (fill (make-octets 32) #xa5))
         (updated (fill (make-octets 32) #x5a))
         (trie (make-trie)))
    (dotimes (i count) (setf (svref keys i) (clempo.io:random-octets 32)))
    (sb-ext:gc :full t)
    (format t "Insert ~:D keys~%" count)
    (let ((start (get-internal-real-time)))
      (time (dotimes (i count) (trie-put trie (svref keys i) value)))
      (format t "~:D inserts/s~%"
              (round (* count internal-time-units-per-second) (- (get-internal-real-time) start))))
    (format t "First root~%")
    (format t "~A~%" (octets->hex (time (trie-root trie)) :prefix t))
    (trie-put trie (svref keys 0) updated)
    (format t "Root after one update~%")
    (format t "~A~%" (octets->hex (time (trie-root trie)) :prefix t))))
