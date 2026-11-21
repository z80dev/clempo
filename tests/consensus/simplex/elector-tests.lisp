(defpackage #:clempo.simplex.tests
  (:use #:cl #:clempo.base #:clempo.cwcodec #:clempo.consensus #:clempo.simplex))

(in-package #:clempo.simplex.tests)

(defun test-path (relative)
  (asdf:system-relative-pathname "clempo-consensus-simplex"
                                 (concatenate 'string "../../../tests/consensus/" relative)))

(defun hex-file (relative)
  (hex->octets (string-trim '(#\Space #\Newline #\Return)
                            (clempo.io:read-file-string (test-path relative)))))

(defun split-fields (line)
  (loop with start = 0
        for space = (position #\Space line :start start)
        collect (subseq line start space)
        while space
        do (setf start (1+ space))))

(defparameter *epoch-outcomes*
  '((1949 . "dkg/data/mainnet_block_42098399_extradata.hex")
    (2042 . "dkg/data/mainnet_block_44107199_extradata.hex")))

(defun epoch-players (epoch)
  "The epoch's participants: the players of the DKG outcome its boundary block installed."
  (let ((outcome (clempo.dkg:decode-onchain-dkg-outcome
                  (hex-file (cdr (assoc epoch *epoch-outcomes*))))))
    (is= epoch (clempo.dkg:outcome-epoch outcome))
    (coerce (clempo.dkg:outcome-players outcome) 'vector)))

(defun elector-rows ()
  "(version epoch height finalization header) per mainnet block."
  (with-input-from-string (in (clempo.io:read-file-string
                               (test-path "simplex/data/mainnet_elector.txt")))
    (loop for line = (read-line in nil)
          while (and line (plusp (length line)))
          collect (destructuring-bind (version epoch height final header) (split-fields line)
                    (list (if (string= version "v0") :v0 :v1)
                          (parse-integer epoch) (parse-integer height)
                          (cw-decode 'finalization (hex->octets final))
                          (clempo.headers:decode-header (hex->octets header)))))))

(defun row-view (row) (proposal-view (finalization-proposal (fourth row))))
(defun row-seed (row) (vrf-signature-seed (finalization-certificate (fourth row))))

(deftest mainnet-random-elector ()
  ;; Each block's proposer (its header's consensus context) is the leader the Random elector
  ;; picks from the previous view's certificate seed: V0 before T12, V1 after; round robin at
  ;; view 1.
  (let ((checked 0) (previous nil))
    (dolist (row (elector-rows))
      (destructuring-bind (version epoch height finalization header) row
        (declare (ignore height))
        (let* ((ctx (clempo.headers:header-consensus-context header))
               (view (row-view row))
               (prev (and previous (= (second previous) epoch) previous)))
          (is= view (clempo.headers:consensus-context-view ctx))
          (is= epoch (proposal-epoch (finalization-proposal finalization)))
          (when prev
            (is= (1+ (third prev)) (third row) "consecutive heights")
            (is= (row-view prev) (clempo.headers:consensus-context-parent-view ctx)))
          (when (or prev (= view 1))
            (let* ((players (epoch-players epoch))
                   (leader (elect (make-random-elector version) epoch view (length players)
                                  (and prev (row-seed prev)))))
              (is (octets= (aref players leader) (clempo.headers:consensus-context-proposer ctx))
                  "epoch ~D view ~D elects the mainnet proposer" epoch view)
              (incf checked)))
          (setf previous row))))
    (is= 9 checked "eight adjacent pairs and the view-1 block")))

(deftest random-elector-versions-differ ()
  (let* ((rows (elector-rows))
         (seeds (mapcar #'row-seed rows))
         (v0 (make-random-elector :v0)) (v1 (make-random-elector :v1)))
    (is (some (lambda (seed) (/= (elect v0 1 2 16 seed) (elect v1 1 2 16 seed))) seeds))
    ;; view 1 has no certificate: (epoch + view) mod n either way
    (is= 3 (elect v0 2 1 4 nil))
    (is= 3 (elect v1 2 1 4 nil))
    (signals invariant-violation (elect v1 2 5 4 nil))))

(deftest round-robin-elector ()
  (let ((rr (make-round-robin-elector)))
    (is= '(1 2 3 0 1) (loop for view from 1 to 5 collect (elect rr 0 view 4 nil)))
    (is= '(2 3 4 0 1) (loop for epoch below 5 collect (elect rr epoch 2 5 nil)))
    ;; epoch + view wraps at 2^64
    (is= (mod (- (+ (1- (expt 2 64)) 6) (expt 2 64)) 5)
         (elect rr (1- (expt 2 64)) 6 5 nil)))
  ;; a seeded rotation is a fixed permutation: n consecutive views elect everyone once
  (let* ((rr (make-round-robin-elector :seed (string->octets "seed")))
         (leaders (loop for view from 1 to 7 collect (elect rr 3 view 7 nil))))
    (is= '(0 1 2 3 4 5 6) (sort (copy-list leaders) #'<))
    (is (not (equal leaders (loop for view from 1 to 7 collect (mod (+ 3 view) 7)))))))
