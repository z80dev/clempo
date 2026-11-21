(defpackage #:clempo.consensus.tests
  (:use #:cl #:clempo.base #:clempo.cwcodec #:clempo.consensus))

(in-package #:clempo.consensus.tests)

(defun data-path (name)
  (merge-pathnames (concatenate 'string "data/" name)
                   (asdf:system-relative-pathname "clempo-consensus" "../../tests/consensus/")))

(defun hex (o) (octets->hex o :prefix nil))

(defun golden ()
  (clempo.json:parse-json (clempo.io:read-file-string (data-path "rust_golden.json"))))

(defun golden-scheme-parts (g)
  "The Rust harness's sharing: coefficients from its DKG Output (summary, mode, total, poly)
and the four shares (varint index || 32-byte scalar)."
  (let* ((out (hex->octets (clempo.json:jget g "output")))
         (r (make-cw-reader out :start 32)))
    (read-u8 r)                              ; mode
    (let* ((total (read-uint r 4))
           (coefficients (loop repeat (read-length r) collect (read-raw r 96)))
           (shares (loop for s across (clempo.json:jget g "shares")
                         collect (let ((o (hex->octets s)))
                                   (cons (aref o 0) (octets->integer o :start 1))))))
      (values coefficients total shares))))

(defun num (v key)
  "A golden-vector field that is a JSON number or a decimal string."
  (let ((x (clempo.json:jget v key)))
    (if (stringp x) (parse-integer x) x)))

(defun vector-proposal (v)
  (make-proposal :round (make-round :epoch (num v "epoch")
                                    :view (num v "view"))
                 :parent (num v "parent")
                 :payload (hex->octets (clempo.json:jget v "payload"))))

(deftest golden-votes-and-certificates ()
  ;; Votes signed with the Rust harness's shares are byte-identical to Rust's, certificates
  ;; assembled from them are byte-identical too, and everything verifies and round-trips.
  (let ((g (golden)))
    (multiple-value-bind (coefficients total shares) (golden-scheme-parts g)
      (let ((schemes (mapcar (lambda (s) (make-signer-scheme coefficients total s)) shares))
            (votes (make-hash-table :test 'equal))
            (checked 0))
        (loop for v across (clempo.json:jget g "vectors")
              for name = (clempo.json:jget v "name")
              for kind = (clempo.json:jget v "kind")
              for bytes = (hex->octets (clempo.json:jget v "bytes"))
              for epoch = (or (num v "epoch") 0)
              when (eql (num v "channel") 0)
                do (let* ((signer (num v "signer"))
                          (p (vector-proposal v))
                          (k (intern (string-upcase kind) :keyword))
                          (vote (sign-vote (nth signer schemes) k
                                           (if (eq k :nullify) (proposal-round p) p))))
                     (incf checked)
                     (is= (hex bytes) (hex (encode-epoch-mux epoch 'vote vote)) "~A" name)
                     (is (verify-vote (first schemes) vote) "~A verifies" name)
                     (is-equalp vote (decode-epoch-mux 'vote bytes))
                     (push vote (gethash (list kind epoch) votes)))
              when (eql (num v "channel") 1)
                do (let* ((k (subseq kind 0 (- (length kind) 5))) ; notarization -> notariz
                          (key (list (cond ((string= k "notariz") "notarize")
                                           ((string= k "nullific") "nullify")
                                           (t "finalize"))
                                     epoch))
                          (cert (assemble-from-votes (first schemes)
                                                     (subseq (reverse (gethash key votes)) 0 3))))
                     (incf checked)
                     (is= (hex bytes) (hex (encode-epoch-mux epoch 'certificate cert)) "~A" name)
                     (is (verify-artifact (make-certificate-verifier (first coefficients)) cert))
                     (is-equalp cert (decode-epoch-mux 'certificate bytes))))
        (is= 45 checked)))))

(deftest tampered-certificates-fail ()
  (let ((g (golden)))
    (multiple-value-bind (coefficients total shares) (golden-scheme-parts g)
      (let* ((s (make-signer-scheme coefficients total (first shares)))
             (p (make-proposal :round (make-round :epoch 1 :view 2) :parent 1
                               :payload (make-octets 32)))
             (votes (loop for sh in shares
                          collect (sign-vote (make-signer-scheme coefficients total sh) :finalize p)))
             (cert (assemble-from-votes s (subseq votes 1)))
             (cv (make-certificate-verifier (first coefficients)))
             (other (make-proposal :round (make-round :epoch 1 :view 3) :parent 1
                                   :payload (make-octets 32))))
        (is (verify-artifact cv cert))
        ;; a different subject, a swapped seed, another signer's partial
        (is (not (verify-certificate cv (list :finalize other) (finalization-certificate cert))))
        (is (not (verify-certificate cv (list :notarize p) (finalization-certificate cert))))
        (let ((sig (finalization-certificate cert)))
          (is (not (verify-certificate cv (list :finalize p)
                                       (make-vrf-signature :vote (vrf-signature-seed sig)
                                                           :seed (vrf-signature-vote sig))))))
        (let ((v (first votes)))
          (setf (attestation-signer (finalize-attestation v)) 1)
          (is (not (verify-vote s v))))
        ;; batches: all good, then one bad among good
        (is (verify-certificates cv (list (cons (list :finalize p) (finalization-certificate cert))
                                          (cons (list :notarize p)
                                                (notarization-certificate
                                                 (assemble-from-votes
                                                  s (loop for sh in (rest shares)
                                                          collect (sign-vote (make-signer-scheme coefficients total sh)
                                                                             :notarize p))))))))
        (is (not (verify-certificates cv (list (cons (list :finalize p) (finalization-certificate cert))
                                               (cons (list :finalize other) (finalization-certificate cert))))))))))
