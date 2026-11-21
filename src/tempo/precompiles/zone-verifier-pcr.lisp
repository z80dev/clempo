(in-package #:clempo.zone-verifier)

;;; Measurements are pinned to approved EIF builds, never derived from a proof.
;;; Source: crates/precompiles/src/zone_verifier/{pcr.rs,pcrs.json,mod.rs}.
(define-condition pcr-policy-error (error)
  ((reason :initarg :reason :reader pcr-policy-error-reason)
   (value :initarg :value :initform nil :reader pcr-policy-error-value))
  (:report (lambda (condition stream)
             (format stream "PCR policy ~A~@[ (~A)~]"
                     (pcr-policy-error-reason condition) (pcr-policy-error-value condition)))))

(defun parse-pcrs (values)
  "Parse exactly three 48-byte hex measurements, rejecting debug (all-zero) PCRs."
  (unless (= (length values) 3) (error 'pcr-policy-error :reason :invalid))
  (mapcar (lambda (value)
            (let* ((start (if (and (>= (length value) 2) (string= value "0x" :end1 2)) 2 0))
                   (bytes (make-octets 48)))
              (unless (= (- (length value) start) 96) (error 'pcr-policy-error :reason :invalid))
              (dotimes (i 48)
                (let ((high (digit-char-p (char value (+ start (* i 2))) 16))
                      (low (digit-char-p (char value (+ start (* i 2) 1)) 16)))
                  (unless (and high low) (error 'pcr-policy-error :reason :invalid))
                  (setf (aref bytes i) (+ (* high 16) low))))
              (when (every #'zerop bytes) (error 'pcr-policy-error :reason :debug))
              bytes))
          (coerce values 'list)))

(defun pcr-split (value separator)
  (loop with start = 0 for end = (position separator value :start start)
        collect (string-trim '(#\Space #\Tab #\Newline #\Return #\Page)
                             (subseq value start end))
        while end do (setf start (1+ end))))

(defun parse-pcr-policy (value)
  "Parse PCR0,PCR1,PCR2 or semicolon-separated HARDFORK=PCR0,PCR1,PCR2 entries."
  (let ((entries nil))
    (dolist (entry (pcr-split value #\;))
      (unless (zerop (length entry))
        (let* ((equals (position #\= entry))
               (name (when equals (string-trim '(#\Space #\Tab #\Newline #\Return #\Page)
                                               (subseq entry 0 equals))))
               (fork (if equals
                         (or (find name +forks+ :test #'string-equal :key #'symbol-name)
                             (error 'pcr-policy-error :reason :invalid-hardfork :value name))
                         :genesis)))
          (when (assoc fork entries) (error 'pcr-policy-error :reason :duplicate-hardfork :value fork))
          (push (cons fork (parse-pcrs (pcr-split (if equals (subseq entry (1+ equals)) entry) #\,)))
                entries))))
    (unless entries (error 'pcr-policy-error :reason :invalid))
    (nreverse entries)))

(defun validate-pcr-policy (policy chain-id)
  "Validate a development-only override; production chain IDs forbid custom PCR policy."
  (when (member chain-id (list +mainnet-chain-id+ +moderato-chain-id+))
    (error 'pcr-policy-error :reason :custom-policy-not-allowed :value chain-id))
  policy)

(defun pcr-policy-at (policy &optional (fork *spec*))
  "Return the newest applicable measurement triple from a parsed custom policy."
  (let ((chosen nil))
    (dolist (entry policy (cdr chosen))
      (when (and (fork>= fork (car entry))
                 (or (null chosen) (fork> (car entry) (car chosen))))
        (setf chosen entry)))))

(define-constant +nitro-t13-pcrs+
  (mapcar #'hex->octets
          '("d64354677406bfda618e6f694528eec9e96c967296a4021249ad0153234f10f95861cd13509fbb7240f7a6d38ce69302"
            "0cfa8adbf2c8842affa4e9d2bf3cd59ba3782c538ca5086c203d2382ed8b9945f289537c86dcbe52ac85057e8895fdbd"
            "7cde50b7d313a691c03106281a84f6ae3c7bcdcffc8063bd1729d5f4969f535c508553c7102475bc1f02b97432ab56b9")))

(define-constant +nitro-t14-pcrs+
  (mapcar #'hex->octets
          '("9f39c6eb8628230bd3aa04b36b6afc63580a44248a2350784a3802829be39f194a33a68f96399533651bce289e4bbc00"
            "0cfa8adbf2c8842affa4e9d2bf3cd59ba3782c538ca5086c203d2382ed8b9945f289537c86dcbe52ac85057e8895fdbd"
            "f59b8e8abaeb1acbbadf5a49df3910fdf300624498df8eac2160063d9e02f67b371c9433d59ceb5fe642e2edd692cbc2")))

(defun approved-pcrs ()
  (fork-if (:since :t14 :doc "T14 approved Nitro EIF measurements")
      +nitro-t14-pcrs+
      (fork-if (:since :t13 :doc "TIP-1098 approved Nitro EIF measurements") +nitro-t13-pcrs+ nil)))
