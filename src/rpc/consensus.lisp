(defpackage #:clempo.rpc.consensus
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render)
  (:local-nicknames (#:json #:clempo.json) (#:serde #:clempo.serde) (#:chain #:clempo.rpc.chain)
                    (#:io #:clempo.io) (#:consensus #:clempo.consensus))
  (:documentation "Tempo's consensus namespace (crates/node/src/rpc/consensus): finalization
certificates with their blocks. The reference answers from its marshal archive; clempo answers
from the certificate archive under ~/cache/clempo/finalizations/<chain>/ that `clempo-dev
certify` fills and verifies (one fin-<first>.txt file of \"height hex\" lines per 1000
heights) and the backend's blocks. A height without an archived certificate is answered as the
reference answers one its marshal lacks.")
  (:export #:finalization-dir #:archived-certificate #:read-consensus-query #:finalization-event))

(in-package #:clempo.rpc.consensus)

;;; The certificate archive

(defconstant +span+ 1000 "heights per archive file")

(defun finalization-dir (&optional (chain "mainnet"))
  (format nil "~A/cache/clempo/finalizations/~A/" (io:getenv "HOME") chain))

(defun backend-finalization-dir (&optional (backend chain:*backend*))
  (let ((id (chain:backend-chain-id backend)))
    (finalization-dir (if (eql id 4217) "mainnet" id))))

(defvar *spans* (make-hash-table :test 'equal :synchronized t)
  "(dir . first) -> vector of certificate hex strings (NIL where a height has none). Files
are written whole and never change, so a loaded span stays; a missing one is looked for again.")

(defun span-path (dir first) (format nil "~Afin-~10,'0D.txt" dir first))

(defun load-span (dir first)
  (let ((key (cons dir first)))
    (or (gethash key *spans*)
        (let ((path (span-path dir first)))
          (when (probe-file path)
            (let ((v (make-array +span+ :initial-element nil)))
              (with-open-file (in path)
                (loop for line = (read-line in nil)
                      while line
                      do (let* ((space (position #\Space line))
                                (h (and space (parse-integer line :end space :junk-allowed t))))
                           (when (and h (<= first h (+ first +span+ -1)))
                             (setf (aref v (- h first)) (subseq line (1+ space)))))))
              (setf (gethash key *spans*) v)))))))

(defun archived-certificate (height &optional (dir (backend-finalization-dir)))
  "The archived finalization certificate (hex) of block HEIGHT, or NIL."
  (let* ((first (* +span+ (floor height +span+)))
         (v (load-span dir first)))
    (and v (aref v (- height first)))))

(defun archived-spans (dir)
  "The first heights of the archive's files, highest first."
  (sort (loop for p in (directory (format nil "~Afin-*.txt" dir))
              for n = (parse-integer (pathname-name p) :start 4 :junk-allowed t)
              when n collect n)
        #'>))

;;; Certified blocks

(defun service-unavailable (what)
  "The reference's Response::Missing: 503 with what was missing as the data."
  (rpc-fail 503 "the consensus subservice was not available, but the request can be retried later" what))

(defun certified-block-json (b certificate)
  "CertifiedBlock {epoch, view, digest, certificate, block} of block B."
  (let* ((proposal (consensus:finalization-proposal (consensus:decode-finalization certificate)))
         (digest (consensus:proposal-payload proposal)))
    (invariant (octets= digest (cblock-hash b))
               "archived certificate of block ~D finalizes ~A" (cblock-number b) (hex32 digest))
    (json:jobject "epoch" (consensus:proposal-epoch proposal)
                  "view" (consensus:proposal-view proposal)
                  "digest" (hex32 digest)
                  "certificate" certificate
                  "block" (primitive-block-json b))))

(defun finalization-event (block certificate seen)
  "The flattened consensus_event result. On archive heads, use their cached certificate."
  (let ((certificate (or certificate (archived-certificate (cblock-number block)))))
    (when certificate
      (cons :object (list* (cons "type" "finalized")
                           (cons "seen" seen)
                           (cdr (certified-block-json block certificate)))))))

(defun certified-at (height)
  "Block HEIGHT with its certificate, or the reference's 503 naming what is missing."
  (let ((certificate (archived-certificate height)))
    (unless certificate (service-unavailable "certificate"))
    (let ((b (and (<= height chain:*head*) (chain-block height))))
      (unless b (service-unavailable "block"))
      (certified-block-json b certificate))))

(defun latest-certified ()
  "The highest block at or below the head with an archived certificate, or NIL."
  (let ((dir (backend-finalization-dir)))
    (loop for first in (archived-spans dir)
          when (<= first chain:*head*)
            do (let ((v (load-span dir first)))
                 (loop for h from (min chain:*head* (+ first +span+ -1)) downto first
                       for certificate = (aref v (- h first))
                       for b = (and certificate (chain-block h))
                       when b do (return-from latest-certified (certified-block-json b certificate)))))))

;;; Query: serde's externally tagged enum { Latest, Height(u64) }, camelCase

(defun read-consensus-query (src)
  ":latest or the height."
  (multiple-value-bind (name height)
      (serde:read-enum src '("latest" "height")
                       :newtype (list (cons "height" (lambda (s) (serde:read-integer s 0 +u64-max+ "u64")))))
    (if (string= name "latest") :latest height)))

(defparam consensus-query (src) "Tempo consensus Query: \"latest\" or {\"height\": u64}"
  (read-consensus-query src))

;;; Methods

(defrpc "consensus_getFinalization" ((query consensus-query))
  "a finalization certificate with its block: \"latest\" or {\"height\": N}"
  (if (eq query :latest)
      (or (latest-certified) (service-unavailable "certifications"))
      (certified-at query)))

(defrpc "consensus_getLatest" ()
  "the latest finalized block with its certificate (notarizations are not tracked)"
  (json:jobject "finalized" (or (latest-certified) :null) "notarized" :null))
