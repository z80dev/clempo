(in-package #:clempo.simplex)

;;; The voter's journal (commonware simplex actors/voter/actor.rs, a segmented journal with
;;; one section per view): every vote the voter signs, every certificate it records and every
;;; certification verdict is appended, synced before anything built from it leaves the voter,
;;; and replayed on restart, so a restarted voter never signs a vote that contradicts one it
;;; already sent.
;;;
;;; Consensus touches no files: a journal is any object specializing the four generic
;;; functions below on opaque octets (the node's validator provides a file-backed one).
;;; MEMORY-JOURNAL is the in-memory reference: appends become durable only at JOURNAL-SYNC.

(defgeneric journal-append (journal epoch view octets)
  (:documentation "Buffer OCTETS (one encoded artifact) in section (EPOCH, VIEW)."))

(defgeneric journal-sync (journal epoch)
  (:documentation "Return only when every earlier append of EPOCH is durable."))

(defgeneric journal-replay (journal epoch)
  (:documentation "The durable entries of EPOCH: a list of octets in ascending view, in append
order within a view."))

(defgeneric journal-prune (journal epoch min-view)
  (:documentation "Allow every entry of EPOCH below MIN-VIEW to be dropped."))

;;; Artifacts: commonware's simplex Artifact, byte for byte.

(defcodec (certification-record (:constructor make-certification-record))
  (round view-round)
  (ok :bool))

(defcwunion artifact
  (0 notarize) (1 notarization) (2 certification-record) (3 nullify)
  (4 nullification) (5 finalize) (6 finalization))

(defun encode-artifact (artifact) (cw-encode 'artifact artifact))
(defun decode-artifact (octets) (cw-decode 'artifact octets))

(defun artifact-view (artifact)
  (round-view (etypecase artifact
                ((or notarize nullify finalize) (vote-round artifact))
                ((or notarization nullification finalization) (certificate-round artifact))
                (certification-record (certification-record-round artifact)))))

;;; In memory

(defstruct (memory-journal (:constructor make-memory-journal ()))
  "Durable sections by (epoch . view), each a list of octets newest first, and the appends
not yet synced."
  (sections (make-hash-table :test 'equal))
  (pending '()))

(defmethod journal-append ((j memory-journal) epoch view octets)
  (push (list epoch view (copy-octets octets)) (memory-journal-pending j)))

(defmethod journal-sync ((j memory-journal) epoch)
  (setf (memory-journal-pending j)
        (loop for entry in (reverse (memory-journal-pending j))
              if (= (first entry) epoch)
                do (push (third entry)
                         (gethash (cons epoch (second entry)) (memory-journal-sections j)))
              else collect entry into keep
              finally (return (reverse keep)))))

(defmethod journal-replay ((j memory-journal) epoch)
  (let ((views '()))
    (maphash (lambda (key entries)
               (declare (ignore entries))
               (when (= (car key) epoch) (push (cdr key) views)))
             (memory-journal-sections j))
    (loop for view in (sort views #'<)
          append (reverse (gethash (cons epoch view) (memory-journal-sections j))))))

(defmethod journal-prune ((j memory-journal) epoch min-view)
  (let ((dead '()))
    (maphash (lambda (key entries)
               (declare (ignore entries))
               (when (and (= (car key) epoch) (< (cdr key) min-view)) (push key dead)))
             (memory-journal-sections j))
    (dolist (key dead) (remhash key (memory-journal-sections j)))))

(defun memory-journal-drop-unsynced (j)
  "Forget appends never synced, as a crash would."
  (setf (memory-journal-pending j) '()))
