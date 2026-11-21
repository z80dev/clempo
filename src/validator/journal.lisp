(in-package #:clempo.validator)

;;; Only the voter owner uses this backend. Each immutable-after-sync view file
;;; contains length/bytes/checksum records. Replacing it atomically prevents an
;;; interrupted batch from invalidating the votes already sent to the network.

(defconstant +journal-section-limit+ (* 64 1024))

(defclass file-journal ()
  ((directory :initarg :directory :reader journal-directory)
   (pending :initform (make-hash-table) :reader journal-pending)))

(defun make-file-journal (directory)
  (let ((dir (uiop:ensure-directory-pathname directory)))
    (ensure-directories-exist dir)
    (fsync-validator-directory (uiop:pathname-parent-directory-pathname dir))
    (make-instance 'file-journal :directory dir)))

(defun fsync-validator-directory (directory)
  (let ((fd (sb-posix:open (namestring directory) sb-posix:o-rdonly)))
    (unwind-protect (sb-posix:fsync fd) (sb-posix:close fd))))

(defun replace-durably (path writer)
  (let* ((temp (make-pathname :type "tmp" :defaults path))
         (fd (sb-posix:open (namestring temp)
                            (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-trunc sb-posix:o-nofollow)
                            #o600))
         (stream nil))
    (unwind-protect
         (progn
           (setf stream (sb-sys:make-fd-stream fd :element-type 'octet :output t :auto-close t))
           (funcall writer stream)
           (finish-output stream)
           (sb-posix:fsync fd))
      (if stream (close stream) (sb-posix:close fd)))
    (sb-posix:rename (namestring temp) (namestring path))
    (fsync-validator-directory (make-pathname :name nil :type nil :defaults path))))

(defun journal-file (journal epoch view)
  (merge-pathnames (format nil "~20,'0D-~20,'0D.sx" epoch view) (journal-directory journal)))

(defun journal-files (journal epoch)
  (sort (directory (merge-pathnames (format nil "~20,'0D-*.sx" epoch) (journal-directory journal)))
        #'string< :key #'pathname-name))

(defun journal-section-view (path)
  (let ((name (pathname-name path)))
    (unless (= (length name) 41) (error "Invalid journal section ~A" path))
    (parse-integer name :start 21)))

(defun encode-journal-record (stream bytes)
  (let ((n (length bytes)))
    (write-byte (ldb (byte 8 8) n) stream)
    (write-byte (ldb (byte 8 0) n) stream)
    (write-sequence bytes stream)
    (write-sequence (clempo.sha2:sha256 bytes) stream)))

(defun read-journal-section (path)
  (with-open-file (in path :element-type 'octet :if-does-not-exist nil)
    (when in
      (unless (<= (file-length in) +journal-section-limit+) (error "Oversize vote journal file ~A" path))
      (loop for first = (read-byte in nil) while first
            for size = (logior (ash first 8) (read-byte in))
            for data = (clempo.p2p-wire:read-octets in size)
            for checksum = (clempo.p2p-wire:read-octets in 32)
            do (unless (and (plusp size) (octets= checksum (clempo.sha2:sha256 data)))
                 (error "Corrupt vote journal record in ~A" path))
            collect data))))

(defmethod clempo.simplex:journal-append ((journal file-journal) epoch view bytes)
  (unless (and (typep epoch 'u64) (typep view 'u64) (typep bytes 'octets) (<= 1 (length bytes) #xffff))
    (error "Invalid vote journal entry"))
  (push (cons view bytes) (gethash epoch (journal-pending journal)))
  nil)

(defmethod clempo.simplex:journal-sync ((journal file-journal) epoch)
  (let ((sections (make-hash-table)))
    (dolist (entry (gethash epoch (journal-pending journal)))
      (push (cdr entry) (gethash (car entry) sections)))
    (maphash (lambda (view new)
               (let* ((path (journal-file journal epoch view))
                      (entries (nconc (read-journal-section path) new)))
                 (unless (<= (loop for bytes in entries sum (+ 34 (length bytes))) +journal-section-limit+)
                   (error "Vote journal view ~D exceeds section limit" view))
                 (replace-durably path (lambda (stream)
                                         (dolist (bytes entries) (encode-journal-record stream bytes))))))
             sections)
    (remhash epoch (journal-pending journal)))
  nil)

(defmethod clempo.simplex:journal-replay ((journal file-journal) epoch)
  (loop for path in (journal-files journal epoch)
        nconc (read-journal-section path)))

(defmethod clempo.simplex:journal-prune ((journal file-journal) epoch min-view)
  (let ((removed nil))
    (dolist (path (journal-files journal epoch))
      (when (< (journal-section-view path) min-view)
        (delete-file path)
        (setf removed t)))
    (when removed (fsync-validator-directory (journal-directory journal))))
  nil)

;;; The DKG manager syncs every State/Event before sending dependent messages.
;;; Files are only visible after fsync + atomic rename; numeric padding preserves
;;; append order across restarts. Vote journals use the same checked envelope.
(defclass file-dkg-store ()
  ((directory :initarg :directory :reader dkg-directory)
   (next-sequence :initform 0 :accessor dkg-next-sequence)))

(defun make-file-dkg-store (directory)
  (let ((dir (uiop:ensure-directory-pathname directory)))
    (ensure-directories-exist dir)
    (fsync-validator-directory (uiop:pathname-parent-directory-pathname dir))
    (make-instance 'file-dkg-store :directory dir)))

(defun dkg-files (store suffix)
  (sort (directory (merge-pathnames suffix (dkg-directory store)))
        #'string< :key #'pathname-name))

(defun dkg-file-pair (path)
  (let* ((entries (read-journal-section path))
         (name (pathname-name path))
         (separator (position #\- name)))
    (unless (and entries (null (cdr entries))) (error "Corrupt DKG state/event file ~A" path))
    (cons (parse-integer name :end separator) (first entries))))

(defun write-dkg-file (path bytes)
  (unless (<= 1 (length bytes) (- +journal-section-limit+ 34))
    (error "Oversize DKG state/event"))
  (replace-durably path (lambda (stream) (encode-journal-record stream bytes))))

(defmethod clempo.dkg:dkg-store-states ((store file-dkg-store))
  (mapcar #'dkg-file-pair (dkg-files store "*.state")))

(defmethod clempo.dkg:dkg-store-events ((store file-dkg-store))
  (loop for path in (sort (dkg-files store "*.event")
                          (lambda (a b) (string< a b :start1 21 :start2 21)) :key #'pathname-name)
        for sequence = (parse-integer (pathname-name path) :start 21)
        do (setf (dkg-next-sequence store) (max (1+ sequence) (dkg-next-sequence store)))
        collect (dkg-file-pair path)))

(defmethod clempo.dkg:dkg-store-put-state ((store file-dkg-store) epoch octets)
  (write-dkg-file (merge-pathnames (format nil "~20,'0D.state" epoch) (dkg-directory store)) octets))

(defmethod clempo.dkg:dkg-store-append-event ((store file-dkg-store) epoch octets)
  (write-dkg-file (merge-pathnames (format nil "~20,'0D-~20,'0D.event" epoch (dkg-next-sequence store))
                                  (dkg-directory store))
                  octets)
  (incf (dkg-next-sequence store)))

(defmethod clempo.dkg:dkg-store-prune ((store file-dkg-store) up-to-epoch)
  (loop for path in (nconc (dkg-files store "*.state") (dkg-files store "*.event"))
        when (< (parse-integer (pathname-name path) :end 20) up-to-epoch)
          do (delete-file path))
  (fsync-validator-directory (dkg-directory store)))
