(defpackage #:clempo.io.tests
  (:use #:cl #:clempo.base #:clempo.io))

(in-package #:clempo.io.tests)

(deftest manual-clock-drives-now ()
  (multiple-value-bind (clock handle) (make-manual-clock 5000)
    (let ((*clock* clock))
      (is= 5 (now-seconds))
      (manual-clock-advance handle 2500)
      (is= 7500 (now-ms)))))

(deftest seeded-entropy-is-reproducible ()
  (let ((a (let ((*entropy* (make-seeded-entropy 42))) (random-octets 16)))
        (b (let ((*entropy* (make-seeded-entropy 42))) (random-octets 16))))
    (is-equalp a b)
    (is (< (let ((*entropy* (make-seeded-entropy 1))) (random-below 10)) 10))))

(deftest run-capture-pipes-stdin ()
  (multiple-value-bind (out code) (run-capture "cat" '() :input (string->octets "hello"))
    (is= 0 code)
    (is= "hello" (octets->string out))))

(defun scratch-directory ()
  "A fresh private directory under the temp directory, made with mkdtemp."
  (concatenate 'string (sb-posix:mkdtemp (concatenate 'string (temp-directory) "clempo-test-XXXXXX"))
               "/"))

(defun directory-entries (dir)
  (mapcar #'namestring (directory (concatenate 'string dir "*.*") :resolve-symlinks nil)))

(defun remove-scratch (dir)
  (dolist (f (directory-entries dir)) (ignore-errors (sb-posix:unlink f)))
  (ignore-errors (sb-posix:rmdir dir)))

(deftest new-files-are-private-and-unique ()
  (let ((dir (scratch-directory)))
    (unwind-protect
         (let ((names (loop repeat 50 collect (write-new-file (concatenate 'string dir "f-")
                                                              (octets-of 1 2 3)))))
           (is= 50 (length (remove-duplicates names :test #'string=)))
           (dolist (name names)
             (is= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:lstat name))) "~A" name)
             (is-equalp (octets-of 1 2 3) (read-file-octets name))))
      (remove-scratch dir))))

(deftest atomic-writes-never-follow-a-planted-link ()
  ;; The old fixed sibling PATH.tmp was opened :if-exists :supersede, so a symlink planted
  ;; there redirected the write into whatever it named.
  (let* ((dir (scratch-directory))
         (victim (concatenate 'string dir "victim"))
         (target (concatenate 'string dir "chunk.blk")))
    (unwind-protect
         (progn
           (write-file-octets victim (string->octets "victim"))
           (sb-posix:symlink victim (concatenate 'string target ".tmp"))
           (write-file-atomically target (string->octets "data"))
           (is= "victim" (read-file-string victim))
           (is= "data" (read-file-string target))
           (write-file-atomically target (string->octets "again"))
           (is= "again" (read-file-string target))
           ;; Only the planted link, the victim and the target remain: no temp left behind.
           (is= 3 (length (directory-entries dir))))
      (ignore-errors (sb-posix:unlink (concatenate 'string target ".tmp")))
      (remove-scratch dir))))

(deftest run-capture-input-goes-through-a-private-file ()
  ;; Stdin once went through /tmp/clempo-<pid>-<counter>.in, a name another local user could
  ;; pre-create or link. It is now a mkstemp file (mode 0600) under $TMPDIR, removed after.
  (let ((dir (scratch-directory)) (saved (getenv "TMPDIR")))
    (unwind-protect
         (progn
           (sb-posix:setenv "TMPDIR" dir 1)
           (multiple-value-bind (out code)
               (run-capture "sh" (list "-c" (format nil "cat >/dev/null; find ~A -name 'clempo-*' -perm 600" dir))
                            :input (string->octets "secret"))
             (is= 0 code)
             (is (search (concatenate 'string dir "clempo-") (octets->string out))))
           (is (null (directory-entries dir))))
      (if saved (sb-posix:setenv "TMPDIR" saved 1) (sb-posix:unsetenv "TMPDIR"))
      (remove-scratch dir))))
