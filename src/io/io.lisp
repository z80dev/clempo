(defpackage #:clempo.io
  (:use #:cl #:clempo.base)
  (:documentation "The only doors to the outside world for deterministic layers.
Consensus, storage and execution read the clock and randomness through these special
variables, so a simulator rebinds them instead of threading parameters.")
  (:export #:*clock* #:now-ms #:now-seconds #:real-clock #:make-manual-clock
           #:manual-clock-advance
           #:*entropy* #:random-octets #:random-below #:make-seeded-entropy #:real-entropy
           #:read-file-octets #:write-file-octets #:write-file-atomically #:write-new-file
           #:temp-directory #:read-file-string #:getenv #:run-capture))

(in-package #:clempo.io)

;;; Clock: a function of no arguments returning Unix milliseconds.

(defun real-clock ()
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ (* sec 1000) (floor usec 1000))))

(defvar *clock* #'real-clock)

(defun now-ms () (funcall *clock*))
(defun now-seconds () (floor (funcall *clock*) 1000))

(defstruct (manual-clock (:constructor %make-manual-clock (ms)))
  (ms 0 :type integer))

(defun make-manual-clock (&optional (start-ms 0))
  "Returns (values clock-function handle); MANUAL-CLOCK-ADVANCE moves it."
  (let ((h (%make-manual-clock start-ms)))
    (values (lambda () (manual-clock-ms h)) h)))

(defun manual-clock-advance (handle ms)
  (incf (manual-clock-ms handle) ms))

;;; Entropy: a function (n) -> n random octets.

(defun real-entropy (n)
  (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
    (let ((o (make-octets n))) (read-sequence o in) o)))

(defun make-seeded-entropy (seed)
  "Deterministic entropy for simulations and tests."
  (let ((state (sb-ext:seed-random-state seed)))
    (lambda (n)
      (let ((o (make-octets n)))
        (dotimes (i n o) (setf (aref o i) (random 256 state)))))))

(defvar *entropy* #'real-entropy)

(defun random-octets (n) (funcall *entropy* n))

(defun random-below (limit)
  "Uniform integer in [0, LIMIT) by rejection sampling."
  (let* ((bytes (max 1 (ceiling (integer-length limit) 8)))
         (span (ash 1 (* 8 bytes)))
         (cutoff (- span (mod span limit))))
    (loop for x = (octets->integer (random-octets bytes))
          when (< x cutoff) return (mod x limit))))

;;; Files

(defun read-file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((o (make-octets (file-length in))))
      (read-sequence o in)
      o)))

(defun read-file-string (path)
  (octets->string (read-file-octets path)))

(defun write-file-octets (path o)
  (ensure-directories-exist path)
  (with-open-file (out path :element-type '(unsigned-byte 8) :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
    (write-sequence o out)
    path))

(defun write-new-file (prefix o)
  "Create a file named PREFIX followed by six random characters with mkstemp, which opens it
O_CREAT|O_EXCL with mode 0600, so no existing path, symlink or other writer's file is ever
reused; write the octets O through that descriptor and return the name."
  (multiple-value-bind (fd name) (sb-posix:mkstemp (concatenate 'string prefix "XXXXXX"))
    (let ((out (sb-sys:make-fd-stream fd :output t :element-type '(unsigned-byte 8)
                                         :buffering :full :auto-close t))
          (done nil))
      (unwind-protect (progn (write-sequence o out) (finish-output out) (setf done t))
        (close out)
        (unless done (ignore-errors (sb-posix:unlink name)))))
    name))

(defun write-file-atomically (path o)
  "Write to a fresh sibling (WRITE-NEW-FILE, so concurrent writers never share it), then
rename it over PATH."
  (ensure-directories-exist path)
  (let ((tmp (write-new-file (format nil "~A." (namestring path)) o)))
    (multiple-value-bind (ok errno) (sb-unix:unix-rename tmp (namestring path))
      (unless ok (ignore-errors (sb-posix:unlink tmp)))
      (invariant ok "rename ~A failed: errno ~D" tmp errno))
    path))

(defun getenv (name) (sb-ext:posix-getenv name))

(defun temp-directory ()
  "$TMPDIR, else /tmp, ending in a slash."
  (let ((dir (or (getenv "TMPDIR") "")))
    (cond ((zerop (length dir)) "/tmp/")
          ((char= (char dir (1- (length dir))) #\/) dir)
          (t (concatenate 'string dir "/")))))

(defun run-capture (program args &key input)
  "Run PROGRAM with ARGS (and INPUT octets on stdin); return (values stdout-octets exit-code)."
  (let ((tmp (when input (write-new-file (concatenate 'string (temp-directory) "clempo-") input))))
    (unwind-protect
         (let ((sink (make-string-output-stream)))
           (let ((proc (sb-ext:run-program program args :search t :wait t
                                                        :input (or tmp nil) :output sink
                                                        :error nil :external-format :latin-1)))
             (values (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                          (get-output-stream-string sink))
                     (sb-ext:process-exit-code proc))))
      (when tmp (ignore-errors (sb-posix:unlink tmp))))))
