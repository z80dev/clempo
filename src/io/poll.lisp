(defpackage #:clempo.poll
  (:use #:cl #:clempo.base)
  (:documentation "Readiness polling over many sockets, kqueue on Darwin and epoll on Linux,
through direct system calls, and the non-blocking socket calls an event loop needs.

A poller is waited on by one thread. WATCH registers a descriptor for good, edge-triggered:
an event comes each time new input (or end of stream, or an error) arrives after the last
wait returned it, so a reader that stops before draining the socket waits for more.
WATCH-WRITE adds one event for room to write. WATCH-LEVEL is level-triggered instead: every
wait reports the descriptor while it is readable (a wake-up pipe, a listening socket).")
  (:export #:make-poller #:poller-close #:watch #:watch-write #:unwatch #:watch-level
           #:poller-wait #:poller-event #:poller-event-hangup-p
           #:accept-connection #:fd-read-some #:fd-send-some #:fd-shutdown #:fd-close
           #:set-nonblocking #:set-send-buffer #:make-wakeup-pipe #:wakeup #:available-cpus))

(in-package #:clempo.poll)

(defmacro libc (name result &rest args)
  "Call the C function NAME; ARGS alternate alien types and values."
  `(sb-alien:alien-funcall
    (sb-alien:extern-alien ,name (function ,result ,@(loop for (type) on args by #'cddr collect type)))
    ,@(loop for (nil value) on args by #'cddr collect value)))

(defun syscall-fail (what)
  (error "~A failed: ~A" what (sb-int:strerror (sb-alien:get-errno))))

(defconstant +max-events+ 64 "Events one wait returns at most.")

;;; kqueue: struct kevent is {uintptr_t ident; int16_t filter; uint16_t flags;
;;; uint32_t fflags; intptr_t data; void *udata}, 32 bytes.
;;; epoll: struct epoll_event is {uint32_t events; uint64_t data}, packed (12 bytes) on
;;; x86-64 only.

#+darwin
(progn
  (defconstant +evfilt-read+ -1)
  (defconstant +evfilt-write+ -2)
  (defconstant +ev-add+ #x1)
  (defconstant +ev-delete+ #x2)
  (defconstant +ev-oneshot+ #x10)
  (defconstant +ev-clear+ #x20)
  (defconstant +event-size+ 32)
  (defconstant +event-fd-offset+ 0))

#+linux
(progn
  (defconstant +epollin+ #x1)
  (defconstant +epollout+ #x4)
  (defconstant +epollrdhup+ #x2000)
  (defconstant +epollet+ (ash 1 31))
  (defconstant +epoll-ctl-add+ 1)
  (defconstant +epoll-ctl-del+ 2)
  (defconstant +epoll-ctl-mod+ 3)
  (defconstant +event-size+ #+x86-64 12 #-x86-64 16)
  (defconstant +event-fd-offset+ #+x86-64 4 #-x86-64 8))

(defstruct (poller (:constructor %make-poller (fd)))
  (fd -1 :type fixnum)
  (events (make-octets (* +max-events+ +event-size+)) :type octets))

#+darwin
(defun kevent-change (kq fd filter flags)
  (declare (type fixnum kq fd filter flags))
  (sb-alien:with-alien ((ev (array (sb-alien:unsigned 8) #.+event-size+)))
    (let ((sap (sb-alien:alien-sap ev)))
      (setf (sb-sys:sap-ref-64 sap 0) fd
            (sb-sys:signed-sap-ref-16 sap 8) filter
            (sb-sys:sap-ref-16 sap 10) flags
            (sb-sys:sap-ref-32 sap 12) 0
            (sb-sys:sap-ref-64 sap 16) 0
            (sb-sys:sap-ref-64 sap 24) 0)
      (loop repeat 100
            do (when (>= (libc "kevent" sb-alien:int sb-alien:int kq sb-alien:system-area-pointer sap
                               sb-alien:int 1 sb-alien:system-area-pointer (sb-sys:int-sap 0)
                               sb-alien:int 0 sb-alien:system-area-pointer (sb-sys:int-sap 0))
                         0)
                 (return t))
               (unless (= (sb-alien:get-errno) sb-unix:eintr) (return nil))))))

#+linux
(defun epoll-change (epfd op fd events)
  (declare (type fixnum epfd op fd events))
  (sb-alien:with-alien ((ev (array (sb-alien:unsigned 8) #.+event-size+)))
    (let ((sap (sb-alien:alien-sap ev)))
      (setf (sb-sys:sap-ref-32 sap 0) events
            (sb-sys:sap-ref-64 sap +event-fd-offset+) fd)
      (zerop (libc "epoll_ctl" sb-alien:int sb-alien:int epfd sb-alien:int op
                   sb-alien:int fd sb-alien:system-area-pointer sap)))))

(defun make-poller ()
  (let ((fd #+darwin (libc "kqueue" sb-alien:int)
            #+linux (libc "epoll_create1" sb-alien:int sb-alien:int #x80000))) ; EPOLL_CLOEXEC
    (when (minusp fd) (syscall-fail "creating a poller"))
    (%make-poller fd)))

(defun poller-close (poller)
  (when (>= (poller-fd poller) 0)
    (sb-unix:unix-close (poller-fd poller))
    (setf (poller-fd poller) -1)))

(defun watch (poller fd)
  "Report FD whenever input, end of stream or an error arrives (edge-triggered); an event
comes at once if input is already waiting. NIL if FD cannot be watched."
  #+darwin (kevent-change (poller-fd poller) fd +evfilt-read+ (logior +ev-add+ +ev-clear+))
  #+linux (epoll-change (poller-fd poller) +epoll-ctl-add+ fd
                        (logior +epollin+ +epollrdhup+ +epollet+)))

(defun watch-write (poller fd)
  "Also report the watched FD once it has room to write. NIL if it cannot be watched."
  #+darwin (kevent-change (poller-fd poller) fd +evfilt-write+ (logior +ev-add+ +ev-oneshot+))
  #+linux (epoll-change (poller-fd poller) +epoll-ctl-mod+ fd
                        (logior +epollin+ +epollrdhup+ +epollout+ +epollet+)))

(defun unwatch (poller fd)
  "Stop reporting FD."
  #+darwin (progn (kevent-change (poller-fd poller) fd +evfilt-write+ +ev-delete+)
                  (kevent-change (poller-fd poller) fd +evfilt-read+ +ev-delete+))
  #+linux (epoll-change (poller-fd poller) +epoll-ctl-del+ fd 0))

(defun watch-level (poller fd)
  "Report FD on every wait for as long as it is readable."
  (or #+darwin (kevent-change (poller-fd poller) fd +evfilt-read+ +ev-add+)
      #+linux (epoll-change (poller-fd poller) +epoll-ctl-add+ fd +epollin+)
      (syscall-fail "watching a descriptor")))

(defun poller-wait (poller timeout-ms)
  "Wait up to TIMEOUT-MS milliseconds (NIL: without limit) for events: their count (0 on
timeout or when a signal interrupted the wait). POLLER-EVENT names each one's descriptor."
  (declare (type (or null (integer 0 #.most-positive-fixnum)) timeout-ms))
  (let ((pfd (poller-fd poller)) (events (poller-events poller)))
    (sb-sys:with-pinned-objects (events)
      (let* ((sap (sb-sys:vector-sap events))
             (n #+darwin
                (sb-alien:with-alien ((ts (array (sb-alien:unsigned 8) 16)))
                  (let ((tsap (sb-alien:alien-sap ts)))
                    (when timeout-ms
                      (multiple-value-bind (s ms) (floor timeout-ms 1000)
                        (setf (sb-sys:sap-ref-64 tsap 0) s (sb-sys:sap-ref-64 tsap 8) (* ms 1000000))))
                    (libc "kevent" sb-alien:int sb-alien:int pfd
                          sb-alien:system-area-pointer (sb-sys:int-sap 0) sb-alien:int 0
                          sb-alien:system-area-pointer sap sb-alien:int +max-events+
                          sb-alien:system-area-pointer (if timeout-ms tsap (sb-sys:int-sap 0)))))
                #+linux
                (libc "epoll_wait" sb-alien:int sb-alien:int pfd sb-alien:system-area-pointer sap
                      sb-alien:int +max-events+ sb-alien:int (or timeout-ms -1))))
        (cond ((>= n 0) n)
              ((= (sb-alien:get-errno) sb-unix:eintr) 0)
              (t (syscall-fail "waiting for events")))))))

(declaim (inline poller-event))
(defun poller-event (poller i)
  "The descriptor of the Ith event the last wait returned."
  (let ((events (poller-events poller)))
    (sb-sys:with-pinned-objects (events)
      (sb-sys:sap-ref-64 (sb-sys:vector-sap events) (+ (* i +event-size+) +event-fd-offset+)))))

(defun poller-event-hangup-p (poller i)
  "Whether the Ith event says the peer ended its side (or the socket failed): input that
arrived with it may be followed by an end of stream no later event announces."
  (let ((events (poller-events poller)))
    (sb-sys:with-pinned-objects (events)
      (let ((sap (sb-sys:vector-sap events)) (at (* i +event-size+)))
        #+darwin (logtest #x8000 (sb-sys:sap-ref-16 sap (+ at 10)))      ; EV_EOF in flags
        #+linux (logtest #x2018 (sb-sys:sap-ref-32 sap at))))))           ; RDHUP, HUP, ERR

;;; Sockets

(defun set-int-option (fd level name value)
  (sb-alien:with-alien ((v sb-alien:int value))
    (zerop (libc "setsockopt" sb-alien:int sb-alien:int fd sb-alien:int level sb-alien:int name
                 (* sb-alien:int) (sb-alien:addr v) sb-alien:unsigned-int 4))))

(defconstant +sol-socket+ #+darwin #xffff #+linux 1)

(defun set-send-buffer (fd bytes)
  "Set FD's SO_SNDBUF."
  (set-int-option fd +sol-socket+ #+darwin #x1001 #+linux 7 bytes))

(defun set-nonblocking (fd)
  (let ((flags (sb-posix:fcntl fd sb-posix:f-getfl)))
    (sb-posix:fcntl fd sb-posix:f-setfl (logior flags sb-posix:o-nonblock))))

(defun accept-connection (listen-fd)
  "Accept a pending connection on the non-blocking LISTEN-FD: the new socket's descriptor,
non-blocking and close-on-exec, with TCP_NODELAY (and, on Darwin, SO_NOSIGPIPE). NIL and the
errno when none could be taken (EAGAIN: none is pending)."
  (loop repeat 100
        do (let ((fd #+linux (libc "accept4" sb-alien:int sb-alien:int listen-fd
                                   sb-alien:system-area-pointer (sb-sys:int-sap 0)
                                   sb-alien:system-area-pointer (sb-sys:int-sap 0)
                                   sb-alien:int (logior #x800 #x80000)) ; SOCK_NONBLOCK|SOCK_CLOEXEC
                     #+darwin (libc "accept" sb-alien:int sb-alien:int listen-fd
                                    sb-alien:system-area-pointer (sb-sys:int-sap 0)
                                    sb-alien:system-area-pointer (sb-sys:int-sap 0))))
             (cond ((>= fd 0)
                    #+darwin (progn (set-nonblocking fd)
                                    (sb-posix:fcntl fd 2 1)                    ; F_SETFD FD_CLOEXEC
                                    (set-int-option fd +sol-socket+ #x1022 1)) ; SO_NOSIGPIPE
                    (set-int-option fd 6 1 1)                                   ; TCP_NODELAY
                    (return fd))
                   (t (let ((errno (sb-alien:get-errno)))
                        ;; a connection reset while queued is skipped, as is a signal
                        (unless (member errno (list sb-unix:eintr #+linux 103 #+darwin 53))
                          (return (values nil errno)))))))))

(defun fd-read-some (fd o start end)
  "Read what FD has into O[START,END) without waiting: the count (0 at end of stream), or
NIL and the errno (EAGAIN when nothing is there yet)."
  (declare (type fixnum fd start end) (type octets o))
  (loop repeat 100
        do (multiple-value-bind (count errno)
               (sb-sys:with-pinned-objects (o)
                 (sb-unix:unix-read fd (sb-sys:sap+ (sb-sys:vector-sap o) start) (- end start)))
             (unless (eql errno sb-unix:eintr) (return (values count errno))))))

(defun fd-send-some (fd o start end)
  "Send what FD takes of O[START,END) without waiting (and without SIGPIPE): the count, or
NIL and the errno (EAGAIN when the socket's buffer is full)."
  (declare (type fixnum fd start end) (type octets o))
  (loop repeat 100
        do (let ((count (sb-sys:with-pinned-objects (o)
                          (libc "send" sb-alien:long sb-alien:int fd
                                sb-alien:system-area-pointer (sb-sys:sap+ (sb-sys:vector-sap o) start)
                                sb-alien:unsigned-long (- end start)
                                sb-alien:int #+linux #x4000 #+darwin 0)))) ; MSG_NOSIGNAL
             (if (>= count 0)
                 (return count)
                 (let ((errno (sb-alien:get-errno)))
                   (unless (= errno sb-unix:eintr) (return (values nil errno))))))))

(defun fd-shutdown (fd)
  "Shut both directions of the socket FD: its reader sees end of stream, its writer an error."
  (libc "shutdown" sb-alien:int sb-alien:int fd sb-alien:int 2))

(defun fd-close (fd) (sb-unix:unix-close fd))

(defun make-wakeup-pipe ()
  "(values read-fd write-fd), both non-blocking."
  (multiple-value-bind (r w) (sb-posix:pipe)
    (set-nonblocking r)
    (set-nonblocking w)
    (values r w)))

(defun wakeup (write-fd)
  (let ((o (make-octets 1)))
    (sb-sys:with-pinned-objects (o)
      (sb-unix:unix-write write-fd o 0 1))))

(defun available-cpus ()
  "The processors this process may run on: its affinity mask on Linux, the active CPUs on
Darwin."
  #+linux
  (sb-alien:with-alien ((mask (array (sb-alien:unsigned 64) 16)))
    (let ((n (libc "sched_getaffinity" sb-alien:int sb-alien:int 0 sb-alien:unsigned-long 128
                   sb-alien:system-area-pointer (sb-alien:alien-sap mask))))
      (if (zerop n)
          (loop for i below 16 sum (logcount (sb-alien:deref mask i)))
          1)))
  #+darwin
  (sb-alien:with-alien ((value sb-alien:int 0) (size sb-alien:unsigned-long 4))
    (if (zerop (libc "sysctlbyname" sb-alien:int sb-alien:c-string "hw.activecpu"
                     (* sb-alien:int) (sb-alien:addr value)
                     (* sb-alien:unsigned-long) (sb-alien:addr size)
                     sb-alien:system-area-pointer (sb-sys:int-sap 0) sb-alien:unsigned-long 0))
        (max 1 value)
        1)))
