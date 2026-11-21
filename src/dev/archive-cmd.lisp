(in-package #:clempo.dev)

(defcommand "fetch-range" (args)
  "fill the raw block archive: fetch-range [-from 0] [-to N] [-threads 8] [-url U] [-dir D]"
  (let ((clempo.archive:*archive-dir* (arg args :dir)))
    (multiple-value-bind (done errors)
        (clempo.archive:fetch-range (arg-int args :from 0)
                                    (arg-int args :to most-positive-fixnum)
                                    :url (arg args :url)
                                    :threads (arg-int args :threads 8))
      (log-line "fetch-range: ~D chunks written, ~D errors; contiguous to ~D" done (length errors)
                (clempo.archive:archive-contiguous-height))
      (dolist (e errors) (log-line "  chunk ~D: ~A" (car e) (cdr e)))
      (if errors 1 0))))

(defcommand "raw-block" (args)
  "print one archived raw block as hex: raw-block <n>"
  (write-line (octets->hex (clempo.archive:raw-block (parse-integer (first (arg args :rest))))))
  0)
