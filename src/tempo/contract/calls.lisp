(in-package #:clempo.contract)

;;; Events.

(defun parse-event (signature)
  "\"Transfer(address indexed from, address indexed to, uint256 amount)\" ->
(values topic0 ((type . indexed-p) ...))."
  (let* ((lparen (position #\( signature))
         (inner (subseq signature (1+ lparen) (position #\) signature :from-end t)))
         (params (clempo.abi::split-top-level inner #\,))
         (parsed (mapcar (lambda (p)
                           (let ((words (clempo.abi::split-top-level p #\Space)))
                             (cons (clempo.abi:parse-abi-type (first words))
                                   (and (member "indexed" (rest words) :test #'string=) t))))
                         params)))
    (values (clempo.abi:event-topic
             (clempo.abi:canonical-signature (subseq signature 0 lparen) (mapcar #'car parsed)))
            parsed)))

(defmacro defevent (name signature)
  "Register the Solidity event NAME; (emit address 'NAME args...) logs it."
  (multiple-value-bind (topic params) (parse-event signature)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'event-signature) (list ,topic ',params)
             (get ',name 'event-canonical-signature)
             ,(multiple-value-bind (sol types) (clempo.abi:parse-signature signature)
                (clempo.abi:canonical-signature sol types)))
       ',name)))

(defun indexed-topic (type value)
  (if (clempo.abi:abi-dynamic-p type)
      (ecase type
        (:string (clempo.keccak:keccak256-integer (clempo.abi:utf8-encode value)))
        (:bytes (clempo.keccak:keccak256-integer value)))
      (octets->integer (clempo.abi:abi-encode-value type value))))

(defun emit (address name &rest args)
  "Emit the registered event NAME from ADDRESS (charges LOG gas, rejected when static)."
  (destructuring-bind (topic params) (or (get name 'event-signature)
                                         (error "unknown event ~S" name))
    (invariant (= (length params) (length args)) "~S takes ~D arguments" name (length params))
    (let ((topics (list topic)) (types '()) (values '()))
      (loop for (type . indexed) in params
            for v in args
            do (if indexed
                   (push (indexed-topic type v) topics)
                   (progn (push type types) (push v values))))
      (emit-log address (nreverse topics) (clempo.abi:abi-encode (nreverse types) (nreverse values))))))

;;; Calls. DEFCALL registers one ABI function of a contract; DEFDISPATCH defines the
;;; contract's entry, whose (DISPATCH-CALLS) runs the dispatch! macro's steps.

(defstruct (call-entry (:constructor make-call-entry
                           (contract name selector signature arg-types return-types mutates
                            since until function)))
  contract name selector signature arg-types return-types mutates since until function)

(defun contract-calls (contract)
  "selector -> CALL-ENTRY"
  (or (get contract 'calls)
      (setf (get contract 'calls) (make-hash-table))))

(defun parse-returns (returns)
  (mapcar (lambda (r) (if (stringp r) (clempo.abi:parse-abi-type r) r))
          (if (listp returns) returns (list returns))))

(defmacro defcall (contract name (&rest params) (&key sol returns mutates since until)
                  &body body &environment env)
  "Define the ABI function SOL of CONTRACT. BODY runs with SELF (the contract's address),
SENDER (msg.sender) and PARAMS bound to the decoded arguments; its values are the
returns, encoded as RETURNS (a type string or a list of them). MUTATES rejects static
calls. SINCE / UNTIL gate the selector by fork."
  (multiple-value-bind (sol-name arg-types) (clempo.abi:parse-signature sol)
    (let* ((pkg (symbol-package contract))
           (self (intern "SELF" pkg))
           (sender (intern "SENDER" pkg))
           (fname (intern (format nil "~A/~A" (symbol-name contract) (symbol-name name)) pkg))
           (names (mapcar (lambda (p) (if (consp p) (first p) p)) params))
           (canonical (clempo.abi:canonical-signature sol-name arg-types))
           (source (fork-source-position env (list contract name))))
      (unless (= (length names) (length arg-types))
        (error "defcall ~S ~S: ~D parameters for ~S" contract name (length names) sol))
      `(progn
         (defun ,fname (,self ,sender ,@names)
           (declare (ignorable ,self ,sender))
           (with-fork-site-context ((,contract ,name))
             ,@body))
         ,@(when since
             `((record-fork-site ,since :call '(,contract ,name :since) ,canonical
                                 ',(list* :relation :since source))))
         ,@(when until
             `((record-fork-site ,until :call '(,contract ,name :until) ,canonical
                                 ',(list* :relation :until source))))
         (setf (gethash ,(clempo.abi:abi-selector canonical) (contract-calls ',contract))
               (make-call-entry ',contract ',name ,(clempo.abi:abi-selector canonical) ,canonical
                                ',arg-types ',(parse-returns returns) ,mutates ,since ,until
                                ',fname))
         ',fname))))

(defun selector-of (input)
  (logior (ash (aref input 0) 24) (ash (aref input 1) 16) (ash (aref input 2) 8) (aref input 3)))

(defun unknown-selector (selector)
  (revert-data (clempo.abi:abi-encode-call (clempo.abi:abi-selector "UnknownFunctionSelector(bytes4)")
                                           '((:bytes-n 4)) (list selector))))

(defun missing-selector ()
  (if (spec>= :t1)
      (revert-data (make-octets 0))
      (halt "Invalid input: missing function selector")))

(defun reject-static-call ()
  "typed::reject_static_call: StaticCallNotAllowed() before T12, a halt from T12."
  (if (spec< :t12)
      (revert-data (clempo.abi:abi-encode-call (clempo.abi:abi-selector "StaticCallNotAllowed()") '() '()))
      (halt :static-call)))

(defun run-calls (contract self input sender)
  "The dispatch! macro: selector, fork gate, decode (strict from T11, trailing bytes allowed
from T12), static check for mutating calls, then the call. Returns the output octets."
  (when (< (length input) 4) (missing-selector))
  (let* ((selector (selector-of input))
         (entry (gethash selector (contract-calls contract))))
    (when (or (null entry)
              (and (call-entry-since entry) (spec< (call-entry-since entry)))
              (and (call-entry-until entry) (spec>= (call-entry-until entry))))
      (unknown-selector selector))
    (let ((args (handler-case
                    (clempo.abi:abi-decode-call (call-entry-arg-types entry) input
                                                :strict (spec>= :t11)
                                                :allow-trailing (spec>= :t12)
                                                :memory-limit (* 16 1024 1024))
                  (clempo.abi:abi-decode-error () (revert-data (make-octets 0))))))
      (when (and (call-entry-mutates entry) (static-p))
        (reject-static-call))
      (let* ((types (call-entry-return-types entry))
             (results (multiple-value-list (apply (call-entry-function entry) self sender args))))
        (if types
            (clempo.abi:abi-encode types (subseq results 0 (length types)))
            (make-octets 0))))))

(defmacro defdispatch (contract (self input sender) &body body)
  "Define CONTRACT's Precompile::call. BODY runs the precompile's own prelude (input cost,
initialization checks) and returns the output octets; (DISPATCH-CALLS) inside it runs the
DEFCALL table."
  (let ((fname (intern (format nil "~A/CALL" (symbol-name contract)) (symbol-package contract))))
    `(progn
       (defun ,fname (,self ,input ,sender)
         (declare (ignorable ,self ,input ,sender))
         (macrolet ((dispatch-calls () '(run-calls ',contract ,self ,input ,sender)))
           (with-fork-site-context ((,contract :dispatch))
             ,@body)))
       (setf (get ',contract 'dispatcher) ',fname)
       ',fname)))

(defun contract-dispatcher (contract)
  "The function DEFDISPATCH defined for CONTRACT: (self input sender) -> output octets."
  (get contract 'dispatcher))

(defun charge-input-cost (input)
  "charge_input_cost: per-word calldata gas."
  (deduct-gas (input-cost *spec* (length input))))

(defun has-duplicates-metered (values &key (test 'equal))
  "has_duplicates_metered: T11 dedup gas, then whether VALUES repeats an element."
  (deduct-gas (dedup-cost *spec* (length values)))
  (let ((seen (make-hash-table :test test)))
    (dolist (v values nil)
      (when (gethash v seen) (return t))
      (setf (gethash v seen) t))))

;;; Declaration metadata, rather than dispatch execution, is the ABI specification.

(defun abi-fork-name (fork)
  (if fork (string-downcase (symbol-name fork)) :null))

(defun abi-number-string (number digits)
  (string-downcase (format nil "0x~V,'0X" digits number)))

(defun abi-entry< (a b)
  (let ((na (first a)) (nb (first b)))
    (if (= na nb) (string< (second a) (second b)) (< na nb))))

(defun contract-abi (contract)
  "Deterministic JSON metadata for CONTRACT's calls and its package's errors/events.
Selectors and topics are fixed-width hexadecimal strings; gates are fork names or null.
The result contains declarations only and never reads contract storage."
  (let ((calls '()) (errors '()) (events '()) (package (symbol-package contract)))
    (maphash
     (lambda (selector entry)
       (push
        (list selector (call-entry-signature entry)
              (clempo.json:jobject
               "selector" (abi-number-string selector 8)
               "signature" (call-entry-signature entry)
               "inputs" (map 'vector #'clempo.abi:abi-type-string (call-entry-arg-types entry))
               "outputs" (map 'vector #'clempo.abi:abi-type-string (call-entry-return-types entry))
               "mutability" (if (call-entry-mutates entry) "nonpayable" "view")
               "since" (abi-fork-name (call-entry-since entry))
               "until" (abi-fork-name (call-entry-until entry))))
        calls))
     (contract-calls contract))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
        (let ((error (get symbol 'error-signature))
              (event (get symbol 'event-signature)))
          (when error
            (destructuring-bind (selector types signature) error
              (declare (ignore types))
              (push (list selector signature
                          (clempo.json:jobject "selector" (abi-number-string selector 8)
                                               "signature" signature))
                    errors)))
          (when event
            (destructuring-bind (topic params) event
              (let ((signature (get symbol 'event-canonical-signature)))
                (push (list topic signature
                            (clempo.json:jobject
                             "topic" (abi-number-string topic 64) "signature" signature
                             "indexed" (map 'vector
                                            (lambda (param) (clempo.json:jbool (cdr param)))
                                            params)))
                      events)))))))
    (clempo.json:jobject
     "contract" (string-downcase (format nil "~A:~A" (package-name package) (symbol-name contract)))
     "address" (let ((address (contract-address contract)))
                 (if address (abi-number-string address 40) :null))
     "calls" (map 'vector #'third (sort calls #'abi-entry<))
     "errors" (map 'vector #'third (sort errors #'abi-entry<))
     "events" (map 'vector #'third (sort events #'abi-entry<)))))
