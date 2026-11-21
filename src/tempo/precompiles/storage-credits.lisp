(in-package #:clempo.storage-credits)

;;; TIP-1060 (storage_credits/{mod,dispatch}.rs). These are raw account-derived
;;; slots, not a Solidity mapping. Transient words are shared with the SSTORE hook.
(defcontract storage-credits (:address +storage-credits-address+))

(deferror invalid-mode "InvalidMode()")
(deferror only-direct-call "OnlyDirectCall()")

(defun initialize ()
  (set-code +storage-credits-address+ (hex->octets "ef"))
  (values))

(defun balance-of (account)
  "Return ACCOUNT's persistent u64 credit balance (one full-slot SLOAD)."
  (read (place +storage-credits-address+ account 'u64)))

(defun decode-credit-state (word)
  "Return mode, budget and pending refunds; unused bits are ignored as in Rust."
  (let ((mode (ldb (byte 8 0) word)))
    (unless (<= mode 2) (revert 'invalid-mode))
    (values mode (ldb (byte 64 64) word) (ldb (byte 64 192) word))))

(defun encode-credit-state (mode budget pending)
  (logior mode (ash budget 64) (ash pending 192)))

(defun credit-state-of (account)
  (decode-credit-state (tload +storage-credits-address+ account)))

(defun write-credit-state (account mode budget pending)
  (tstore +storage-credits-address+ account (encode-credit-state mode budget pending))
  (values))

(defun mode-of (account)
  (nth-value 0 (credit-state-of account)))

(defun budget-of (account)
  (nth-value 1 (credit-state-of account)))

(defun write-mode-with-budget (account mode budget)
  (multiple-value-bind (old-mode old-budget pending) (credit-state-of account)
    (declare (ignore old-mode old-budget))
    (write-credit-state account mode budget pending)))

(defun set-mode (account mode)
  "Set mode 0 (Refund), 1 (Preserve), or 2 (Direct). Direct gets an unlimited budget."
  (unless (and (integerp mode) (<= 0 mode 2)) (revert 'invalid-mode))
  (write-mode-with-budget account mode (if (= mode 2) u64-max 0)))

(defun set-budget (account budget)
  "Enter Direct mode with BUDGET credits, including when BUDGET is zero."
  (write-mode-with-budget account 2 budget))

(defun track-minted-credits (account callback)
  "Call zero-argument CALLBACK; return its primary value and credits minted.
Before T7 return zero delta without storage reads. A negative delta is fatal."
  (fork-if (:before :t7)
      (values (funcall callback) 0)
      (let* ((before (balance-of account))
             (value (funcall callback))
             (after (balance-of account)))
        (when (< after before)
          (fatal "storage credit operation for owner ~X consumed credits (before: ~D, after: ~D)"
                 account before after))
        (values value (- after before)))))

(defun with-budget (account limit callback)
  "Call zero-argument CALLBACK; return its primary value and signed credit delta.
Caller must enter in Preserve mode. A positive LIMIT installs a Direct allowance,
then reads the delta and restores Preserve/zero budget even when CALLBACK signals
an error. Like Rust, a failed accounting read prevents restoration. LIMIT zero
leaves transient state alone and propagates callback failure before the after read.
Before T7 invoke CALLBACK without any storage operations and return zero delta."
  (fork-if (:before :t7)
      (values (funcall callback) 0)
      (if (zerop limit)
          (let* ((before (balance-of account))
                 (value (funcall callback))
                 (after (balance-of account)))
            (values value (- after before)))
          (progn
            (set-budget account limit)
            (let* ((before (balance-of account))
                   (failure nil)
                   (value (handler-case (funcall callback)
                            (error (condition) (setf failure condition) nil)))
                   (after (balance-of account)))
              (write-mode-with-budget account 1 0)
              (when failure (error failure))
              (values value (- after before)))))))
;;; Transaction-local exclusions are lazy because most transactions never clear
;;; either fee-bookkeeping slot. NIL key means no keychain exclusion.
(defstruct credit-exemptions
  (fee-payer 0) (fee-token 0) keychain-fee-key fee-balance-slot keychain-limit-slot)

(defun initialize-credit-exemptions (exemptions fee-payer fee-token keychain-fee-key)
  (setf (credit-exemptions-fee-payer exemptions) fee-payer
        (credit-exemptions-fee-token exemptions) fee-token
        (credit-exemptions-keychain-fee-key exemptions) keychain-fee-key)
  (values))

(defun clear-credit-exemptions (exemptions)
  (setf (credit-exemptions-fee-payer exemptions) 0
        (credit-exemptions-fee-token exemptions) 0
        (credit-exemptions-keychain-fee-key exemptions) nil
        (credit-exemptions-fee-balance-slot exemptions) nil
        (credit-exemptions-keychain-limit-slot exemptions) nil)
  (values))

(defun credit-exempt-slot-p (exemptions owner slot)
  (let ((token (credit-exemptions-fee-token exemptions)))
    (and (not (zerop token))
         (or (and (= owner token)
                  (= slot
                     (or (credit-exemptions-fee-balance-slot exemptions)
                         (setf (credit-exemptions-fee-balance-slot exemptions)
                               (mapping-slot (credit-exemptions-fee-payer exemptions) 9)))))
             (and (= owner +account-keychain-address+)
                  (credit-exemptions-keychain-fee-key exemptions)
                  (= slot
                     (or (credit-exemptions-keychain-limit-slot exemptions)
                         (setf (credit-exemptions-keychain-limit-slot exemptions)
                               (mapping-slot
                                token
                                (mapping-slot
                                 (clempo.keccak:keccak256-integer
                                  (concatenate '(simple-array (unsigned-byte 8) (*))
                                   (integer->octets (credit-exemptions-fee-payer exemptions) 20)
                                   (integer->octets (credit-exemptions-keychain-fee-key exemptions) 20)))
                                 1))))))))))


;;; Accumulate earned credits without reading persistent state inside a fill loop.
;;; Flush in ascending address order, matching Rust's BTreeMap.
(defun make-credit-deltas () (make-hash-table :test 'eql))

(defun credit-slots (deltas account slots)
  (unless (zerop slots)
    (setf (gethash account deltas) (min u64-max (+ (gethash account deltas 0) slots))))
  (values))

(defun flush-credit-deltas (deltas apply-function)
  (dolist (account (sort (loop for account being the hash-keys of deltas collect account) #'<))
    (funcall apply-function account (gethash account deltas)))
  (values))

(defcall storage-credits balance-of* (account)
    (:sol "balanceOf(address account)" :returns "uint64")
  (balance-of account))

(defcall storage-credits mode-of* (account)
    (:sol "modeOf(address account)" :returns "uint8")
  (mode-of account))

(defcall storage-credits budget-of* (account)
    (:sol "budgetOf(address account)" :returns "uint64")
  (budget-of account))

(defcall storage-credits set-mode* (mode)
    (:sol "setMode(uint8 newMode)" :mutates t)
  (set-mode sender mode))

(defcall storage-credits set-budget* (credits)
    (:sol "setBudget(uint64 credits)" :mutates t)
  (set-budget sender credits))

(defdispatch storage-credits (self input sender)
  (charge-input-cost input)
  (dispatch-calls))

(register-precompile +storage-credits-address+ 'storage-credits :t7)

;;; Solidity enums have a uint8 selector but strict decoding (T11+) also checks
;;; the variant range. Before T11 the handler returns InvalidMode instead.
(setf (clempo.contract::call-entry-arg-types
       (gethash (clempo.abi:abi-selector "setMode(uint8)")
                (contract-calls 'storage-credits)))
      '((:enum 3)))
