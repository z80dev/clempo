(in-package #:clempo.committee)

;;; CurrentCommittee (crates/precompiles/src/current_committee, TIP-1070, T8): the next
;;; epoch's committee public keys, written by a system transaction at the epoch's last block.

(defcontract current-committee (:address +current-committee-address+ :prefix "S")
  (epoch u64)
  (ids (vec b256)))

(deferror unauthorized "Unauthorized()")

(defun get-committee-members ()
  "(values epoch public-keys): the stored epoch, then the key list."
  (let* ((epoch (read (s-epoch +current-committee-address+)))
         (keys (read (s-ids +current-committee-address+))))
    (values epoch keys)))

(defun set-committee-members (sender epoch public-keys)
  "CurrentCommittee::set_committee_members: system-only (SENDER zero). The writes neither
mint nor consume TIP-1060 storage credits."
  (unless (zerop sender) (revert 'unauthorized))
  (set-tip1060-credits nil)
  (write (s-epoch +current-committee-address+) epoch)
  (write (s-ids +current-committee-address+) public-keys)
  nil)

(defcall current-committee get-committee-members* ()
    (:sol "getCommitteeMembers()" :returns ("uint64" "bytes32[]"))
  (get-committee-members))

(defcall current-committee set-committee-members* (epoch public-keys)
    (:sol "setCommitteeMembers(uint64 epoch, bytes32[] publicKeys)" :mutates t)
  (set-committee-members sender epoch public-keys)
  (values))

(defdispatch current-committee (self input sender)
  (charge-input-cost input)
  (dispatch-calls))

(register-precompile +current-committee-address+ 'current-committee :t8)
