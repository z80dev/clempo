(in-package #:clempo.address-registry)

;;; AddressRegistry (crates/precompiles/src/address_registry, TIP-1022, T3): masters
;;; registered by a 32-bit proof of work; a virtual address is masterId(4) ‖ 0xFD×10 ‖
;;; userTag(6) and resolves to its master.

(defstorable master-record
  (master-address address)
  (reserved b11)
  (ty u8))

(defcontract address-registry (:address +address-registry-address+)
  (data (mapping b4 master-record)))

(deferror master-id-collision "MasterIdCollision(address master)")
(deferror invalid-master-address "InvalidMasterAddress()")
(deferror proof-of-work-failed "ProofOfWorkFailed()")
(deferror virtual-address-unregistered "VirtualAddressUnregistered()")

(defevent master-registered "MasterRegistered(bytes4 indexed masterId, address indexed masterAddress)")

(defconstant +virtual-magic+ #xfdfdfdfdfdfdfdfdfdfd)

(defun virtual-address-p (address)
  "TempoAddressExt::is_virtual: bytes 4..14 are 0xFD."
  (= (ldb (byte 80 48) address) +virtual-magic+))

(defun master-id (address) (ldb (byte 32 128) address))
(defun user-tag (address) (ldb (byte 48 0) address))

(defun valid-master-p (address)
  (not (or (zerop address) (virtual-address-p address) (tip20-address-p address))))

(defun implicitly-approved-p (address)
  "TIP-1035: on the implicit approval list (T5+)."
  (fork-if (:since :t5)
           (and (member address (list +fee-manager-address+ +stablecoin-dex-address+
                                      +channel-reserve-address+))
                t)
           nil))

(defun get-master (master-id)
  "The registered master of MASTER-ID, or NIL."
  (let ((master (master-record-master-address
                 (read (data +address-registry-address+ master-id)))))
    (unless (zerop master) master)))

(defun register-virtual-master (sender salt)
  (unless (valid-master-p sender) (revert 'invalid-master-address))
  (let ((hash (clempo.keccak:keccak256-integer
               (octets-concat (integer->octets sender 20) (integer->octets salt 32)))))
    (unless (zerop (ldb (byte 32 224) hash)) (revert 'proof-of-work-failed))
    (let* ((id (ldb (byte 32 192) hash))
           (existing (get-master id)))
      (when existing (revert 'master-id-collision existing))
      (write (data +address-registry-address+ id) (make-master-record :master-address sender))
      (emit +address-registry-address+ 'master-registered id sender)
      id)))

(defun resolve-recipient (to)
  "AddressRegistry::resolve_recipient: TO, or from T3 the master of a virtual TO
(VirtualAddressUnregistered() when none)."
  (fork-if (:before :t3) (return-from resolve-recipient to))
  (if (virtual-address-p to)
      (or (get-master (master-id to)) (revert 'virtual-address-unregistered))
      to))

(defun resolve-virtual-address (address)
  "The master of a virtual ADDRESS, or zero."
  (if (virtual-address-p address)
      (or (get-master (master-id address)) 0)
      0))

(defdispatch address-registry (self input sender)
  (charge-input-cost input)
  (dispatch-calls))

(register-precompile +address-registry-address+ 'address-registry :t3)

(defcall address-registry register-virtual-master* ((salt b256))
    (:sol "registerVirtualMaster(bytes32 salt)" :returns "bytes4" :mutates t)
  (register-virtual-master sender salt))
(defcall address-registry get-master* ((master-id b4))
    (:sol "getMaster(bytes4 masterId)" :returns "address")
  (or (get-master master-id) 0))
(defcall address-registry resolve-recipient* ((to address))
    (:sol "resolveRecipient(address to)" :returns "address")
  (resolve-recipient to))
(defcall address-registry resolve-virtual-address* ((address address))
    (:sol "resolveVirtualAddress(address virtualAddr)" :returns "address")
  (resolve-virtual-address address))
(defcall address-registry is-virtual-address ((address address))
    (:sol "isVirtualAddress(address addr)" :returns "bool")
  (virtual-address-p address))
(defcall address-registry decode-virtual-address ((address address))
    (:sol "decodeVirtualAddress(address addr)" :returns ("bool" "bytes4" "bytes6"))
  (if (virtual-address-p address)
      (values t (master-id address) (user-tag address))
      (values nil 0 0)))
(defcall address-registry is-implicitly-approved ((address address))
    (:sol "isImplicitlyApproved(address addr)" :returns "bool" :since :t5)
  (implicitly-approved-p address))
