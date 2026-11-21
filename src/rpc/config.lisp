(defpackage #:clempo.rpc.config
  (:use #:cl #:clempo.base #:clempo.rpc)
  (:local-nicknames (#:json #:clempo.json) (#:spec #:clempo.chainspec) (#:chain #:clempo.rpc.chain))
  (:documentation "Chain configuration methods: debug_chainConfig, eth_config (EIP-7910) and
tempo_forkSchedule, rendered from the chain spec's genesis config with EIP-2124 fork ids.")
  (:export #:crc32 #:fork-id #:fork-schedule))

(in-package #:clempo.rpc.config)

(defparameter *chain* :mainnet)

;;; CRC-32 (IEEE 802.3), for EIP-2124 fork ids

(defparameter +crc-table+
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8) (setf c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c)))))

(defun crc32-update (crc octets)
  (let ((c (logxor crc #xFFFFFFFF)))
    (loop for b across octets
          do (setf c (logxor (aref +crc-table+ (logand (logxor c b) #xFF)) (ash c -8))))
    (logxor c #xFFFFFFFF)))

(defun crc32 (octets) (crc32-update 0 octets))

;;; The schedule

(defun genesis-config () (json:jget (clempo.chainspec::chain-document (spec:load-chainspec *chain*)) "config"))

(defun fork-schedule ()
  "Tempo forks in the genesis config, oldest first: list of (name . activation-time)."
  (sort (loop for (key . value) in (cdr (genesis-config))
              when (and (> (length key) 5) (char= (char key 0) #\t) (digit-char-p (char key 1))
                        (string= "Time" key :start2 (- (length key) 4)))
                collect (cons (string-upcase (subseq key 0 (- (length key) 4))) value))
        (lambda (a b) (or (< (cdr a) (cdr b))
                          (and (= (cdr a) (cdr b)) (string< (car a) (car b)))))))

(defun genesis-time () (spec:genesis-timestamp *chain*))

(defun fork-id (time)
  "EIP-2124 fork hash with every activation after genesis up to TIME."
  (let ((crc (crc32 (spec:genesis-block-hash *chain*))) (seen '()))
    (dolist (f (fork-schedule) (format nil "0x~(~8,'0x~)" crc))
      (let ((at (cdr f)))
        (when (and (> at (genesis-time)) (<= at time) (not (member at seen)))
          (push at seen)
          (setf crc (crc32-update crc (integer->octets at 8))))))))

(defun now () (clempo.io:now-seconds))

;;; Methods

(defparameter +blob-params+ (json:jobject "baseFeeUpdateFraction" 5007716 "max" 9 "target" 6))

(defrpc "debug_chainConfig" () "the genesis chain config with its blob schedule"
  (append (genesis-config)
          (list (cons "blobSchedule" (json:jobject "cancun" +blob-params+ "osaka" +blob-params+
                                                   "prague" +blob-params+)))))

(defrpc "tempo_forkSchedule" () "Tempo forks, their activation times, fork ids and which are active"
  (let ((now (now)) (active nil))
    (json:jobject
     "schedule" (map 'simple-vector
                     (lambda (f)
                       (let ((on (<= (cdr f) now)))
                         (when on (setf active (car f)))
                         (apply #'json:jobject "name" (car f) "activationTime" (cdr f) "active" (json:jbool on)
                                (when on (list "forkId" (fork-id (cdr f)))))))
                     (fork-schedule))
     "active" active)))

(defparameter +precompiles+
  '(("BLAKE2F" . 9) ("BLS12_G1ADD" . #xb) ("BLS12_G1MSM" . #xc) ("BLS12_G2ADD" . #xd)
    ("BLS12_G2MSM" . #xe) ("BLS12_MAP_FP2_TO_G2" . #x11) ("BLS12_MAP_FP_TO_G1" . #x10)
    ("BLS12_PAIRING_CHECK" . #xf) ("BN254_ADD" . 6) ("BN254_MUL" . 7) ("BN254_PAIRING" . 8)
    ("ECREC" . 1) ("ID" . 4) ("KZG_POINT_EVALUATION" . #xa) ("MODEXP" . 5) ("P256VERIFY" . #x100)
    ("RIPEMD160" . 3) ("SHA256" . 2)))

(defparameter +system-contracts+
  '(("BEACON_ROOTS_ADDRESS" . #x000f3df6d732807ef1319fb7b8bb8522d0beac02)
    ("CONSOLIDATION_REQUEST_PREDEPLOY_ADDRESS" . #x0000bbddc7ce488642fb579f8b00f3a590007251)
    ("DEPOSIT_CONTRACT_ADDRESS" . 0)
    ("HISTORY_STORAGE_ADDRESS" . #x0000f90827f1c53a10cb7a02335b175320002935)
    ("WITHDRAWAL_REQUEST_PREDEPLOY_ADDRESS" . #x00000961ef480eb55e80d19ad83579a64c007002)))

(defun fork-config (time)
  (json:jobject "activationTime" time
                "blobSchedule" +blob-params+
                "chainId" (quantity->hex (chain:backend-chain-id chain:*backend*))
                "forkId" (fork-id time)
                "precompiles" (cons :object (loop for (k . a) in +precompiles+ collect (cons k (integer->hex a :bytes 20))))
                "systemContracts" (cons :object (loop for (k . a) in +system-contracts+
                                                      collect (cons k (integer->hex a :bytes 20))))))

(defrpc "eth_config" () "EIP-7910: the current, next and last fork configurations"
  (let* ((now (now))
         (times (remove-duplicates (mapcar #'cdr (fork-schedule))))
         (current (car (last (remove-if (lambda (at) (> at now)) times))))
         (next (find-if (lambda (at) (> at now)) times))
         (last (car (last times))))
    (json:jobject "current" (fork-config current)
                  "next" (if next (fork-config next) :null)
                  "last" (if (and last (> last now)) (fork-config last) :null))))
