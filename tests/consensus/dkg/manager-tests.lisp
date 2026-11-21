(in-package #:clempo.dkg.tests)

(deftest dkg-scalar-from-ikm ()
  ;; EIP-2333 test case 0: derive_master_SK(seed) = HKDF_mod_r(seed), blst_keygen's algorithm
  (is= 6083874454709270928345386274498605044986640685124978867557563392430687146096
       (scalar-from-ikm
        (hex->octets "c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04"))))

(deftest dkg-persisted-codecs ()
  (let* ((output (rust-output))
         (share (first (rust-shares)))
         (state (make-epoch-state :epoch 300 :seed (make-octets 32) :output output :share share
                                  :players (rust-keys) :syncers '() :is-full-dkg t))
         (octets (encode-epoch-state state)))
    ;; epoch varint, seed, output, ShareState::Plaintext(Some(share)), players, empty syncers, bool
    (is= "ac02" (hex (subseq octets 0 2)))
    (is (octets= octets (encode-epoch-state (decode-epoch-state octets))))
    (is (search (octets-concat (octets-of 0 1) (cw-encode 'share share)) octets))
    (is= "0001" (hex (subseq octets (- (length octets) 2))))
    (let ((none (encode-epoch-state (make-epoch-state :epoch 1 :seed (make-octets 32) :output output
                                                      :share nil :players (rust-keys)
                                                      :syncers '() :is-full-dkg nil))))
      (is (null (state-share (decode-epoch-state none))))))
  (let ((key (first (rust-keys))))
    (dolist (event (list (list :ack key (make-octets 64))
                         (list :finalized (make-octets 32) (make-octets 32) 1000)))
      (is (equalp event (decode-dkg-event (encode-dkg-event event)))))
    (is= (concatenate 'string "01" (hex key) (make-string 128 :initial-element #\0))
         (hex (encode-dkg-event (list :ack key (make-octets 64)))))
    (is= (concatenate 'string "03" (make-string 128 :initial-element #\0) "e807")
         (hex (encode-dkg-event (list :finalized (make-octets 32) (make-octets 32) 1000))))))

;;; Rust's DKG channel frames through the manager: our acks equal Rust's.

(defun rust-round-manager (round player-index)
  "A manager for participant PLAYER-INDEX in the rust_messages.json ROUND."
  (let* ((keys (rust-keys))
         (previous (eq (clempo.json:jget round "previous") :true))
         (store (make-memory-dkg-store))
         (epoch (clempo.json:jget round "epoch"))
         (state (make-epoch-state :epoch epoch :seed (make-octets 32) :output (rust-output)
                                  :share (and previous (nth player-index (rust-shares)))
                                  :players keys :syncers '() :is-full-dkg (not previous))))
    (dkg-store-put-state store epoch (encode-epoch-state state))
    (values (open-dkg-manager :seed (nth player-index (rust-seeds)) :store store
                              :epoch-length 1000 :t12-time 0
                              :last-finalized-height (1- (* epoch 1000))
                              :header-at (constantly nil))
            store)))

(defun strip-mux (hex epoch)
  (let* ((octets (hex->octets hex)) (r (make-cw-reader octets)))
    (is= epoch (read-varint r))
    (subseq octets (cw-reader-position r))))

(deftest dkg-manager-rust-frames ()
  (dolist (round (rust-rounds))
    (let ((epoch (clempo.json:jget round "epoch"))
          (managers (loop for i below 4 collect (rust-round-manager round i))))
      (loop for m across (clempo.json:jget round "messages")
            unless (clempo.json:jget m "signed_log")
              do (let* ((manager (nth (clempo.json:jget m "player") managers))
                        (dealer (nth (clempo.json:jget m "dealer") (rust-keys)))
                        (actions (dkg-receive manager dealer epoch
                                              (strip-mux (clempo.json:jget m "dealer_bytes") epoch))))
                   (is= 1 (length actions))
                   (destructuring-bind (kind to e octets) (first actions)
                     (is (eq :send kind)) (is (octets= dealer to)) (is= epoch e)
                     (is= (hex (strip-mux (clempo.json:jget m "ack_bytes") epoch)) (hex octets)))
                   ;; a resent dealing gets the same ack back
                   (is (equalp actions (dkg-receive manager dealer epoch
                                                    (strip-mux (clempo.json:jget m "dealer_bytes")
                                                               epoch))))))
      ;; a dealing from a non-dealer and garbage are errors (the validator blocks the peer)
      (let ((m (first managers)))
        (signals dkg-error (dkg-receive m (make-octets 32) epoch (octets-of 0 1 2)))
        (signals error (dkg-receive m (second (rust-keys)) epoch (octets-of 0 1 2)))))))

;;; A Tempo chain of validators driving managers: blocks proposed round-robin, finalized at
;;; once by everyone; the network delivers DKG messages in order.

(defstruct node seed key store manager (shares '()))

(defstruct chain
  nodes (blocks (make-hash-table)) epoch-length (t15-time nil) (drop (constantly nil))
  (queue '()) (identities '()) (outcomes '()))

(defun chain-block (chain height) (gethash height (chain-blocks chain)))

(defun node-header-at (chain)
  (lambda (height) (chain-block chain height)))

(defun deliver (chain node actions)
  (dolist (action actions)
    (ecase (first action)
      (:send (destructuring-bind (to epoch octets) (rest action)
               (unless (funcall (chain-drop chain) (node-key node) to)
                 (setf (chain-queue chain)
                       (append (chain-queue chain) (list (list (node-key node) to epoch octets)))))))
      (:exit nil)
      (:enter (destructuring-bind (epoch output share) (rest action)
                (push (list epoch output share) (node-shares node)))))))

(defun pump (chain)
  (loop while (chain-queue chain)
        do (destructuring-bind (from to epoch octets) (pop (chain-queue chain))
             (let ((node (find to (chain-nodes chain) :key #'node-key :test #'octets=)))
               (when (and node (node-manager node))
                 (deliver chain node (dkg-receive (node-manager node) from epoch octets)))))))

(defun live-nodes (chain) (remove nil (chain-nodes chain) :key #'node-manager))

(defun produce-block (chain height)
  (let* ((length (chain-epoch-length chain))
         (nodes (live-nodes chain))
         (proposer (nth (mod height (length nodes)) nodes))
         (m (node-manager proposer))
         (parent (chain-block chain (1- height)))
         (extra (if (boundary-height-p height length)
                    (let ((all (mapcar (lambda (n)
                                         (dkg-boundary-extra-data
                                          (node-manager n) parent
                                          (dkg-manager-next-players (node-manager n)) 3))
                                       nodes)))
                      ;; every validator computes the same outcome
                      (is (every (lambda (o) (octets= o (first all))) all))
                      (first all))
                    (or (dkg-dealer-log m (height-epoch height length)) (make-octets 0))))
         (block (make-dkg-block :height height :digest (clempo.sha2:sha256 (integer->octets height 8))
                                :parent (dkg-block-digest parent) :timestamp (* 2 height)
                                :extra-data extra)))
    (unless (or (boundary-height-p height length) (zerop (length extra)))
      ;; every validator accepts the log as the proposer's
      (dolist (n nodes)
        (is (octets= (node-key proposer)
                     (dkg-verify-dealer-log (node-manager n) (height-epoch height length)
                                            (* 2 height) extra)))))
    (setf (gethash height (chain-blocks chain)) block)
    (dolist (n nodes)
      (deliver chain n (dkg-finalized-block (node-manager n) block))
      (pump chain))
    (when (boundary-height-p height length)
      (let ((outcome (decode-onchain-dkg-outcome extra :strict t)))
        (push outcome (chain-outcomes chain))
        (push (outcome-network-identity outcome) (chain-identities chain))))
    block))

(defun open-node (chain node last-finalized)
  (multiple-value-bind (m actions)
      (open-dkg-manager :seed (node-seed node) :store (node-store node)
                        :epoch-length (chain-epoch-length chain) :t12-time 0
                        :t15-time (chain-t15-time chain)
                        :header-at (node-header-at chain) :last-finalized-height last-finalized)
    (setf (node-manager node) m)
    (deliver chain node actions)))

(defun make-test-chain (n epoch-length &key (entropy 1))
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy entropy))
         (seeds (make-seeds n))
         (keys (mapcar #'seed-key seeds))
         (chain (make-chain :epoch-length epoch-length)))
    (multiple-value-bind (output shares) (deal keys)
      (setf (gethash 0 (chain-blocks chain))
            (make-dkg-block :height 0 :digest (make-octets 32) :parent (make-octets 32)
                            :extra-data (encode-onchain-dkg-outcome
                                         (boundary-outcome output keys :epoch 0))))
      (setf (chain-nodes chain)
            (loop for seed in seeds
                  collect (make-node :seed seed :key (seed-key seed) :store (make-memory-dkg-store))))
      (dolist (node (chain-nodes chain))
        ;; the configured initial share, as a fresh validator gets it at genesis
        (multiple-value-bind (m actions)
            (open-dkg-manager :seed (node-seed node) :store (node-store node)
                              :epoch-length epoch-length :t12-time 0
                              :header-at (node-header-at chain) :last-finalized-height 0
                              :initial-share (cdr (assoc (node-key node) shares :test #'octets=)))
          (setf (node-manager node) m)
          (deliver chain node actions))))
    chain))

(defun check-epoch-shares (chain epoch)
  "Every node entered EPOCH with a share of its output, and the shares sign."
  (let* ((entries (mapcar (lambda (n) (find epoch (node-shares n) :key #'first)) (chain-nodes chain)))
         (output (second (first entries))))
    (is (every (lambda (e) (equalp output (second e))) entries))
    (is (every #'third entries))
    (check-shares output (mapcar (lambda (n e) (cons (node-key n) (third e)))
                                 (chain-nodes chain) entries))))

(deftest dkg-manager-chain ()
  ;; 4 validators, epochs of 20 blocks: reshares keep the identity; the outcome installing
  ;; epoch 3 schedules a full DKG for it (next-full-dkg-epoch 3), whose output changes it.
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 41))
         (chain (make-test-chain 4 20)))
    (loop for height from 1 below 80 do (produce-block chain height))
    (let ((outcomes (reverse (chain-outcomes chain)))
          (ids (reverse (chain-identities chain))))
      (is= '(1 2 3 4) (mapcar #'outcome-epoch outcomes))
      ;; enough dealer logs made it into every outcome (0 failures)
      (dolist (o outcomes) (is= 3 (length (outcome-dealers o))))
      (is (octets= (first ids) (second ids)))
      (is (octets= (second ids) (third ids)))
      (is (not (octets= (third ids) (fourth ids))))
      (is (outcome-is-next-full-dkg (third outcomes)))
      (dolist (epoch '(1 2 3 4)) (check-epoch-shares chain epoch)))))

(deftest dkg-manager-restart ()
  ;; A validator restarts in each phase of an epoch, once after missing three finalized
  ;; blocks (it reads them back from the chain), and still ends the epoch with a share.
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 42))
         (chain (make-test-chain 4 20))
         (victim (third (chain-nodes chain))))
    (loop for height from 1 below 40
          do (case height
               (23 (setf (node-manager victim) nil))     ; down for 23..25
               (26 (open-node chain victim 25))
               (31 (open-node chain victim 30)))
             (produce-block chain height))
    (let ((outcomes (chain-outcomes chain)))
      (is= 2 (outcome-epoch (first outcomes)))
      (is= 3 (length (outcome-dealers (first outcomes))))
      (check-epoch-shares chain 1)
      (check-epoch-shares chain 2))))

(defun tempo-dealer-pub (m)
  (let ((td (clempo.dkg::dkg-manager-dealer m)))
    (and td (clempo.dkg::tempo-dealer-pub-msg td))))

(deftest dkg-manager-restart-redeals-identically ()
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 43))
         (chain (make-test-chain 4 20))
         (victim (second (chain-nodes chain))))
    (loop for height from 1 below 23 do (produce-block chain height))
    (let ((before (tempo-dealer-pub (node-manager victim))))
      (open-node chain victim 22)
      (is (equalp before (tempo-dealer-pub (node-manager victim))))
      ;; acks survived: nothing left to resend
      (is (null (clempo.dkg::tempo-dealer-unsent
                 (clempo.dkg::dkg-manager-dealer (node-manager victim))))))))

(deftest dkg-manager-lost-dealings ()
  ;; One validator never receives dealings from another: that dealer's dealing for it is
  ;; revealed in the log; the ceremony still succeeds and everybody gets a share.
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 44))
         (chain (make-test-chain 4 20)))
    (let ((a (node-key (first (chain-nodes chain))))
          (b (node-key (second (chain-nodes chain)))))
      (setf (chain-drop chain) (lambda (from to) (and (octets= from a) (octets= to b)))))
    (loop for height from 1 below 40 do (produce-block chain height))
    (let ((outcomes (reverse (chain-outcomes chain))))
      (is= 2 (length outcomes))
      (check-epoch-shares chain 1)
      (check-epoch-shares chain 2))))

(deftest dkg-manager-notarized-branch ()
  ;; Before the dealer logs are finalized, the boundary outcome on a branch comes from the
  ;; logs on that branch; a missing ancestor is named.
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 45))
         (chain (make-test-chain 4 20))
         (m (node-manager (first (chain-nodes chain)))))
    (loop for height from 1 to 10 do (produce-block chain height))
    ;; a branch 11..18 carrying all four logs, not finalized
    (let ((logs (remove nil (mapcar (lambda (n) (dkg-dealer-log (node-manager n) 0))
                                    (chain-nodes chain))))
          (blocks '())
          (parent (chain-block chain 10)))
      (is= 4 (length logs))
      (loop for height from 11 to 18
            for extra = (or (nth (- height 11) logs) (make-octets 0))
            do (setf parent (make-dkg-block :height height
                                            :digest (clempo.sha2:sha256 (octets-concat (octets-of 7) (integer->octets height 8)))
                                            :parent (dkg-block-digest parent) :timestamp height
                                            :extra-data extra :parent-view height))
               (push parent blocks))
      ;; only the tip is known: the walk asks for its parent
      (multiple-value-bind (output missing height) (dkg-ceremony-output m (first blocks))
        (is (null output)) (is (eq :needs-ancestor missing)) (is= 17 height))
      ;; the finalized tip (10) is not needed
      (dolist (b (rest blocks)) (dkg-notarized-block m b))
      (let ((output (dkg-ceremony-output m (first blocks))))
        (is output)
        (is= 3 (length (output-dealers output)))
        ;; the same parent again comes from the cache
        (is (eq output (dkg-ceremony-output m (first blocks))))))))

;;; A Rust devnet's chain through the manager, as an observer.

(defun devnet-blocks ()
  (with-open-file (in (data-path "devnet_r6_blocks.jsonl"))
    (loop for line = (read-line in nil)
          while line
          collect (let ((j (clempo.json:parse-json line)))
                    (list (clempo.json:jget j "height")
                          (hex->octets (clempo.json:jget j "hash"))
                          (hex->octets (clempo.json:jget j "extra_data")))))))

(deftest dkg-manager-rust-devnet ()
  ;; Fed the finalized blocks of an all-Rust devnet, the manager rebuilds each round from the
  ;; previous boundary, accepts every dealer log, and computes each boundary block's extraData
  ;; byte for byte (as a proposer would) from the parent; it then moves to the next epoch.
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 7))
         (rows (devnet-blocks))
         (blocks (make-hash-table)))
    (loop for (height hash extra) in rows
          for parent = (make-octets 32) then (second (nth (1- height) rows))
          do (setf (gethash height blocks)
                   (make-dkg-block :height height :digest hash :parent parent
                                   :timestamp 0 :extra-data extra)))
    (let ((m (open-dkg-manager :seed (clempo.io:random-octets 32) :store (make-memory-dkg-store)
                               :epoch-length 60 :t12-time 0 :t15-time nil
                               :header-at (lambda (h) (gethash h blocks))
                               :last-finalized-height 0))
          (boundaries 0) (logs 0))
      (is= 0 (dkg-manager-current-epoch m))
      (loop for height from 1 below (length rows)
            for block = (gethash height blocks)
            for extra = (dkg-block-extra-data block)
            do (cond ((boundary-height-p height 60)
                      (let ((outcome (decode-onchain-dkg-outcome extra :strict t)))
                        (is= (hex extra)
                             (hex (dkg-boundary-extra-data
                                   m (gethash (1- height) blocks) (outcome-next-players outcome)
                                   (if (outcome-is-next-full-dkg outcome)
                                       (outcome-epoch outcome)
                                       0))))
                        (incf boundaries)))
                     ((plusp (length extra))
                      (is (dkg-verify-dealer-log m (height-epoch height 60) 0 extra))
                      (incf logs)))
               (dkg-finalized-block m block))
      (is= 2 boundaries)
      (is= 12 logs)                     ; a dealer re-proposes its log until one is finalized
      (is= 2 (dkg-manager-current-epoch m))
      ;; the reshares kept the devnet's identity
      (is (octets= (outcome-network-identity (decode-onchain-dkg-outcome
                                              (dkg-block-extra-data (gethash 0 blocks))))
                   (sharing-public (output-public (dkg-manager-output m))))))))
