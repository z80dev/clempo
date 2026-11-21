(in-package #:clempo.dex)

;;; StablecoinDEX (crates/precompiles/src/stablecoin_dex): the on-chain limit orderbook of
;;; USD TIP-20 pairs. Each pair is a book keyed by keccak(base ‖ quote); each side keeps a
;;; tick -> FIFO level map, a bitmap of initialized ticks and its best tick. Orders are
;;; records in a versioned layout (legacy, and from T8 the compact V1/V2 records of
;;; TIP-1062/TIP-1087); flip orders re-rest on the other side when filled; TIP-1060 storage
;;; credits freed by orders are kept per maker (T7). Every storage operation, account load
;;; and log is in the reference's order: gas depends on it.

(defconstant +dex+ +stablecoin-dex-address+)

(defconstant +min-tick+ -2000)
(defconstant +max-tick+ 2000)
(defconstant +tick-spacing+ 10)
(defconstant +price-scale+ 100000)
(defconstant +min-price+ 98000 "PRICE_SCALE + MIN_TICK")
(defconstant +max-price+ 102000 "PRICE_SCALE + MAX_TICK")
(defconstant +min-order-amount+ 100000000 "$100 at six decimals")
(defconstant +i16-min+ -32768 "best_bid_tick of a book without bids")
(defconstant +i16-max+ 32767 "best_ask_tick of a book without asks")
(defconstant +u256-max+ (1- (ash 1 256)))

;;; Storage. The tick level keeps its linked-list ends packed in the first slot (Rust's
;;; TickLevelLinks, read and written alone from T12) and the pre-T12 liquidity aggregate in the
;;; second.

(defstorable tick-level
  (head u128)
  (tail u128)
  (total-liquidity u128))

(defstorable tick-level-links
  (head u128)
  (tail u128))

(defstorable orderbook
  (base address)
  (quote-token address :label "quote")
  (bids (mapping i16 tick-level))
  (asks (mapping i16 tick-level))
  (best-bid-tick i16)
  (best-ask-tick i16)
  (book-id u32 :doc "T8: 1-based index into book-keys; zero means unset.")
  (bid-bitmap (mapping i16 u256))
  (ask-bitmap (mapping i16 u256)))

;;; Order records. ORDER is the legacy record and the logical order everything else works
;;; on; V1-ORDER (TIP-1062) drops the id and packs the flags, V2-ORDER (TIP-1087) replaces
;;; the book key with its book-keys index. The version byte sits at the top of slot 0.

(defstorable order
  (order-id u128)
  (maker address)
  (book-key b256)
  (is-bid bool)
  (tick i16)
  (amount u128)
  (remaining u128)
  (prev u128)
  (next u128)
  (is-flip bool)
  (flip-tick i16))

(defenum order-version legacy v1 v2)

(defstorable v1-order
  (maker address)
  (metadata u8)
  (tick i16)
  (flip-tick i16)
  (unused b6)
  (version order-version)
  (amount u128)
  (remaining u128)
  (prev u128)
  (next u128)
  (book-key b256))

(defstorable v2-order
  (maker address)
  (metadata u8)
  (tick i16)
  (flip-tick i16)
  (book-index u32)
  (unused b2)
  (version order-version)
  (amount u128)
  (remaining u128)
  (prev u128)
  (next u128))

(defcontract stablecoin-dex (:address +stablecoin-dex-address+)
  (books (mapping b256 orderbook))
  (orders (mapping u128 order) :doc "Versioned records, see READ-ORDER.")
  (balances (mapping address (mapping address u128)))
  (next-order-id u128)
  (book-keys (vec b256))
  (dex-storage-credits (mapping address u64)
                       :doc "T7: reusable order-storage credits per maker (TIP-1060)."))

;;; ABI.

(deferror unauthorized "Unauthorized()")
(deferror pair-does-not-exist "PairDoesNotExist()")
(deferror pair-already-exists "PairAlreadyExists()")
(deferror order-does-not-exist "OrderDoesNotExist()")
(deferror identical-tokens "IdenticalTokens()")
(deferror invalid-token "InvalidToken()")
(deferror tick-out-of-bounds "TickOutOfBounds(int16 tick)")
(deferror invalid-tick "InvalidTick()")
(deferror invalid-flip-tick "InvalidFlipTick()")
(deferror insufficient-balance "InsufficientBalance()")
(deferror insufficient-liquidity "InsufficientLiquidity()")
(deferror insufficient-output "InsufficientOutput()")
(deferror max-input-exceeded "MaxInputExceeded()")
(deferror below-minimum-order-size "BelowMinimumOrderSize(uint128 amount)")
(deferror invalid-base-token "InvalidBaseToken()")
(deferror order-not-stale "OrderNotStale()")
(deferror index-already-set "IndexAlreadySet()")

(defevent pair-created "PairCreated(bytes32 indexed key, address indexed base, address indexed quote)")
(defevent order-placed "OrderPlaced(uint128 indexed orderId, address indexed maker, address indexed token, uint128 amount, bool isBid, int16 tick, bool isFlipOrder, int16 flipTick)")
(defevent order-filled "OrderFilled(uint128 indexed orderId, address indexed maker, address indexed taker, uint128 amountFilled, bool partialFill)")
(defevent order-flipped "OrderFlipped(uint128 indexed orderId, address indexed maker, address indexed token, uint128 amount, bool isBid, int16 tick, int16 flipTick)")
(defevent flip-failed "FlipFailed(uint128 indexed orderId, address indexed maker, bytes4 reason)")
(defevent order-cancelled "OrderCancelled(uint128 indexed orderId)")

;;; Prices (orderbook.rs). Pure.

(defun tick-to-price (tick) (+ +price-scale+ tick))

(defun base-to-quote (base-amount tick round-up)
  "base * price / PRICE_SCALE, rounded up when ROUND-UP; NIL when it exceeds u128."
  (let* ((n (* base-amount (tick-to-price tick)))
         (r (if round-up (ceiling n +price-scale+) (floor n +price-scale+))))
    (and (<= r u128-max) r)))

(defun quote-to-base (quote-amount tick round-up)
  "quote * PRICE_SCALE / price, rounded up when ROUND-UP; NIL when it exceeds u128."
  (let* ((n (* quote-amount +price-scale+))
         (price (tick-to-price tick))
         (r (if round-up (ceiling n price) (floor n price))))
    (and (<= r u128-max) r)))

(defun wrap-signed (value bits)
  (let ((v (ldb (byte bits 0) value)))
    (if (logbitp (1- bits) v) (- v (ash 1 bits)) v)))

(defun price-to-tick* (price)
  "orderbook::price_to_tick: TickOutOfBounds outside [MIN_PRICE, MAX_PRICE], with the tick
computed in wrapping i32 then i16 arithmetic."
  (let ((tick (wrap-signed (- (wrap-signed price 32) +price-scale+) 16)))
    (unless (<= +min-price+ price +max-price+)
      (revert 'tick-out-of-bounds tick))
    tick))

(defun validate-tick-spacing (tick)
  (unless (zerop (rem tick +tick-spacing+)) (revert 'invalid-tick)))

(defun validate-order-terms (amount tick flip-tick bid-p)
  "validate_order_terms: the storage-free checks of place (FLIP-TICK NIL) and placeFlip."
  (unless (<= +min-tick+ tick +max-tick+) (revert 'tick-out-of-bounds tick))
  (validate-tick-spacing tick)
  (when flip-tick
    (unless (<= +min-tick+ flip-tick +max-tick+) (revert 'tick-out-of-bounds flip-tick))
    (unless (zerop (rem flip-tick +tick-spacing+)) (revert 'invalid-flip-tick))
    (when (or (and (= flip-tick tick) (fork-if (:before :t5 :doc "TIP-1030 same-tick flips") t nil))
              (and bid-p (< flip-tick tick))
              (and (not bid-p) (> flip-tick tick)))
      (revert 'invalid-flip-tick)))
  (when (< amount +min-order-amount+) (revert 'below-minimum-order-size amount)))

(defun compute-book-key (base quote)
  "keccak256(base ‖ quote), uncharged (alloy's keccak256)."
  (let ((buf (make-octets 40)))
    (replace buf (integer->octets base 20))
    (replace buf (integer->octets quote 20) :start1 20)
    (clempo.keccak:keccak256-integer buf)))

(defun taker-output (fill-amount tick bid-p)
  (if bid-p (base-to-quote fill-amount tick nil) fill-amount))

(defun step-exact-in (amount-in order bid-p)
  "step_exact_in: (values fill-amount accumulate next-amount), or NIL on overflow."
  (let ((remaining (order-remaining order)) (tick (order-tick order)))
    (multiple-value-bind (fill next)
        (if bid-p
            (values (min amount-in remaining) (max 0 (- amount-in remaining)))
            (let ((base-out (quote-to-base amount-in tick nil)))
              (unless base-out (return-from step-exact-in nil))
              (values (min base-out remaining)
                      (if (> base-out remaining)
                          (let ((needed (base-to-quote remaining tick t)))
                            (if (and needed (>= amount-in needed))
                                (- amount-in needed)
                                (return-from step-exact-in nil)))
                          0))))
      (let ((acc (taker-output fill tick (order-is-bid order))))
        (and acc (values fill acc next))))))

(defun step-exact-out (amount-out order bid-p)
  "step_exact_out: (values fill-amount accumulate next-amount), or NIL on overflow."
  (let ((remaining (order-remaining order)) (tick (order-tick order)))
    (multiple-value-bind (fill acc demand)
        (if bid-p
            (let ((needed (quote-to-base amount-out tick t)))
              (unless needed (return-from step-exact-out nil))
              (let ((fill (min needed remaining))) (values fill fill needed)))
            (let* ((fill (min amount-out remaining))
                   (in (base-to-quote fill tick t)))
              (unless in (return-from step-exact-out nil))
              (values fill in amount-out)))
      (let ((next (if (> demand remaining)
                      (let ((received (taker-output remaining tick (order-is-bid order))))
                        (if (and received (>= amount-out received))
                            (- amount-out received)
                            (return-from step-exact-out nil)))
                      0)))
        (values fill acc next)))))

(defun walk-resting-orders (order amount bid-p step settle)
  "walk_resting_orders: fill ORDER and its successors until AMOUNT is used. STEP is the
per-order arithmetic; SETTLE (order full-p fill-amount) settles one order and returns the
next order or NIL. Returns the running total."
  (let ((total 0))
    (loop while (plusp amount)
          do (let ((remaining (order-remaining order)))
               (multiple-value-bind (fill acc next) (funcall step amount order bid-p)
                 (unless fill (panic +panic-under-overflow+))
                 (when (< fill remaining)
                   (funcall settle order nil fill)
                   (setf total (checked+ total acc 128))
                   (return))
                 (let ((following (funcall settle order t fill)))
                   (setf total (checked+ total acc 128))
                   (unless following
                     (when (plusp next) (revert 'insufficient-liquidity))
                     (return))
                   (setf order following
                         amount next)))))
    total))

;;; Order records (order/storage.rs).

(defconstant +legacy-order-slots+ 6)
(defconstant +v1-order-slots+ 4)
(defconstant +v2-order-slots+ 3)

(defun order-slots (version)
  (ecase version (0 +legacy-order-slots+) (1 +v1-order-slots+) (2 +v2-order-slots+)))

(defun record-base (id) (place-slot (orders +dex+ id)))

(defun record-version-and-slot0 (id)
  "version_and_slot: (values version slot0). Before T8 every record is legacy and nothing
is loaded; from T8 slot 0 is loaded and its top byte is the version."
  (fork-if (:before :t8 :doc "TIP-1062 versioned order records")
      (values (enum-value order-version legacy) nil)
      (let* ((slot0 (sload +dex+ (record-base id)))
             (version (ldb (byte 8 248) slot0)))
        (unless (<= version 2)
          (fatal "unknown stablecoin DEX order storage version ~D" version))
        (values version slot0))))

(defun record-version (id) (values (record-version-and-slot0 id)))

(defun record-maker (id)
  "OrderHandler::maker: reuses the version load when the maker sits in slot 0."
  (multiple-value-bind (version slot0) (record-version-and-slot0 id)
    (if (= version (enum-value order-version legacy))
        (read (place +dex+ (+ (record-base id) 1) 'address 0))
        (if slot0
            (ldb (byte 160 0) slot0)
            (read (place +dex+ (record-base id) 'address 0))))))

(defun record-field (id name)
  "The packed u128 place of NAME (:remaining :prev :next) in ID's record layout."
  (multiple-value-bind (slot offset)
      (if (= (record-version id) (enum-value order-version legacy))
          (ecase name (:remaining (values 4 0)) (:prev (values 4 16)) (:next (values 5 0)))
          (ecase name (:remaining (values 1 16)) (:prev (values 2 0)) (:next (values 2 16))))
    (place +dex+ (+ (record-base id) slot) 'u128 offset)))

(defun metadata-byte (order)
  (logior (if (order-is-bid order) 1 0) (if (order-is-flip order) 2 0)))

(defun read-order (id &optional book-key)
  "OrderHandler::read / read_in_book: the logical order ID, resolving a V2 record's book from
its index unless BOOK-KEY is known."
  (let ((version (record-version id))
        (base (record-base id)))
    (ecase version
      (0 (read (place +dex+ base 'order)))
      (1 (let ((r (read (place +dex+ base 'v1-order))))
           (%make-order-from id (v1-order-maker r) (v1-order-book-key r) (v1-order-metadata r)
                             (v1-order-tick r) (v1-order-amount r) (v1-order-remaining r)
                             (v1-order-prev r) (v1-order-next r) (v1-order-flip-tick r))))
      (2 (let ((r (read (place +dex+ base 'v2-order))))
           (%make-order-from id (v2-order-maker r)
                             (or book-key (book-key-for-index (v2-order-book-index r)))
                             (v2-order-metadata r) (v2-order-tick r) (v2-order-amount r)
                             (v2-order-remaining r) (v2-order-prev r) (v2-order-next r)
                             (v2-order-flip-tick r)))))))

(defun %make-order-from (id maker book-key metadata tick amount remaining prev next flip-tick)
  (make-order :order-id id :maker maker :book-key book-key :is-bid (logbitp 0 metadata)
              :tick tick :amount amount :remaining remaining :prev prev :next next
              :is-flip (logbitp 1 metadata) :flip-tick flip-tick))

(defun write-order (order &optional (book-id nil book-id-p))
  "OrderHandler::write / write_in_book: before T8 the legacy record; from T8 a V2 record when
the book has an index (BOOK-ID, 1-based, when known; else looked up) and V1 otherwise,
clearing slots the old record used beyond the new one."
  (let* ((id (order-order-id order))
         (base (record-base id)))
    (fork-if (:before :t8)
        (write (place +dex+ base 'order) order)
        (multiple-value-bind (old-version slot0) (record-version-and-slot0 id)
          (let* ((old-slots (order-slots old-version))
                 (index (if book-id-p
                            (and (plusp book-id) (1- book-id))
                            (book-key-index (order-book-key order))))
                 (new-slots
                   (if index
                       (progn
                         (write (place +dex+ base 'v2-order)
                                (make-v2-order :maker (order-maker order) :metadata (metadata-byte order)
                                               :tick (order-tick order) :flip-tick (order-flip-tick order)
                                               :book-index index :unused 0
                                               :version (enum-value order-version v2)
                                               :amount (order-amount order)
                                               :remaining (order-remaining order)
                                               :prev (order-prev order) :next (order-next order)))
                         +v2-order-slots+)
                       (progn
                         (write (place +dex+ base 'v1-order)
                                (make-v1-order :maker (order-maker order) :metadata (metadata-byte order)
                                               :tick (order-tick order) :flip-tick (order-flip-tick order)
                                               :unused 0 :version (enum-value order-version v1)
                                               :amount (order-amount order)
                                               :remaining (order-remaining order)
                                               :prev (order-prev order) :next (order-next order)
                                               :book-key (order-book-key order)))
                         +v1-order-slots+))))
            (when (or (null slot0) (/= slot0 0))
              (loop for offset from new-slots below old-slots
                    do (sstore +dex+ (+ base offset) 0))))))))

(defun delete-order-record (id)
  "OrderHandler::delete: zero every slot of the record's layout."
  (let ((slots (order-slots (record-version id)))
        (base (record-base id)))
    (loop for offset from 0 below slots do (sstore +dex+ (+ base offset) 0))))

;;; Books and tick levels (orderbook.rs).

(defun book-place (book-key) (books +dex+ book-key))

(defun read-book (book-key) (read (book-place book-key)))

(defun book-initialized-p (book) (/= 0 (orderbook-base book)))

(defun best-tick-place (book-key bid-p)
  (field (book-place book-key) (if bid-p 'best-bid-tick 'best-ask-tick)))

(defun tick-level-place (book-key tick bid-p)
  (at (field (book-place book-key) (if bid-p 'bids 'asks)) tick))

(defun links-place (level-place)
  (place (place-address level-place) (place-slot level-place) 'tick-level-links))

(defun read-tick-level (book-key tick bid-p)
  "TickLevelHandler::read: from T12 only the links slot; the aggregate reads as zero."
  (let ((p (tick-level-place book-key tick bid-p)))
    (fork-if (:since :t12 :doc "tick levels keep no liquidity aggregate")
        (let ((links (read (links-place p))))
          (make-tick-level :head (tick-level-links-head links) :tail (tick-level-links-tail links)
                           :total-liquidity 0))
        (read p))))

(defun write-tick-level (book-key tick bid-p level)
  (let ((p (tick-level-place book-key tick bid-p)))
    (fork-if (:since :t12)
        (write (links-place p) (make-tick-level-links :head (tick-level-head level)
                                                      :tail (tick-level-tail level)))
        (write p level))))

(defun delete-tick-level (book-key tick bid-p)
  (let ((p (tick-level-place book-key tick bid-p)))
    (fork-if (:since :t12)
        (delete (links-place p))
        (delete p))))

(defun bitmap-place (book-key bid-p word-index)
  (at (field (book-place book-key) (if bid-p 'bid-bitmap 'ask-bitmap)) word-index))

(defun tick-word-index (tick)
  (unless (<= +min-tick+ tick +max-tick+) (revert 'invalid-tick))
  (ash tick -8))

(defun set-tick-bit (book-key tick bid-p)
  (let* ((p (bitmap-place book-key bid-p (tick-word-index tick)))
         (word (read p)))
    (write p (logior word (ash 1 (ldb (byte 8 0) tick))))))

(defun delete-tick-bit (book-key tick bid-p)
  (let* ((p (bitmap-place book-key bid-p (tick-word-index tick)))
         (word (read p)))
    (write p (logandc2 word (ash 1 (ldb (byte 8 0) tick))))))

(defun next-initialized-tick (book-key tick bid-p)
  "(values tick found-p): the nearest initialized tick below TICK for bids, above for asks,
one bitmap word per SLOAD."
  (if bid-p
      (next-initialized-bid-tick book-key tick)
      (next-initialized-ask-tick book-key tick)))

(defun next-initialized-ask-tick (book-key tick)
  (when (>= tick +max-tick+) (return-from next-initialized-ask-tick (values +max-tick+ nil)))
  (let ((next (1+ tick))
        (max-word (ash +max-tick+ -8)))
    (loop
      (let ((word-index (ash next -8)))
        (when (> word-index max-word) (return (values next nil)))
        (let* ((bit (ldb (byte 8 0) next))
               (masked (logand (read (bitmap-place book-key nil word-index))
                               (ldb (byte 256 0) (ash +u256-max+ bit)))))
          (unless (zerop masked)
            (let ((found (logior (ash word-index 8)
                                 (1- (integer-length (logand masked (- masked)))))))
              (return (values found (<= found +max-tick+)))))
          (let ((next-word (1+ word-index)))
            (when (> next-word max-word) (return (values (ash next-word 8) nil)))
            (setf next (ash next-word 8))))))))

(defun next-initialized-bid-tick (book-key tick)
  (when (<= tick +min-tick+) (return-from next-initialized-bid-tick (values +min-tick+ nil)))
  (let ((next (1- tick))
        (min-word (ash +min-tick+ -8)))
    (loop
      (let ((word-index (ash next -8)))
        (when (< word-index min-word) (return (values next nil)))
        (let* ((bit (ldb (byte 8 0) next))
               (masked (logand (read (bitmap-place book-key t word-index))
                               (1- (ash 1 (1+ bit))))))
          (unless (zerop masked)
            (let ((found (logior (ash word-index 8) (1- (integer-length masked)))))
              (return (values found (>= found +min-tick+)))))
          (let ((prev-word (1- word-index)))
            (when (< prev-word min-word)
              (return (values (logior (ash prev-word 8) #xff) nil)))
            (setf next (logior (ash prev-word 8) #xff))))))))

;;; Book index (TIP-1087).

(defun book-key-index (book-key)
  "StablecoinDEX::book_key_index: the zero-based book-keys index persisted for BOOK-KEY, or
NIL when unset; PairDoesNotExist for an unknown book."
  (let ((book (read-book book-key)))
    (unless (book-initialized-p book) (revert 'pair-does-not-exist))
    (let ((id (orderbook-book-id book)))
      (and (plusp id) (1- id)))))

(defun book-key-for-index (index)
  "StablecoinDEX::book_key_for_index: the book key at INDEX of the append-only book-keys
vector; PairDoesNotExist past its end."
  (let ((p (vec-at (book-keys +dex+) index)))
    (unless p (revert 'pair-does-not-exist))
    (read p)))

(defun set-book-index (index)
  (let* ((book-key (book-key-for-index index))
         (current (book-key-index book-key)))
    (when current
      (if (= index current)
          (return-from set-book-index (values))
          (revert 'index-already-set)))
    (write (field (book-place book-key) 'book-id) (1+ index))
    (values)))

;;; TIP-1060 storage credits of the DEX's order records (T7).

(defun preserve-storage-credits ()
  "dispatch::preserve_storage_credits: Preserve mode for the DEX from T7."
  (fork-if (:since :t7 :doc "TIP-1060 storage credits")
      (funcall 'clempo.storage-credits:set-mode +dex+ 1)))

(defun credit-dex-storage-slots (user slots)
  "Add SLOTS reusable order-storage credits to USER's counter. Creating the counter spends
one of the DEX's own credits from the slots just granted."
  (when (or (zerop slots) (fork-if (:before :t7) t nil))
    (return-from credit-dex-storage-slots nil))
  (let* ((p (dex-storage-credits +dex+ user))
         (current (read p))
         (updated (min u64-max (+ current slots))))
    (if (/= current 0)
        (write p updated)
        (let ((delta (nth-value 1 (funcall 'clempo.storage-credits:with-budget
                                      +dex+ 1 (lambda () (write p updated))))))
          (unless (= delta -1)
            (fatal "DEX storage credit bookkeeping spend mismatch: reserved 1, delta ~D" delta))))
    nil))

(defun delete-order (order)
  "Delete ORDER's record; returns the DEX credits minted."
  (nth-value 1 (funcall 'clempo.storage-credits:track-minted-credits
                +dex+ (lambda () (delete-order-record (order-order-id order))))))

(defun rewrite-order (order book-id)
  "Rewrite ORDER's record; returns the DEX credits minted."
  (nth-value 1 (funcall 'clempo.storage-credits:track-minted-credits
                +dex+ (lambda () (write-order order book-id)))))

(defun unlink-neighbor-and-credit-maker (order-id update)
  (let ((credits (nth-value 1 (funcall 'clempo.storage-credits:track-minted-credits +dex+ update))))
    (unless (zerop credits)
      (credit-dex-storage-slots (record-maker order-id) credits))))

(defun write-order-spending-credits (order book-id)
  "Write a new order record spending at most its maker's DEX credits."
  (let* ((user (order-maker order))
         (p (dex-storage-credits +dex+ user))
         (credits (read p)))
    (if (zerop credits)
        (write-order order book-id)
        (progn
          (delete p)
          (let* ((delta (nth-value 1 (funcall 'clempo.storage-credits:with-budget
                                         +dex+ credits (lambda () (write-order order book-id)))))
                 (spent (if (minusp delta) (- delta) 0)))
            (credit-dex-storage-slots user (max 0 (- credits spent))))))))

(defun make-credit-deltas () (list :deltas))

(defun credit-slots (deltas user slots)
  "StorageCreditDeltas::credit_slots."
  (unless (zerop slots)
    (let ((entry (assoc user (rest deltas))))
      (if entry
          (setf (cdr entry) (min u64-max (+ (cdr entry) slots)))
          (push (cons user slots) (rest deltas))))))

(defun flush-credit-deltas (deltas)
  "StorageCreditDeltas::flush: credit each user, in address order (a BTreeMap)."
  (loop for (user . slots) in (sort (copy-list (rest deltas)) #'< :key #'car)
        do (credit-dex-storage-slots user slots)))

;;; Balances.

(defun balance-of (user token) (read (balances +dex+ user token)))

(defun set-balance (user token amount) (write (balances +dex+ user token) amount))

(defun increment-balance (user token amount)
  (set-balance user token (checked+ (balance-of user token) amount 128)))

(defun sub-balance (user token amount)
  (set-balance user token (checked- (balance-of user token) amount)))

(defun read-next-order-id ()
  "next_order_id: always at least 1."
  (max 1 (read (next-order-id +dex+))))

(defun increment-next-order-id ()
  (write (next-order-id +dex+) (1+ (read-next-order-id))))

;;; Token movements.

(defun transfer-out (token to amount)
  (funcall 'clempo.tip20:from-address token)
  (funcall 'clempo.tip20:transfer token +dex+ to amount))

(defun transfer-in (token sender amount)
  (funcall 'clempo.tip20:from-address token)
  (fork-if (:since :t5 :doc "TIP-1035 system transfers")
      (funcall 'clempo.tip20:system-transfer-from token +dex+ sender amount)
      (funcall 'clempo.tip20:transfer-from token +dex+ sender +dex+ amount)))

(defun decrement-balance-or-transfer-from (sender token amount check-pause)
  "Take AMOUNT of TOKEN from SENDER's DEX balance, pulling the shortfall from the wallet."
  (funcall 'clempo.tip20:from-address token)
  (funcall 'clempo.tip20:ensure-transfer-authorized token sender +dex+)
  (let ((balance (balance-of sender token)))
    (if (>= balance amount)
        (progn
          (when (and check-pause (fork-if (:since :t4) t nil))
            (funcall 'clempo.tip20:check-not-paused token))
          (sub-balance sender token amount))
        (progn
          (transfer-in token sender (checked- amount balance))
          (set-balance sender token 0)))))

(defun escrow-of (bid-p amount tick base quote)
  "(values escrow-token escrow-amount non-escrow-token): bids escrow quote rounded up."
  (if bid-p
      (values quote (or (base-to-quote amount tick t) (revert 'insufficient-balance)) base)
      (values base amount quote)))

;;; Pairs.

(defun create-pair (base)
  (unless (funcall 'clempo.tip20-factory:tip20-p base) (revert 'invalid-base-token))
  (funcall 'clempo.tip20:from-address base)
  (let ((quote (funcall 'clempo.tip20:quote-token base)))
    (funcall 'clempo.tip20:validate-usd-currency base)
    (funcall 'clempo.tip20:validate-usd-currency quote)
    (let ((book-key (compute-book-key base quote)))
      (when (book-initialized-p (read-book book-key)) (revert 'pair-already-exists))
      (write (book-place book-key)
             (make-orderbook :base base :quote-token quote
                             :best-bid-tick +i16-min+ :best-ask-tick +i16-max+
                             :book-id (fork-if (:since :t8 :doc "TIP-1087 book index")
                                          (1+ (ldb (byte 32 0) (vec-length (book-keys +dex+))))
                                          0)))
      (vec-push (book-keys +dex+) book-key)
      (emit +dex+ 'pair-created book-key base quote)
      book-key)))

(defun validate-or-create-pair (book token)
  (unless (book-initialized-p book) (create-pair token)))

;;; Placing orders.

(defun commit-order-to-book (order charge-credits)
  "Link ORDER at the tail of its tick level, maintain bitmap and best tick, and write its
record (spending maker credits for placements from T7; crediting rewrites from T8)."
  (let* ((book-key (order-book-key order))
         (tick (order-tick order))
         (bid-p (order-is-bid order))
         (book (read-book book-key))
         (book-id (orderbook-book-id book))
         (level (read-tick-level book-key tick bid-p))
         (prev-tail (tick-level-tail level)))
    (if (zerop prev-tail)
        (progn
          (setf (tick-level-head level) (order-order-id order)
                (tick-level-tail level) (order-order-id order))
          (set-tick-bit book-key tick bid-p)
          (if bid-p
              (when (> tick (orderbook-best-bid-tick book))
                (write (best-tick-place book-key t) tick))
              (when (< tick (orderbook-best-ask-tick book))
                (write (best-tick-place book-key nil) tick))))
        (progn
          (fork-if (:since :t8 :doc "link the previous tail by its next field alone")
              (write (record-field prev-tail :next) (order-order-id order))
              (let ((prev-order (read-order prev-tail book-key)))
                (setf (order-next prev-order) (order-order-id order))
                (write-order prev-order book-id)))
          (setf (order-prev order) prev-tail
                (tick-level-tail level) (order-order-id order))))
    (fork-if (:before :t12)
        (setf (tick-level-total-liquidity level)
              (checked+ (tick-level-total-liquidity level) (order-remaining order) 128)))
    (write-tick-level book-key tick bid-p level)
    (cond ((and charge-credits (fork-if (:since :t7) t nil))
           (write-order-spending-credits order book-id))
          ((and (not charge-credits) (fork-if (:since :t8 :doc "flip rewrites credit freed slots") t nil))
           (credit-dex-storage-slots (order-maker order) (rewrite-order order book-id)))
          (t (write-order order book-id)))))

(defun place-order (sender token amount bid-p tick)
  (funcall 'clempo.tip20:from-address token)
  (let* ((quote (funcall 'clempo.tip20:quote-token token))
         (book-key (compute-book-key token quote)))
    (validate-or-create-pair (read-book book-key) token)
    (validate-order-terms amount tick nil bid-p)
    (multiple-value-bind (escrow-token escrow-amount non-escrow) (escrow-of bid-p amount tick token quote)
      (funcall 'clempo.tip20:from-address non-escrow)
      (funcall 'clempo.tip20:ensure-transfer-authorized non-escrow +dex+ sender)
      (fork-if (:since :t4 :doc "non-escrow token must not be paused")
          (funcall 'clempo.tip20:check-not-paused non-escrow))
      (decrement-balance-or-transfer-from sender escrow-token escrow-amount t)
      (let ((order-id (read-next-order-id)))
        (increment-next-order-id)
        (commit-order-to-book (make-order :order-id order-id :maker sender :book-key book-key
                                          :is-bid bid-p :tick tick :amount amount
                                          :remaining amount)
                              t)
        (emit +dex+ 'order-placed order-id sender token amount bid-p tick nil 0)
        order-id))))

(defun place-flip (sender token amount bid-p tick flip-tick internal-balance-only)
  (funcall 'clempo.tip20:from-address token)
  (let* ((quote (funcall 'clempo.tip20:quote-token token))
         (book-key (compute-book-key token quote)))
    (with-checkpoint ()
      (validate-or-create-pair (read-book book-key) token)
      (validate-order-terms amount tick flip-tick bid-p)
      (multiple-value-bind (escrow-token escrow-amount non-escrow)
          (escrow-of bid-p amount tick token quote)
        (funcall 'clempo.tip20:from-address non-escrow)
        (funcall 'clempo.tip20:ensure-transfer-authorized non-escrow +dex+ sender)
        (fork-if (:since :t4) (funcall 'clempo.tip20:check-not-paused non-escrow))
        (if internal-balance-only
            (progn
              (funcall 'clempo.tip20:from-address escrow-token)
              (funcall 'clempo.tip20:ensure-transfer-authorized escrow-token sender +dex+)
              (fork-if (:since :t4) (funcall 'clempo.tip20:check-not-paused escrow-token))
              (when (< (balance-of sender escrow-token) escrow-amount)
                (revert 'insufficient-balance))
              (sub-balance sender escrow-token escrow-amount))
            (decrement-balance-or-transfer-from sender escrow-token escrow-amount t))
        (let ((order-id (read-next-order-id)))
          (fork-if (:since :t1c :doc "skip a redundant next-order-id load")
              (write (next-order-id +dex+) (1+ order-id))
              (increment-next-order-id))
          (commit-order-to-book (make-order :order-id order-id :maker sender :book-key book-key
                                            :is-bid bid-p :tick tick :amount amount
                                            :remaining amount :is-flip t :flip-tick flip-tick)
                                t)
          (emit +dex+ 'order-placed order-id sender token amount bid-p tick t flip-tick)
          order-id)))))

(defun flip-in-place (order base quote)
  "T5: re-rest a filled flip order on the other side under the same id (TIP-1056)."
  (with-checkpoint ()
    (let ((flipped (make-order :order-id (order-order-id order) :maker (order-maker order)
                               :book-key (order-book-key order) :is-bid (not (order-is-bid order))
                               :tick (order-flip-tick order) :amount (order-amount order)
                               :remaining (order-amount order) :is-flip t
                               :flip-tick (order-tick order))))
      (multiple-value-bind (escrow-token escrow-amount non-escrow)
          (escrow-of (order-is-bid flipped) (order-amount flipped) (order-tick flipped) base quote)
        (let ((maker (order-maker flipped)))
          (when (< (balance-of maker escrow-token) escrow-amount)
            (revert 'insufficient-balance))
          (funcall 'clempo.tip20:from-address escrow-token)
          (funcall 'clempo.tip20:check-not-paused escrow-token)
          (funcall 'clempo.tip20:ensure-transfer-authorized escrow-token maker +dex+)
          (funcall 'clempo.tip20:from-address non-escrow)
          (funcall 'clempo.tip20:check-not-paused non-escrow)
          (funcall 'clempo.tip20:ensure-transfer-authorized non-escrow +dex+ maker)
          (sub-balance maker escrow-token escrow-amount)
          (commit-order-to-book flipped nil)
          (emit +dex+ 'order-flipped (order-order-id flipped) maker base (order-amount flipped)
                (order-is-bid flipped) (order-tick flipped) (order-flip-tick flipped)))))))

;;; Fills.

(defun system-error-p (condition)
  "TempoPrecompileError::is_system_error: halts, fatal errors and panics."
  (typecase condition
    (precompile-revert
     (or (eq (precompile-revert-name condition) 'panic)
         (let ((data (precompile-revert-data condition)))
           (and (>= (length data) 4)
                (= (octets->integer (subseq data 0 4)) (clempo.abi:abi-selector "Panic(uint256)"))))))
    (t t)))

(defun error-selector (condition)
  "TempoPrecompileError::selector: the revert's 4-byte selector, zero otherwise."
  (if (typep condition 'precompile-revert)
      (let ((data (precompile-revert-data condition)))
        (if (>= (length data) 4) (octets->integer (subseq data 0 4)) 0))
      0))

(defun partial-fill-order (order level fill-amount taker)
  (let* ((book-key (order-book-key order))
         (book (read-book book-key))
         (new-remaining (- (order-remaining order) fill-amount)))
    (write (record-field (order-order-id order) :remaining) new-remaining)
    (setf (order-remaining order) new-remaining)
    (if (order-is-bid order)
        (increment-balance (order-maker order) (orderbook-base book) fill-amount)
        (increment-balance (order-maker order) (orderbook-quote-token book)
                           (or (base-to-quote fill-amount (order-tick order) t)
                               (panic +panic-under-overflow+))))
    (fork-if (:before :t12)
        (progn
          (setf (tick-level-total-liquidity level)
                (checked- (tick-level-total-liquidity level) fill-amount))
          (write-tick-level book-key (order-tick order) (order-is-bid order) level)))
    (emit +dex+ 'order-filled (order-order-id order) (order-maker order) taker fill-amount t)))

(defun fill-order (deltas book-key order level taker)
  "Fill ORDER completely, flip or delete it, and advance. Returns (values level order) of
the next order to fill, or NIL when the book side is exhausted."
  (let* ((book (read-book book-key))
         (fill-amount (order-remaining order))
         (bid-p (order-is-bid order))
         (tick (order-tick order)))
    (if bid-p
        (increment-balance (order-maker order) (orderbook-base book) fill-amount)
        (increment-balance (order-maker order) (orderbook-quote-token book)
                           (or (base-to-quote fill-amount tick t) (panic +panic-under-overflow+))))
    (emit +dex+ 'order-filled (order-order-id order) (order-maker order) taker fill-amount nil)
    (if (order-is-flip order)
        (let ((failure
                (handler-case
                    (progn
                      (fork-if (:since :t5 :doc "TIP-1056 in-place flips")
                          (flip-in-place order (orderbook-base book) (orderbook-quote-token book))
                          (place-flip (order-maker order) (orderbook-base book) (order-amount order)
                                      (not bid-p) (order-flip-tick order) tick t))
                      nil)
                  (precompile-condition (c) c))))
          ;; Business errors don't block the swap; system errors propagate from T1A.
          (when failure
            (when (and (system-error-p failure) (fork-if (:since :t1a) t nil))
              (error failure))
            (fork-if (:since :t5 :doc "FlipFailed")
                (emit +dex+ 'flip-failed (order-order-id order) (order-maker order)
                      (error-selector failure))))
          (unless (and (fork-if (:since :t5) t nil) (null failure))
            (credit-slots deltas (order-maker order) (delete-order order))))
        (credit-slots deltas (order-maker order) (delete-order order)))
    (if (zerop (order-next order))
        (progn
          (delete-tick-level book-key tick bid-p)
          (delete-tick-bit book-key tick bid-p)
          (multiple-value-bind (next found) (next-initialized-tick book-key tick bid-p)
            (write (best-tick-place book-key bid-p)
                   (cond (found next) (bid-p +i16-min+) (t +i16-max+)))
            (when found
              (let ((new-level (read-tick-level book-key next bid-p)))
                (values new-level (read-order (tick-level-head new-level) book-key))))))
        (let ((next-id (order-next order)))
          (setf (tick-level-head level) next-id)
          (let ((credits (nth-value 1 (funcall 'clempo.storage-credits:track-minted-credits
                                       +dex+ (lambda () (delete (record-field next-id :prev)))))))
            (fork-if (:before :t12)
                (setf (tick-level-total-liquidity level)
                      (checked- (tick-level-total-liquidity level) fill-amount)))
            (write-tick-level book-key tick bid-p level)
            (let ((new-order (read-order next-id book-key)))
              (credit-slots deltas (order-maker new-order) credits)
              (values level new-order)))))))

(defun best-price-level (book-key bid-p)
  "get_best_price_level: the level at the side's best tick, InsufficientLiquidity if none."
  (let* ((book (read-book book-key))
         (tick (if bid-p (orderbook-best-bid-tick book) (orderbook-best-ask-tick book))))
    (when (= tick (if bid-p +i16-min+ +i16-max+)) (revert 'insufficient-liquidity))
    (read-tick-level book-key tick bid-p)))

(defun fill-orders (deltas book-key bid-p amount taker step)
  "fill_orders_exact_in / fill_orders_exact_out over one book."
  (let* ((level (best-price-level book-key bid-p))
         (order (read-order (tick-level-head level) book-key)))
    (walk-resting-orders
     order amount bid-p step
     (lambda (order full-p fill)
       (if full-p
           (multiple-value-bind (new-level new-order) (fill-order deltas book-key order level taker)
             (when new-order (setf level new-level))
             new-order)
           (progn (partial-fill-order order level fill taker) nil))))))

;;; Routes.

(defun find-path-to-root (token)
  (let ((path (list token)))
    (loop until (= token +path-usd-address+)
          do (funcall 'clempo.tip20:from-address token)
             (setf token (funcall 'clempo.tip20:quote-token token))
             (push token path))
    (nreverse path)))

(defun validate-and-build-route (path)
  "Each hop's (book-key . base-for-quote-p); every input token must not be paused (T3)."
  (loop for (token-in token-out) on path
        while token-out
        collect (progn
                  (funcall 'clempo.tip20:from-address token-in)
                  (fork-if (:since :t3 :doc "route tokens must not be paused")
                      (funcall 'clempo.tip20:check-not-paused token-in))
                  (multiple-value-bind (base quote)
                      (if (= (funcall 'clempo.tip20:quote-token token-in) token-out)
                          (values token-in token-out)
                          (progn
                            (funcall 'clempo.tip20:from-address token-out)
                            (if (= (funcall 'clempo.tip20:quote-token token-out) token-in)
                                (values token-out token-in)
                                (revert 'pair-does-not-exist))))
                    (let ((book-key (compute-book-key base quote)))
                      (when (zerop (orderbook-base (read-book book-key)))
                        (revert 'pair-does-not-exist))
                      (cons book-key (= token-in base)))))))

(defun find-trade-path (token-in token-out)
  (when (= token-in token-out) (revert 'identical-tokens))
  (unless (and (tip20-address-p token-in) (tip20-address-p token-out)) (revert 'invalid-token))
  (funcall 'clempo.tip20:from-address token-in)
  (let ((in-quote (funcall 'clempo.tip20:quote-token token-in)))
    (funcall 'clempo.tip20:from-address token-out)
    (let ((out-quote (funcall 'clempo.tip20:quote-token token-out)))
      (when (or (= in-quote token-out) (= out-quote token-in))
        (return-from find-trade-path (validate-and-build-route (list token-in token-out))))))
  (let* ((path-in (find-path-to-root token-in))
         (path-out (find-path-to-root token-out))
         (lca (or (find-if (lambda (token) (member token path-out)) path-in)
                  (revert 'pair-does-not-exist))))
    (validate-and-build-route
     (append (subseq path-in 0 (1+ (position lca path-in)))
             (reverse (subseq path-out 0 (position lca path-out)))))))

;;; Swaps.

(defun swap-exact-amount-in (sender token-in token-out amount-in min-amount-out)
  (let ((route (find-trade-path token-in token-out))
        (deltas (make-credit-deltas)))
    (decrement-balance-or-transfer-from sender token-in amount-in nil)
    (let ((amount amount-in))
      (loop for (book-key . base-for-quote) in route
            do (setf amount (fill-orders deltas book-key base-for-quote amount sender #'step-exact-in)))
      (when (< amount min-amount-out) (revert 'insufficient-output))
      (transfer-out token-out sender amount)
      (flush-credit-deltas deltas)
      amount)))

(defun swap-exact-amount-out (sender token-in token-out amount-out max-amount-in)
  (let ((route (find-trade-path token-in token-out))
        (deltas (make-credit-deltas))
        (amount amount-out))
    (loop for (book-key . base-for-quote) in (reverse route)
          do (setf amount (fill-orders deltas book-key base-for-quote amount sender #'step-exact-out)))
    (when (> amount max-amount-in) (revert 'max-input-exceeded))
    (decrement-balance-or-transfer-from sender token-in amount nil)
    (transfer-out token-out sender amount-out)
    (flush-credit-deltas deltas)
    amount))

;;; Quotes.

(defun next-order-after (book-key order bid-p)
  "The order a swap fills after consuming ORDER, or NIL."
  (if (/= 0 (order-next order))
      (read-order (order-next order) book-key)
      (multiple-value-bind (tick found) (next-initialized-tick book-key (order-tick order) bid-p)
        (when found
          (read-order (tick-level-head (read-tick-level book-key tick bid-p)) book-key)))))

(defun quote-per-order (book-key amount bid-p step)
  "T12: walk the book order by order with the swap's arithmetic, settling nothing."
  (let* ((level (best-price-level book-key bid-p))
         (order (read-order (tick-level-head level) book-key)))
    (walk-resting-orders order amount bid-p step
                         (lambda (order full-p fill)
                           (declare (ignore fill))
                           (and full-p (next-order-after book-key order bid-p))))))

(defun best-tick-or-fail (book-key bid-p)
  (let* ((book (read-book book-key))
         (tick (if bid-p (orderbook-best-bid-tick book) (orderbook-best-ask-tick book))))
    (when (or (= tick +i16-min+) (= tick +i16-max+)) (revert 'insufficient-liquidity))
    tick))

(defun quote-exact-in-per-tick (book-key amount-in bid-p)
  "Pre-T12 quote over tick aggregates, rounding once per tick."
  (let ((remaining amount-in) (amount-out 0)
        (tick (best-tick-or-fail book-key bid-p)))
    (loop while (plusp remaining)
          do (let ((liquidity (tick-level-total-liquidity (read-tick-level book-key tick bid-p))))
               (if (zerop liquidity)
                   (multiple-value-bind (next found) (next-initialized-tick book-key tick bid-p)
                     (unless found (revert 'insufficient-liquidity))
                     (setf tick next))
                   (multiple-value-bind (fill out consumed)
                       (if bid-p
                           (let ((fill (min remaining liquidity)))
                             (values fill (or (base-to-quote fill tick nil) (panic +panic-under-overflow+))
                                     fill))
                           (let* ((base (or (quote-to-base remaining tick nil)
                                            (panic +panic-under-overflow+)))
                                  (fill (min base liquidity)))
                             (values fill fill (or (base-to-quote fill tick t)
                                                   (panic +panic-under-overflow+)))))
                     (setf remaining (checked- remaining consumed)
                           amount-out (checked+ amount-out out 128))
                     (if (= fill liquidity)
                         (multiple-value-bind (next found) (next-initialized-tick book-key tick bid-p)
                           (when (and (not found) (plusp remaining)) (revert 'insufficient-liquidity))
                           (setf tick next))
                         (return))))))
    amount-out))

(defun quote-exact-out-per-tick (book-key amount-out bid-p)
  "Pre-T12 quote over tick aggregates, rounding once per tick."
  (let ((remaining amount-out) (amount-in 0)
        (tick (best-tick-or-fail book-key bid-p)))
    (loop while (plusp remaining)
          do (let ((liquidity (tick-level-total-liquidity (read-tick-level book-key tick bid-p))))
               (if (zerop liquidity)
                   (multiple-value-bind (next found) (next-initialized-tick book-key tick bid-p)
                     (unless found (revert 'insufficient-liquidity))
                     (setf tick next))
                   (multiple-value-bind (fill in)
                       (if bid-p
                           (let ((fill (min (or (quote-to-base remaining tick t)
                                                (panic +panic-under-overflow+))
                                            liquidity)))
                             (values fill fill))
                           (let ((fill (min remaining liquidity)))
                             (values fill (or (base-to-quote fill tick t)
                                              (panic +panic-under-overflow+)))))
                     (let ((out (if bid-p
                                    (min (or (base-to-quote fill tick nil) (panic +panic-under-overflow+))
                                         remaining)
                                    fill)))
                       (setf remaining (max 0 (- remaining out))
                             amount-in (checked+ amount-in in 128)))
                     (if (= fill liquidity)
                         (multiple-value-bind (next found) (next-initialized-tick book-key tick bid-p)
                           (when (and (not found) (plusp remaining)) (revert 'insufficient-liquidity))
                           (setf tick next))
                         (return))))))
    amount-in))

(defun quote-exact-in (book-key amount bid-p)
  (fork-if (:since :t12 :doc "quotes walk orders like swaps")
      (quote-per-order book-key amount bid-p #'step-exact-in)
      (quote-exact-in-per-tick book-key amount bid-p)))

(defun quote-exact-out (book-key amount bid-p)
  (fork-if (:since :t12)
      (quote-per-order book-key amount bid-p #'step-exact-out)
      (quote-exact-out-per-tick book-key amount bid-p)))

(defun quote-swap-exact-amount-in (token-in token-out amount-in)
  (let ((amount amount-in))
    (loop for (book-key . base-for-quote) in (find-trade-path token-in token-out)
          do (setf amount (quote-exact-in book-key amount base-for-quote)))
    amount))

(defun quote-swap-exact-amount-out (token-in token-out amount-out)
  (let ((amount amount-out))
    (loop for (book-key . base-for-quote) in (reverse (find-trade-path token-in token-out))
          do (setf amount (quote-exact-out book-key amount base-for-quote)))
    amount))

;;; Cancellation and withdrawal.

(defun cancel-active-order (order)
  (let* ((book-key (order-book-key order))
         (tick (order-tick order))
         (bid-p (order-is-bid order))
         (prev (order-prev order))
         (next (order-next order))
         (level (read-tick-level book-key tick bid-p)))
    (if (/= prev 0)
        (unlink-neighbor-and-credit-maker prev (lambda () (write (record-field prev :next) next)))
        (setf (tick-level-head level) next))
    (if (/= next 0)
        (unlink-neighbor-and-credit-maker next (lambda () (write (record-field next :prev) prev)))
        (setf (tick-level-tail level) prev))
    (let ((level-changed
            (fork-if (:since :t12)
                (or (zerop prev) (zerop next))
                (progn
                  (setf (tick-level-total-liquidity level)
                        (checked- (tick-level-total-liquidity level) (order-remaining order)))
                  t))))
      (when (zerop (tick-level-head level))
        (delete-tick-bit book-key tick bid-p)
        (let ((book (read-book book-key)))
          (when (= tick (if bid-p (orderbook-best-bid-tick book) (orderbook-best-ask-tick book)))
            (multiple-value-bind (next-tick found) (next-initialized-tick book-key tick bid-p)
              (write (best-tick-place book-key bid-p)
                     (cond (found next-tick) (bid-p +i16-min+) (t +i16-max+)))))))
      (when level-changed (write-tick-level book-key tick bid-p level)))
    (let ((book (read-book book-key)))
      (if bid-p
          (increment-balance (order-maker order) (orderbook-quote-token book)
                             (or (base-to-quote (order-remaining order) tick t)
                                 (panic +panic-under-overflow+)))
          (increment-balance (order-maker order) (orderbook-base book) (order-remaining order))))
    (credit-dex-storage-slots (order-maker order) (delete-order order))
    (emit +dex+ 'order-cancelled (order-order-id order))
    (values)))

(defun cancel (sender order-id)
  (let ((order (read-order order-id)))
    (when (zerop (order-maker order)) (revert 'order-does-not-exist))
    (unless (= (order-maker order) sender) (revert 'unauthorized))
    (when (zerop (order-remaining order)) (revert 'order-does-not-exist))
    (cancel-active-order order)))

(defun authorized-for-token-p (token address role)
  "is_authorized_for_token: a failed policy lookup counts as unauthorized."
  (funcall 'clempo.tip20:from-address token)
  (let ((policy-id (funcall 'clempo.tip20:transfer-policy-id token)))
    (handler-case (funcall 'clempo.tip403:authorized-as-p policy-id address role)
      (precompile-revert (c)
        (if (funcall 'clempo.tip403:policy-lookup-error-p c) nil (error c))))))

(defun maker-authorized-p (order)
  (let ((book (read-book (order-book-key order))))
    (multiple-value-bind (token-in token-out)
        (if (order-is-bid order)
            (values (orderbook-quote-token book) (orderbook-base book))
            (values (orderbook-base book) (orderbook-quote-token book)))
      (and (authorized-for-token-p token-in (order-maker order) :sender)
           (fork-if (:since :t4 :doc "TIP-1015 payout authorization")
               (authorized-for-token-p token-out (order-maker order) :recipient)
               t)))))

(defun cancel-stale-order (order-id)
  (let ((order (read-order order-id)))
    (when (zerop (order-maker order)) (revert 'order-does-not-exist))
    (when (maker-authorized-p order) (revert 'order-not-stale))
    (cancel-active-order order)))

(defun withdraw (user token amount)
  (when (< (balance-of user token) amount) (revert 'insufficient-balance))
  (sub-balance user token amount)
  (transfer-out token user amount)
  (values))

;;; Views.

(defun get-order (order-id)
  (let ((order (read-order order-id)))
    (if (and (/= 0 (order-maker order)) (< (order-order-id order) (read-next-order-id)))
        order
        (revert 'order-does-not-exist))))

(defun get-price-level (base tick bid-p)
  "getTickLevel; from T12 the liquidity is summed over the level's orders."
  (funcall 'clempo.tip20:from-address base)
  (let* ((book-key (compute-book-key base (funcall 'clempo.tip20:quote-token base)))
         (level (read-tick-level book-key tick bid-p)))
    (fork-if (:since :t12)
        (loop with id = (tick-level-head level)
              initially (setf (tick-level-total-liquidity level) 0)
              until (zerop id)
              do (let ((order (read-order id book-key)))
                   (setf (tick-level-total-liquidity level)
                         (checked+ (tick-level-total-liquidity level) (order-remaining order) 128)
                         id (order-next order)))))
    level))

(defun tick-to-price* (tick)
  (fork-if (:since :t2 :doc "ticks must be aligned to TICK_SPACING")
      (validate-tick-spacing tick))
  (tick-to-price tick))

(defun price-to-tick (price)
  (let ((tick (price-to-tick* price)))
    (fork-if (:since :t2) (validate-tick-spacing tick))
    tick))

(defun order-tuple (o)
  (list (order-order-id o) (order-maker o) (order-book-key o) (order-is-bid o) (order-tick o)
        (order-amount o) (order-remaining o) (order-prev o) (order-next o) (order-is-flip o)
        (order-flip-tick o)))

;;; Dispatch (dispatch.rs).

(defmacro defmutation (name params sol &body body)
  "A mutating call: Preserve storage-credit mode for the DEX (T7), then BODY."
  `(defcall stablecoin-dex ,name ,params ,(append sol '(:mutates t))
     (preserve-storage-credits)
     ,@body))

(defmutation place* (token amount is-bid tick)
    (:sol "place(address token, uint128 amount, bool isBid, int16 tick)" :returns "uint128")
  (place-order sender token amount is-bid tick))

(defmutation place-flip* (token amount is-bid tick flip-tick)
    (:sol "placeFlip(address token, uint128 amount, bool isBid, int16 tick, int16 flipTick)"
     :returns "uint128")
  (place-flip sender token amount is-bid tick flip-tick nil))

(defcall stablecoin-dex balance-of* (user token)
    (:sol "balanceOf(address user, address token)" :returns "uint128")
  (balance-of user token))

(defcall stablecoin-dex get-order* (order-id)
    (:sol "getOrder(uint128 orderId)"
     :returns "(uint128,address,bytes32,bool,int16,uint128,uint128,uint128,uint128,bool,int16)")
  (order-tuple (get-order order-id)))

(defcall stablecoin-dex get-tick-level (base tick is-bid)
    (:sol "getTickLevel(address base, int16 tick, bool isBid)"
     :returns ("uint128" "uint128" "uint128"))
  (let ((level (get-price-level base tick is-bid)))
    (values (tick-level-head level) (tick-level-tail level) (tick-level-total-liquidity level))))

(defcall stablecoin-dex pair-key (token-a token-b)
    (:sol "pairKey(address tokenA, address tokenB)" :returns "bytes32")
  (compute-book-key token-a token-b))

(defcall stablecoin-dex books* (pair-key)
    (:sol "books(bytes32 pairKey)" :returns "(address,address,int16,int16)")
  (let ((book (read-book pair-key)))
    (list (orderbook-base book) (orderbook-quote-token book) (orderbook-best-bid-tick book)
          (orderbook-best-ask-tick book))))

(defcall stablecoin-dex next-order-id* ()
    (:sol "nextOrderId()" :returns "uint128")
  (read-next-order-id))

(defmutation create-pair* (base)
    (:sol "createPair(address base)" :returns "bytes32")
  (create-pair base))

(defmutation withdraw* (token amount)
    (:sol "withdraw(address token, uint128 amount)")
  (withdraw sender token amount))

(defmutation cancel* (order-id)
    (:sol "cancel(uint128 orderId)")
  (cancel sender order-id))

(defmutation cancel-stale-order* (order-id)
    (:sol "cancelStaleOrder(uint128 orderId)")
  (cancel-stale-order order-id))

(defmutation swap-exact-amount-in* (token-in token-out amount-in min-amount-out)
    (:sol "swapExactAmountIn(address tokenIn, address tokenOut, uint128 amountIn, uint128 minAmountOut)"
     :returns "uint128")
  (swap-exact-amount-in sender token-in token-out amount-in min-amount-out))

(defmutation swap-exact-amount-out* (token-in token-out amount-out max-amount-in)
    (:sol "swapExactAmountOut(address tokenIn, address tokenOut, uint128 amountOut, uint128 maxAmountIn)"
     :returns "uint128")
  (swap-exact-amount-out sender token-in token-out amount-out max-amount-in))

(defcall stablecoin-dex quote-swap-exact-amount-in* (token-in token-out amount-in)
    (:sol "quoteSwapExactAmountIn(address tokenIn, address tokenOut, uint128 amountIn)"
     :returns "uint128")
  (quote-swap-exact-amount-in token-in token-out amount-in))

(defcall stablecoin-dex quote-swap-exact-amount-out* (token-in token-out amount-out)
    (:sol "quoteSwapExactAmountOut(address tokenIn, address tokenOut, uint128 amountOut)"
     :returns "uint128")
  (quote-swap-exact-amount-out token-in token-out amount-out))

(defcall stablecoin-dex min-tick () (:sol "MIN_TICK()" :returns "int16") +min-tick+)
(defcall stablecoin-dex max-tick () (:sol "MAX_TICK()" :returns "int16") +max-tick+)
(defcall stablecoin-dex tick-spacing () (:sol "TICK_SPACING()" :returns "int16") +tick-spacing+)
(defcall stablecoin-dex price-scale () (:sol "PRICE_SCALE()" :returns "uint32") +price-scale+)
(defcall stablecoin-dex min-order-amount () (:sol "MIN_ORDER_AMOUNT()" :returns "uint128")
  +min-order-amount+)
(defcall stablecoin-dex min-price () (:sol "MIN_PRICE()" :returns "uint32") +min-price+)
(defcall stablecoin-dex max-price () (:sol "MAX_PRICE()" :returns "uint32") +max-price+)

(defcall stablecoin-dex tick-to-price** (tick)
    (:sol "tickToPrice(int16 tick)" :returns "uint32")
  (tick-to-price* tick))

(defcall stablecoin-dex price-to-tick** (price)
    (:sol "priceToTick(uint32 price)" :returns "int16")
  (price-to-tick price))

(defcall stablecoin-dex storage-credits (user)
    (:sol "storageCredits(address user)" :returns "uint64" :since :t7)
  (read (dex-storage-credits +dex+ user)))

(defcall stablecoin-dex book-index-for-key (book-key)
    (:sol "bookIndexForKey(bytes32 bookKey)" :returns ("bool" "uint32") :since :t8)
  (let ((index (book-key-index book-key)))
    (values (and index t) (or index 0))))

(defcall stablecoin-dex book-key-for-index* (index)
    (:sol "bookKeyForIndex(uint32 index)" :returns "bytes32" :since :t8)
  (book-key-for-index index))

(defmutation set-book-index* (index)
    (:sol "setBookIndex(uint32 index)" :since :t8)
  (set-book-index index))

(defdispatch stablecoin-dex (self input sender)
  (charge-input-cost input)
  (dispatch-calls))

(register-precompile +stablecoin-dex-address+ 'stablecoin-dex :genesis)
