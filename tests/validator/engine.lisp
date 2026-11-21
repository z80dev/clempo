(in-package #:clempo.validator-process.tests)

(defun empty-proposal (db chain parent seconds)
  (clempo.builder:built-payload-block
   (clempo.builder:build-payload db parent (clempo.builder:make-payload-attributes :timestamp seconds)
                                 :chain chain :commit t)))

(deftest speculative-notarized-branch ()
  (let* ((chain (clempo.chainspec:load-chainspec :mainnet))
         (db (clempo.executor:genesis-db :chain chain))
         (root (clempo.store:db-root-hash db)) (digest (clempo.store:db-hash db))
         (genesis (clempo.headers:decode-header (clempo.chainspec:genesis-header-rlp chain :state-root root)))
         (app (net::%make-app :head genesis :db db :chain chain)))
    (multiple-value-bind (a b)
        (clempo.store:with-speculative-blocks (db)
          (let ((a (empty-proposal db chain genesis 1)))
            (values a (empty-proposal db chain (clempo.blocks:block-header a) 2))))
      (let ((a-hash (clempo.blocks:block-hash a)) (b-hash (net::stash-block app b)))
        (multiple-value-bind (blocks valid missing) (net::branch app b-hash)
          (is (null blocks)) (is (not valid)) (is (octets= a-hash missing)))
        (net::stash-block app a)
        (multiple-value-bind (blocks valid missing) (net::branch app b-hash)
          (is valid) (is (null missing)) (is= (list a b) blocks)
          (clempo.store:with-speculative-blocks (db)
            (net::execute-branch app blocks)
            (is= 2 (clempo.store:db-number db))
            (is (octets= b-hash (clempo.store:db-hash db)))))))
    (is= 0 (clempo.store:db-number db))
    (is (octets= root (clempo.store:db-root-hash db)))
    (is (octets= digest (clempo.store:db-hash db)))))

;;; The T13 devnet the Rust validators finalized (tests/tempo/executor/data/devnet-r6): its
;;; genesis and blocks 1..60 with their real consensus contexts. Block 59 is epoch 0's last
;;; block and carries the epoch-1 DKG outcome; blocks 32..44 carry signed dealer logs.

(defun devnet-file (name)
  (asdf:system-relative-pathname
   "clempo-validator" (concatenate 'string "../../tests/tempo/executor/data/devnet-r6/" name)))

(defun devnet-blocks ()
  (with-input-from-string (in (clempo.io:read-file-string (devnet-file "blocks-1-60.txt")))
    (coerce (loop for line = (read-line in nil) while line
                  for trimmed = (string-trim '(#\Space #\Return) line)
                  unless (string= trimmed "") collect (clempo.blocks:decode-block (hex->octets trimmed)))
            'vector)))

(defun make-temp-dir ()
  (uiop:ensure-directory-pathname (sb-posix:mkdtemp "/tmp/clempo-validator-test-XXXXXX")))

(defun call-with-devnet-observer (function)
  "Call FUNCTION with a validator app that follows the devnet under a key outside its
committee, in a fresh datadir, alone on an in-memory network."
  (let* ((chain (clempo.chainspec:load-chainspec (devnet-file "genesis.json")))
         (dir (make-temp-dir))
         (seed (make-array 32 :element-type 'octet :initial-element 9))
         (hub (make-hub))
         (network (make-instance 'memory-network :hub hub :key (clempo.ed25519:ed25519-public-key seed))))
    (push network (hub-nodes hub))
    (unwind-protect
         (net::call-with-validator-app function :chain chain :datadir dir :seed seed :network network)
      (uiop:delete-directory-tree dir :validate t))))

(defun block-context (block)
  (clempo.headers:header-consensus-context (clempo.blocks:block-header block)))

(defun finalization-of (block &key view parent)
  "A finalization of BLOCK (at its header's view unless VIEW). The engine's finality path
takes artifacts the voter has already verified, so the certificate bytes are not checked."
  (let ((context (block-context block)))
    (clempo.consensus:make-finalization
     :proposal (clempo.consensus:make-proposal
                :round (clempo.consensus:make-round
                        :epoch (clempo.headers:consensus-context-epoch context)
                        :view (or view (clempo.headers:consensus-context-view context)))
                :parent (or parent (clempo.headers:consensus-context-parent-view context))
                :payload (clempo.blocks:block-hash block))
     :certificate (clempo.consensus:make-vrf-signature :vote (make-octets 48) :seed (make-octets 48)))))

(defun reproposal-context (block view)
  "The simplex context of VIEW whose parent is BLOCK notarized in its own view."
  (let ((context (block-context block)))
    (clempo.simplex::make-context
     (clempo.consensus:make-round :epoch (clempo.headers:consensus-context-epoch context) :view view)
     0 (clempo.headers:consensus-context-proposer context)
     (cons (clempo.headers:consensus-context-view context) (clempo.blocks:block-hash block)))))

(deftest boundary-reproposal ()
  ;; commonware's marshal re-proposes the epoch's last block in later views of the epoch
  ;; (Rust validators and clempo's own sim do): verify and certify must accept it, and its
  ;; finalization in a later view than its header names must commit it and start the next
  ;; epoch instead of failing the proposal-metadata invariant.
  (call-with-devnet-observer
   (lambda (app)
     (let* ((blocks (devnet-blocks))
            (b57 (aref blocks 56)) (b58 (aref blocks 57)) (b59 (aref blocks 58))
            (view57 (clempo.headers:consensus-context-view (block-context b57)))
            (view59 (clempo.headers:consensus-context-view (block-context b59)))
            (hash59 (clempo.blocks:block-hash b59))
            (later (clempo.consensus:make-round :epoch 0 :view (1+ view59))))
       (is (net::boundary-p app 59))
       (loop for n from 1 to 59 do (is (net::stash-block app (aref blocks (1- n)))))
       (is (eq t (net::certify app later hash59)) "inferred re-proposal at certify")
       (is (null (net::certify app (clempo.consensus:make-round :epoch 0 :view (1+ view57))
                               (clempo.blocks:block-hash b57)))
           "a non-boundary block notarized in another view")
       (is (null (net::verify-proposal app (reproposal-context b57 (1+ view57)) (clempo.blocks:block-hash b57)))
           "re-proposal of a non-boundary block")
       (is (eq t (net::verify-proposal app (reproposal-context b59 (1+ view59)) hash59)))
       (is (octets= hash59 (net::propose app (reproposal-context b59 (1+ view59)))) "the leader re-proposes")
       (setf (net::validator-app-finalizations app) (list (finalization-of b58)))
       (net::drive-finality app)
       (is= 58 (net::head-number app))
       (is= 0 (net::epoch app))
       (setf (net::validator-app-finalizations app) (list (finalization-of b59 :view (+ view59 2) :parent view59)))
       (net::drive-finality app)
       (is= 59 (net::head-number app))
       (is (octets= hash59 (net::head-hash app)))
       (is= 1 (net::epoch app) "the boundary's outcome entered epoch 1")))))
