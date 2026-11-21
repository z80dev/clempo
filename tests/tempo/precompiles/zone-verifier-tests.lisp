(defpackage #:clempo.zone-verifier.tests
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete))
(in-package #:clempo.zone-verifier.tests)

;;; Production fixture copied byte-for-byte (base64 decoded) from
;;; crates/nitro-attestation/testdata/aws_attestation_2026_01_03.b64. Provenance:
;;; https://github.com/base/nitro-validator/blob/main/test/hinted/HintedNitroAttestation.t.sol
;;; The Rust test freezes verification time at 1767472867.
(define-constant +production-nitro-document+ (hex->octets "8444a1013822a0591116bf696d6f64756c655f69647827692d30333638666136376531353664366432332d656e633031396238353936623161396461643666646967657374665348413338346974696d657374616d701b0000019b8597a84a6470637273b00058304b8d4cf2a99e05ce1b5bddaf9d21cb446eb0e606c5bebd1ebf02b473a22165f7b68b0bb0d1ac5a90f0311e493522cfab0158300343b056cd8485ca7890ddd833476d78460aed2aa161548e4e26bedf321726696257d623e8805f3f605946b3d8b0c6aa02583016efcc1d6952c5b9737eb2ab1751a08412835c5125818bfb55f6367cb3cdca49b13d2a5b6d771de9db1578242e328c6d035830000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000045830245617239e18ae8eb5150c2be45200db1e6b1e483d94c81d8386171eaad6cf641a1c90f1571f185426c82f77783feb370558300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000658300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000758300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000858300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000958300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a58300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000b58300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c58300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d58300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e58300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f58300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006b636572746966696361746559027f3082027b30820201a0030201020210019b8596b1a9dad60000000069597ebc300a06082a8648ce3d04030330818e310b30090603550406130255533113301106035504080c0a57617368696e67746f6e3110300e06035504070c0753656174746c65310f300d060355040a0c06416d617a6f6e310c300a060355040b0c034157533139303706035504030c30692d30333638666136376531353664366432332e75732d656173742d312e6177732e6e6974726f2d656e636c61766573301e170d3236303130333230343032355a170d3236303130333233343032385a308193310b30090603550406130255533113301106035504080c0a57617368696e67746f6e3110300e06035504070c0753656174746c65310f300d060355040a0c06416d617a6f6e310c300a060355040b0c03415753313e303c06035504030c35692d30333638666136376531353664366432332d656e63303139623835393662316139646164362e75732d656173742d312e6177733076301006072a8648ce3d020106052b81040022036200049a18f285d23894e60570cd496740cc599e404d35da9932a4ecabe754469e06fc613961134bd55a331689985b32fccdc30326dc8a975534cf19159fbae10ba45b9b139ad2d66be9b764cad1c09feeff06cd60d00542fc88a57344209347ca0db2a31d301b300c0603551d130101ff04023000300b0603551d0f0404030206c0300a06082a8648ce3d0403030368003065023100ac501536ab03a1323591db95411356802e39b2c7ba1cace7edf1731c2579791d32f6a77ee06f90123448b4daceb24a030230287b670040363a0d8a8974e0e25b41864b6d39ecb7d58646d9ff4a54069d141a68368a18ee100f51ffdf95cdefa2f7fd68636162756e646c65845902153082021130820196a003020102021100f93175681b90afe11d46ccb4e4e7f856300a06082a8648ce3d0403033049310b3009060355040613025553310f300d060355040a0c06416d617a6f6e310c300a060355040b0c03415753311b301906035504030c126177732e6e6974726f2d656e636c61766573301e170d3139313032383133323830355a170d3439313032383134323830355a3049310b3009060355040613025553310f300d060355040a0c06416d617a6f6e310c300a060355040b0c03415753311b301906035504030c126177732e6e6974726f2d656e636c617665733076301006072a8648ce3d020106052b8104002203620004fc0254eba608c1f36870e29ada90be46383292736e894bfff672d989444b5051e534a4b1f6dbe3c0bc581a32b7b176070ede12d69a3fea211b66e752cf7dd1dd095f6f1370f4170843d9dc100121e4cf63012809664487c9796284304dc53ff4a3423040300f0603551d130101ff040530030101ff301d0603551d0e041604149025b50dd90547e796c396fa729dcf99a9df4b96300e0603551d0f0101ff040403020186300a06082a8648ce3d0403030369003066023100a37f2f91a1c9bd5ee7b8627c1698d255038e1f0343f95b63a9628c3d39809545a11ebcbf2e3b55d8aeee71b4c3d6adf3023100a2f39b1605b27028a5dd4ba069b5016e65b4fbde8fe0061d6a53197f9cdaf5d943bc61fc2beb03cb6fee8d2302f3dff65902c3308202bf30820245a003020102021100a15c679215ffe44214474254fe6824b3300a06082a8648ce3d0403033049310b3009060355040613025553310f300d060355040a0c06416d617a6f6e310c300a060355040b0c03415753311b301906035504030c126177732e6e6974726f2d656e636c61766573301e170d3235313233313134303734365a170d3236303132303135303734355a3064310b3009060355040613025553310f300d060355040a0c06416d617a6f6e310c300a060355040b0c034157533136303406035504030c2d323437383732346630353936303464632e75732d656173742d312e6177732e6e6974726f2d656e636c617665733076301006072a8648ce3d020106052b8104002203620004346bd89947f666ed5181429e5d9e24390da23b262b251214971720850d65a9aa5654edf3495fb9f9e34d03cc5659cb489ff8f4e1072f92841c1b0b605a613d3cad85df9df25577afef7fbc50fed4476bdae8637a268dd68d86929060fafb0c7ca381d53081d230120603551d130101ff040830060101ff020102301f0603551d230418301680149025b50dd90547e796c396fa729dcf99a9df4b96301d0603551d0e0416041494ea745ff596b8c820fa4fdec1831be6bc5bf830300e0603551d0f0101ff040403020186306c0603551d1f046530633061a05fa05d865b687474703a2f2f6177732d6e6974726f2d656e636c617665732d63726c2e73332e616d617a6f6e6177732e636f6d2f63726c2f61623439363063632d376436332d343262642d396539662d3539333338636236376638342e63726c300a06082a8648ce3d040303036800306502306e869990c5ea0db8e742387d8ed50df9aaba9c737b0d6fd7e1d400437952574ae1070eab3e7343f798df1dd905d51b05023100c89ce9ec8ed957c1dcc759e89803622d58f88483ea645424569a13fb1cf1b58d0f044a27c07a32396efd02f9328dc45d59031a308203163082029ba003020102021100abeec5a4a7559840a64e6c688ceb55c8300a06082a8648ce3d0403033064310b3009060355040613025553310f300d060355040a0c06416d617a6f6e310c300a060355040b0c034157533136303406035504030c2d323437383732346630353936303464632e75732d656173742d312e6177732e6e6974726f2d656e636c61766573301e170d3236303130333037333233395a170d3236303130393031333233395a308189313c303a06035504030c33626533353839613566663262326463652e7a6f6e616c2e75732d656173742d312e6177732e6e6974726f2d656e636c61766573310c300a060355040b0c03415753310f300d060355040a0c06416d617a6f6e310b3009060355040613025553310b300906035504080c0257413110300e06035504070c0753656174746c653076301006072a8648ce3d020106052b81040022036200044b560b9f0208a5222b3fc5abee00b5be9e83253f20495b9f1adb043ed2b348f90bedad300eda014a6fc76e484a6b1224b7d3c8f0031a9391368b8dcf49e760938fb3e04c685c08421b889cfa7e179e214727c2ba25109e1fd2810af16e4df76ba381ea3081e730120603551d130101ff040830060101ff020101301f0603551d2304183016801494ea745ff596b8c820fa4fdec1831be6bc5bf830301d0603551d0e041604145af540fd400daf3db86e7258b2077e331ac66626300e0603551d0f0101ff0404030201863081800603551d1f047930773075a073a071866f687474703a2f2f63726c2d75732d656173742d312d6177732d6e6974726f2d656e636c617665732e73332e75732d656173742d312e616d617a6f6e6177732e636f6d2f63726c2f62303038346165392d373537312d343133392d623939392d3262353534303665313231332e63726c300a06082a8648ce3d04030303690030660231008c7894231a14895be8b74ed78167586ebdcffe4e65d33e20d567da6d5133246004c068d2d65c9eb48ac3985f8e84a4400231009ea356f3e222fc3cf19ea5f45093b1cad111a5cb529b27fe34ad49b604d6487fa9bb7d4f22ed8f419691c1bf41c705785902c2308202be30820245a003020102021500e0083b70b784c89706e956617541349d014e8fde300a06082a8648ce3d040303308189313c303a06035504030c33626533353839613566663262326463652e7a6f6e616c2e75732d656173742d312e6177732e6e6974726f2d656e636c61766573310c300a060355040b0c03415753310f300d060355040a0c06416d617a6f6e310b3009060355040613025553310b300906035504080c0257413110300e06035504070c0753656174746c65301e170d3236303130333131303832345a170d3236303130343131303832345a30818e310b30090603550406130255533113301106035504080c0a57617368696e67746f6e3110300e06035504070c0753656174746c65310f300d060355040a0c06416d617a6f6e310c300a060355040b0c034157533139303706035504030c30692d30333638666136376531353664366432332e75732d656173742d312e6177732e6e6974726f2d656e636c617665733076301006072a8648ce3d020106052b810400220362000439bce9f95132b30b05a1962d126d8b96544709bec9b0c8b88554a1fecc3ea6bb6f1e86147b2d156b4dfc346eaf187c011f4cb59547e14a4fe4fa024f2facdf6b0c3af54429bdd6fd0ca2658750ba6da3d21d28de7066c3836e73a2dd3a834765a366306430120603551d130101ff040830060101ff020100300e0603551d0f0101ff040403020204301d0603551d0e04160414794e1af74d507655dd85ae5173ddfbb014162334301f0603551d230418301680145af540fd400daf3db86e7258b2077e331ac66626300a06082a8648ce3d040303036700306402302d6d8e3a31c67c619096c5711de34b32aa1de02817a7e3f92e8b938caeeb377398af93af20e2a095d4c7cfe88b51e04802305ac36e6a4aad1794ba96091bc0c4166aef57ac692f37b25366b62669c01fe56b9a66f46e4f6dc35e9be71e0da01451e06a7075626c69635f6b6579f669757365725f646174615469553adc61d6e9fcdecbe1ea49bb2b52a60238e0656e6f6e6365f6ff5860ac89a470b3cba7036b414903f07f653dbeae0b0d3a08892b7a44e5022590004715ee2909024608c1ab69836e26b6755392c2cf2c027a41cd652092bd45559e9224e19ef317ce62833da278ff7fac6044959de7534aed1691d7f0fde195b9b086"))

;;; Independently encoded fixture following zone_verifier/attestation.rs::tests:
;;; private scalars [1;48], [2;48], [3;48], root/intermediate/leaf; P384/SHA384;
;;; validity [1700000000,1900000000]; intermediate pathLen 0; PCR bytes 10/11/12;
;;; timestamp 1800000000000, and mod.rs::batch_commitment_type_hash_is_stable's
;;; statement for chain 42431. Signatures use public test nonce k=1, not RFC6979.
;;; This exercises real cryptography, including a fully bound successful verify.
(define-constant +test-nitro-root+ (hex->octets "308201be30820143a003020102020101300a06082a8648ce3d0403033037310b3009060355040613025553310e300c060355040a0c0554656d706f3118301606035504030c0f54657374204e6974726f20526f6f74301e170d3233313131343232313332305a170d3330303331373137343634305a3037310b3009060355040613025553310e300c060355040a0c0554656d706f3118301606035504030c0f54657374204e6974726f20526f6f743076301006072a8648ce3d020106052b810400220362000443e3af2a0db9086750976877650f426d2157a45e10de646ff857198b226df0d4b2243408e03ba711d9c34c51cb344413dd12e3cea20d5112f06b0831d2ea139ba34061f8310e9744fd18d915ef34f6f2c670e34c63eeb80bcc613ecb91f2c196a3233021300f0603551d130101ff040530030101ff300e0603551d0f0101ff040403020186300a06082a8648ce3d0403030369003066023100aa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7023100a7fddec0e32b6a94a30f3f896ed551640c4332a6ed176043418a3e44dc5ec180a7b00ce23a908826a6c01b20b879523c"))
(define-constant +test-nitro-document+ (hex->octets "8444a1013822a0590689a9696d6f64756c655f69646b746573742d6d6f64756c6566646967657374665348413338346974696d657374616d701b000001a3185c50006470637273a30058301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010100158301111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110258301212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212126b63657274696669636174655901c6308201c230820148a003020102020103300a06082a8648ce3d040303303f310b3009060355040613025553310e300c060355040a0c0554656d706f3120301e06035504030c1754657374204e6974726f20496e7465726d656469617465301e170d3233313131343232313332305a170d3330303331373137343634305a3037310b3009060355040613025553310e300c060355040a0c0554656d706f3118301606035504030c0f54657374204e6974726f204c6561663076301006072a8648ce3d020106052b8104002203620004c41da4a2af8dccb4d454d2ee04ed9f7a160cd7d8e7304a711e390a3784c535d51ac0d4ac94de9746750e1147184c4a316995f590ae51e54cc423a6c0d63d41b67504038a8f804e67bb076d2d31a5264bc4035899bd3f8529746d5ece9db828c9a320301e300c0603551d130101ff04023000300e0603551d0f0101ff0404030206c0300a06082a8648ce3d0403030368003065023100aa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7023028b98b084cd7f68c3e55b5490e08fae4b65fdd3fdf011896bcf4c337adc1fae1543b89faf2c54f4835dfda35cdd916c368636162756e646c65825901c2308201be30820143a003020102020101300a06082a8648ce3d0403033037310b3009060355040613025553310e300c060355040a0c0554656d706f3118301606035504030c0f54657374204e6974726f20526f6f74301e170d3233313131343232313332305a170d3330303331373137343634305a3037310b3009060355040613025553310e300c060355040a0c0554656d706f3118301606035504030c0f54657374204e6974726f20526f6f743076301006072a8648ce3d020106052b810400220362000443e3af2a0db9086750976877650f426d2157a45e10de646ff857198b226df0d4b2243408e03ba711d9c34c51cb344413dd12e3cea20d5112f06b0831d2ea139ba34061f8310e9744fd18d915ef34f6f2c670e34c63eeb80bcc613ecb91f2c196a3233021300f0603551d130101ff040530030101ff300e0603551d0f0101ff040403020186300a06082a8648ce3d0403030369003066023100aa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7023100a7fddec0e32b6a94a30f3f896ed551640c4332a6ed176043418a3e44dc5ec180a7b00ce23a908826a6c01b20b879523c5901cc308201c83082014ea003020102020102300a06082a8648ce3d0403033037310b3009060355040613025553310e300c060355040a0c0554656d706f3118301606035504030c0f54657374204e6974726f20526f6f74301e170d3233313131343232313332305a170d3330303331373137343634305a303f310b3009060355040613025553310e300c060355040a0c0554656d706f3120301e06035504030c1754657374204e6974726f20496e7465726d6564696174653076301006072a8648ce3d020106052b8104002203620004316140c268c8841cddd1dcbb51a11d516d285cdda6979f1db9230b9a9436f07ea3bacb8f4200e382634338484d19cdf494a1a457df42b3e7e22a255300495305b5d6f0208f2aada1741af1a9baaa73c39db971ef06d180d9524e1a2a4d33dfa0a326302430120603551d130101ff040830060101ff020100300e0603551d0f0101ff040403020186300a06082a8648ce3d0403030368003065023100aa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab702304738f2c7f543cdb2c57502afd1a03428811bbc1ebddc0208b5bf2291fac90ece3d86e674559632edf24546a572572bfa6a7075626c69635f6b6579f669757365725f6461746158201a703e80dd395e4720d1c88c877ed9f7d77c03052a985133b25cbf3e2b745b9d656e6f6e6365f65860aa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7ddc3fd7429a50bbcc56f96cfd80c869f8bf378e07ff752397e90983b0c1d4f7a984f54a9b8ff2fda33f2da1733023158"))

(defun verifier-args (&optional (proof +test-nitro-document+) (mode 1))
  (list 12 9 10 11 13 14 '(1 2) '(3 4 5 6) '(7 8) 9 (octets-of mode) proof))

(defun fixture-pcrs ()
  (loop for value from 16 to 18
        collect (make-array 48 :element-type '(unsigned-byte 8) :initial-element value)))

(defun verify-fixture (args &key (timestamp 1800000000) (chain 42431)
                                (pcrs (fixture-pcrs)) (root +test-nitro-root+)
                                (portal (clempo.zone-factory:portal-address (first args))))
  (with-spec (:t13)
    (with-storage ((make-hashmap-storage :block (make-block-info :timestamp timestamp :chain-id chain)))
      (clempo.zone-verifier:verify-with-policy portal args root pcrs))))

(defun nitro-invalid-p (function)
  (handler-case (progn (funcall function) nil)
    (clempo.zone-verifier::nitro-validation-error () t)))

(defun test-cbor-bytes (bytes)
  (let ((out (make-octets (+ (length bytes)
                           (clempo.zone-verifier::nitro-cbor-bytes-header-length (length bytes))))))
    (clempo.zone-verifier::nitro-put-cbor-bytes out 0 bytes)
    out))

(defun test-cose-document (payload &optional (signature (make-octets 96)))
  (octets-concat (hex->octets "8444a1013822a0") (test-cbor-bytes payload) (test-cbor-bytes signature)))

(deftest zone-verifier-layout-activation-and-selector ()
  (is (null (layout-fields 'clempo.zone-verifier:zone-verifier)))
  (with-spec (:t12) (is (null (precompile-contract +zone-verifier-address+))))
  (with-spec (:t13)
    (is (eq 'clempo.zone-verifier:zone-verifier (precompile-contract +zone-verifier-address+))))
  (is= #xebb2ddc9
       (clempo.abi:abi-selector "verify(uint32,uint64,uint64,bytes32,uint64,uint256,(bytes32,bytes32),(bytes32,bytes32,uint64,uint64),(uint64,uint64),bytes32,bytes,bytes)")))

(deftest zone-verifier-commitment ()
  (is= #x1a703e80dd395e4720d1c88c877ed9f7d77c03052a985133b25cbf3e2b745b9d
       (octets->integer (apply #'clempo.zone-verifier:batch-commitment 42431 (verifier-args)))))

(deftest zone-verifier-proofless-and-domain ()
  (dolist (mode '(0 1 2 3))
    (is (eq (= mode 2) (verify-fixture (verifier-args (make-octets 0) mode)))))
  (is (not (verify-fixture (verifier-args (make-octets 0) 2) :portal 42)))
  (is (not (verify-fixture (verifier-args +test-nitro-document+ 2))))
  (dolist (config (list (make-octets 0) (octets-of 2 0)))
    (let ((args (verifier-args (make-octets 0) 2)))
      (setf (nth 10 args) config)
      (is (not (verify-fixture args))))))

(deftest zone-verifier-bound-attestation ()
  (is (verify-fixture (verifier-args)))
  (is (not (verify-fixture (verifier-args) :chain 1)))
  (is (not (verify-fixture (verifier-args) :portal 42)))
  ;; Every EIP-712 field is bound, including uint256 heights above u64.
  (dolist (index '(0 1 2 3 4 5 9))
    (let ((args (verifier-args)))
      (incf (nth index args))
      (is (not (verify-fixture args)))))
  (dolist (height (list (+ 14 (ash 1 64)) (1- (ash 1 256))))
    (let ((args (verifier-args)))
      (setf (nth 5 args) height)
      (is (not (verify-fixture args)))))
  (loop for (index count) in '((6 2) (7 4) (8 2))
        do (dotimes (field count)
             (let ((args (copy-tree (verifier-args))))
               (incf (nth field (nth index args)))
               (is (not (verify-fixture args))))))
  (let ((pcrs (fixture-pcrs)))
    (setf (aref (first pcrs) 0) 99)
    (is (not (verify-fixture (verifier-args) :pcrs pcrs))))
  (is (not (verify-fixture (verifier-args) :pcrs nil)))
  ;; Exactly five minutes of future skew is allowed; old attestations have no age floor.
  (is (verify-fixture (verifier-args) :timestamp 1799999700))
  (is (not (verify-fixture (verifier-args) :timestamp 1799999699)))
  (is (verify-fixture (verifier-args) :timestamp 1800001000))
  (is (not (verify-fixture (verifier-args) :timestamp 1900000001)))
  (is (not (verify-fixture (verifier-args) :timestamp 1699999999))))

(deftest zone-verifier-production-nitro ()
  (is= #x641a0321a3e244efe456463195d606317ed7cdcc3c1756e09893f3c68f79bb5b
       (octets->integer (clempo.sha2:sha256 clempo.zone-verifier::+aws-nitro-root-der+)))
  (with-spec (:t13)
    (with-storage ((make-hashmap-storage))
      (let ((attestation (clempo.zone-verifier::verify-attestation-with-root
                          +production-nitro-document+ 1767472867 clempo.zone-verifier::+aws-nitro-root-der+)))
        (is attestation)
        (is= 1767472867402 (clempo.zone-verifier::nitro-attestation-timestamp attestation))
        (is= 16 (count-if #'identity (clempo.zone-verifier::nitro-attestation-pcrs attestation)))
        (is= 790000 (gas-used))))))

(deftest zone-verifier-signatures ()
  (let ((document (copy-octets +test-nitro-document+)))
    (setf (aref document (1- (length document)))
          (logxor 1 (aref document (1- (length document)))))
    (is (not (verify-fixture (verifier-args document)))))
  ;; Both s and n-s are valid ES384 signatures.
  (let* ((document (copy-octets +test-nitro-document+))
         (offset (- (length document) 48))
         (s (octets->integer document :start offset))
         (order #xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973))
    (replace document (integer->octets (- order s) 48) :start1 offset)
    (is (verify-fixture (verifier-args document))))
  (let* ((parsed (clempo.zone-verifier::parse-nitro-attestation +test-nitro-document+))
         (intermediate (second (clempo.zone-verifier::nitro-attestation-cabundle parsed))))
    (setf (aref intermediate (1- (length intermediate)))
          (logxor 1 (aref intermediate (1- (length intermediate)))))
    (is (nitro-invalid-p (lambda ()
                          (clempo.zone-verifier::nitro-verify-parsed parsed 1800000000 +test-nitro-root+)))))
  ;; A pinned root's self-signature is deliberately not verified.
  (let* ((parsed (clempo.zone-verifier::parse-nitro-attestation +test-nitro-document+))
         (root (first (clempo.zone-verifier::nitro-attestation-cabundle parsed))))
    (setf (aref root (1- (length root))) (logxor 1 (aref root (1- (length root)))))
    (is (clempo.zone-verifier::nitro-validate-chain parsed 1800000000 root))))

(deftest zone-verifier-cbor-validation ()
  (dolist (document (list (make-octets 0) (octets-of #xff)
                         (octets-concat (octets-of #xd3) +test-nitro-document+)
                         (octets-concat +test-nitro-document+ (octets-of 0))))
    (is (nitro-invalid-p (lambda () (clempo.zone-verifier::parse-nitro-attestation document)))))
  (let* ((parsed (clempo.zone-verifier::parse-nitro-attestation +test-nitro-document+))
         (original (clempo.zone-verifier::nitro-attestation-payload parsed)))
    ;; Duplicate recognized field and unknown field must fail before signature work.
    (dolist (extra (list (hex->octets "656e6f6e6365f6") (hex->octets "67756e6b6e6f776ef6")))
      (let ((payload (octets-concat original extra)))
        (setf (aref payload 0) #xaa)
        (is (nitro-invalid-p (lambda ()
                              (clempo.zone-verifier::parse-nitro-attestation (test-cose-document payload)))))))
    (let ((payload (copy-octets original)))
      ;; Payload tags and chunked strings are not accepted by the borrowed CBOR profile.
      (is (nitro-invalid-p (lambda ()
                            (clempo.zone-verifier::parse-nitro-attestation
                             (test-cose-document (octets-concat (octets-of #xc0) payload))))))))
  (let ((document (copy-octets +test-nitro-document+)))
    (setf (aref document 0) #x9f)
    (is (nitro-invalid-p (lambda () (clempo.zone-verifier::parse-nitro-attestation document)))))
  ;; Optional tag 18 and indefinite empty unprotected maps remain valid.
  (is (clempo.zone-verifier::parse-nitro-attestation
       (octets-concat (octets-of #xd2) +test-nitro-document+)))
  (is (clempo.zone-verifier::parse-nitro-attestation
       (octets-concat (subseq +test-nitro-document+ 0 6) (octets-of #xbf #xff)
                      (subseq +test-nitro-document+ 7)))))

(deftest zone-verifier-der-validation ()
  (dolist (der (list (make-octets 0) (octets-of #x30 #x80 0 0)
                    (octets-concat +test-nitro-root+ (octets-of 0))))
    (is (nitro-invalid-p (lambda () (clempo.zone-verifier::nitro-check-certificate der 1800000000 t)))))
  (let ((root (copy-octets +test-nitro-root+)))
    ;; v2 is well-formed DER but outside the Nitro profile.
    (let ((offset (search (hex->octets "a003020102") root)))
      (is offset)
      (setf (aref root (+ offset 4)) 1))
    (is (nitro-invalid-p (lambda () (clempo.zone-verifier::nitro-check-certificate root 1800000000 t)))))
  (let ((root (copy-octets +test-nitro-root+)))
    (let ((offset (search (hex->octets "06082a8648ce3d040303") root)))
      (is offset)
      (setf (aref root (+ offset 9)) 2))
    (is (nitro-invalid-p (lambda () (clempo.zone-verifier::nitro-check-certificate root 1800000000 t)))))
  ;; Correct extensions differ for a CA and the leaf.
  (is (nitro-invalid-p (lambda ()
                        (clempo.zone-verifier::nitro-check-certificate +test-nitro-root+ 1800000000 nil))))
  (is (nitro-invalid-p (lambda ()
                        (clempo.zone-verifier::nitro-check-certificate +test-nitro-root+ 1900000001 t)))))

(deftest zone-verifier-metering-and-dispatch ()
  (with-spec (:t13)
    (with-storage ((make-hashmap-storage))
      (is (null (clempo.zone-verifier::verify-attestation-with-root
                 (make-octets 24577) 1800000000 +test-nitro-root+)))
      (is= 0 (gas-used)))
    (with-storage ((make-hashmap-storage))
      (is (null (clempo.zone-verifier::verify-attestation-with-root
                 (octets-of 0) 1800000000 +test-nitro-root+)))
      (is= 40000 (gas-used)))
    (with-storage ((make-hashmap-storage))
      (is (null (clempo.zone-verifier::verify-attestation-with-root
                 +test-nitro-document+ 1800000000 (octets-of 0))))
      (is= 490000 (gas-used)))
    (dolist (limit '(39999 489999))
      (with-storage ((make-metered-storage (make-memory-host) :gas-limit limit))
        (is (handler-case
                (progn (clempo.zone-verifier::verify-attestation-with-root
                        +test-nitro-document+ 1800000000 +test-nitro-root+) nil)
              (precompile-halt (e) (eq (precompile-halt-reason e) :out-of-gas))))))
    (let ((input (make-octets 25221)))
      (replace input (hex->octets "ebb2ddc9"))
      (with-storage ((make-hashmap-storage :static t))
        (multiple-value-bind (status output gas) (call-precompile +zone-verifier-address+ input 0)
          (is (eq status :success)) (is= 0 (octets->integer output))
          (is= (* 30 (ceiling (length input) 32)) gas))))))

(deftest zone-verifier-pcr-policy ()
  (with-spec (:t12) (is (null (clempo.zone-verifier:approved-pcrs))))
  (let ((t13 (with-spec (:t13) (clempo.zone-verifier:approved-pcrs)))
        (t14 (with-spec (:t14) (clempo.zone-verifier:approved-pcrs))))
    (is (not (octets= (first t13) (first t14))))
    (is (octets= (second t13) (second t14)))
    (let* ((measurements (format nil "~{~A~^,~}" (mapcar #'octets->hex t13)))
           (policy (clempo.zone-verifier:parse-pcr-policy (concatenate 'string "T13=" measurements))))
      (is (equalp t13 (clempo.zone-verifier::pcr-policy-at policy :t13)))
      (is (null (clempo.zone-verifier::pcr-policy-at policy :t12)))
      (is (handler-case (progn (clempo.zone-verifier::validate-pcr-policy policy 4217) nil)
            (clempo.zone-verifier::pcr-policy-error () t)))
      (is (handler-case
              (progn (clempo.zone-verifier:parse-pcr-policy
                      (concatenate 'string "T13=" measurements ";T13=" measurements)) nil)
            (clempo.zone-verifier::pcr-policy-error () t)))))
  (dolist (values (list '("00" "00" "00")
                       (list (make-string 96 :initial-element #\0)
                             (make-string 96 :initial-element #\1)
                             (make-string 96 :initial-element #\2))))
    (is (handler-case (progn (clempo.zone-verifier:parse-pcrs values) nil)
          (clempo.zone-verifier::pcr-policy-error () t)))))
