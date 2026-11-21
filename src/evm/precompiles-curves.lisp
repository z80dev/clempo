(in-package #:clempo.evm)

;;; Pairing-curve precompiles: BN254 (EIP-196/197, Istanbul prices from EIP-1108), the
;;; KZG point evaluation (EIP-4844) and BLS12-381 (EIP-2537). The curve code validates
;;; encodings and subgroups; gas and the length rules are here.

(defun one-word (truth) (integer->octets (if truth 1 0) 32))

(defprecompile pc-bn254-add #x06 () (input)
  150
  (clempo.bn254:bn254-add input))

(defprecompile pc-bn254-mul #x07 () (input)
  6000
  (clempo.bn254:bn254-mul input))

(defprecompile pc-bn254-pairing #x08 () (input)
  (+ 45000 (* 34000 (floor (length input) 192)))
  (when (zerop (mod (length input) 192))
    (multiple-value-bind (ok valid) (clempo.bn254:bn254-pairing input)
      (and valid (one-word ok)))))

(defprecompile pc-point-evaluation #x0a (:since :cancun) (input)
  50000
  (and (= (length input) 192)
       (clempo.kzg:point-evaluation input)))

(define-constant +g1-msm-discounts+
  (coerce '(1000 949 848 797 764 750 738 728 719 712 705 698 692 687 682 677 673 669 665
            661 658 654 651 648 645 642 640 637 635 632 630 627 625 623 621 619 617 615
            613 611 609 608 606 604 603 601 599 598 596 595 593 592 591 589 588 586 585
            584 582 581 580 579 577 576 575 574 573 572 570 569 568 567 566 565 564 563
            562 561 560 559 558 557 556 555 554 553 552 551 550 549 548 547 547 546 545
            544 543 542 541 540 540 539 538 537 536 536 535 534 533 532 532 531 530 529
            528 528 527 526 525 525 524 523 522 522 521 520 520 519)
          'simple-vector))

(define-constant +g2-msm-discounts+
  (coerce '(1000 1000 923 884 855 832 812 796 782 770 759 749 740 732 724 717 711 704
            699 693 688 683 679 674 670 666 663 659 655 652 649 646 643 640 637 634 632
            629 627 624 622 620 618 615 613 611 609 607 606 604 602 600 598 597 595 593
            592 590 589 587 586 584 583 582 580 579 578 576 575 574 573 571 570 569 568
            567 566 565 563 562 561 560 559 558 557 556 555 554 553 552 552 551 550 549
            548 547 546 545 545 544 543 542 541 541 540 539 538 537 537 536 535 535 534
            533 532 532 531 530 530 529 528 528 527 526 526 525 524 524)
          'simple-vector))

(defun msm-gas (input pair-length discounts multiplication-cost)
  "EIP-2537 MSM price: k * discount(k) * cost / 1000; 0 for a malformed length (the
precompile then fails and takes all the gas anyway)."
  (let ((len (length input)))
    (if (or (zerop len) (plusp (mod len pair-length)))
        0
        (let ((k (floor len pair-length)))
          (floor (* k (svref discounts (min (1- k) (1- (length discounts)))) multiplication-cost)
                 1000)))))

(defmacro bls-result (form)
  "A BLS call returning (values output valid-p): the output, or NIL when invalid."
  (let ((out (gensym)) (valid (gensym)))
    `(multiple-value-bind (,out ,valid) ,form
       (and ,valid ,out))))

(defprecompile pc-bls12-g1-add #x0b (:since :prague) (input)
  375
  (bls-result (clempo.bls12-381:g1-add-precompile input)))

(defprecompile pc-bls12-g1-msm #x0c (:since :prague) (input)
  (msm-gas input 160 +g1-msm-discounts+ 12000)
  (and (plusp (length input)) (zerop (mod (length input) 160))
       (bls-result (clempo.bls12-381:g1-msm-precompile input))))

(defprecompile pc-bls12-g2-add #x0d (:since :prague) (input)
  600
  (bls-result (clempo.bls12-381:g2-add-precompile input)))

(defprecompile pc-bls12-g2-msm #x0e (:since :prague) (input)
  (msm-gas input 288 +g2-msm-discounts+ 22500)
  (and (plusp (length input)) (zerop (mod (length input) 288))
       (bls-result (clempo.bls12-381:g2-msm-precompile input))))

(defprecompile pc-bls12-pairing #x0f (:since :prague) (input)
  (+ 37700 (* 32600 (floor (length input) 384)))
  (and (plusp (length input)) (zerop (mod (length input) 384))
       (bls-result (clempo.bls12-381:pairing-precompile input))))

(defprecompile pc-bls12-map-fp-to-g1 #x10 (:since :prague) (input)
  5500
  (bls-result (clempo.bls12-381:map-g1-precompile input)))

(defprecompile pc-bls12-map-fp2-to-g2 #x11 (:since :prague) (input)
  23800
  (bls-result (clempo.bls12-381:map-g2-precompile input)))
