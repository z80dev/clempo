(in-package #:clempo.sim)

;;; The R6 scenarios: fixed seeds, three epochs (three reshares, the last installing epoch 3)
;;; each, with faults. Dealers deal in an epoch's first half and post their logs in the
;;; second, so epochs are long enough for a quorum of dealers to lead a block there.

(defscenario "smoke-4"
  :validators 4 :seed 1 :epochs 2 :epoch-length 12 :latency (5 30) :drop 0)

(defscenario "r6-4"
  :validators 4 :seed 4 :epochs 3 :epoch-length 24 :latency (5 60) :drop 1/50
  :faults ((:outage 3 :from 1500 :to 4000)))

(defscenario "r6-5"
  :validators 5 :seed 5 :epochs 3 :epoch-length 25 :latency (5 60) :drop 1/50
  :faults ((:outage 1 :from 1000 :to 3000)
           (:partition ((0 1) (2 3 4)) :from 5000 :to 6500)))

(defscenario "r6-7"
  :validators 7 :seed 7 :epochs 3 :epoch-length 35 :latency (5 80) :drop 1/50
  :faults ((:silent 6)
           (:outage 2 :from 3000 :to 6000)))
