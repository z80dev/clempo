(in-package #:clempo.executor)

;;; Historical states as execution backends: a clempo.history view ("the state after block
;;; N") answers the world's reads exactly as the state database did after that block, so
;;; BEGIN-CALL-BLOCK, APPLY-TRANSACTION on its context and EXECUTE-REQUEST run on any block
;;; the history covers without touching the store.

(defmethod state:backend-account ((view clempo.history:history-state) address)
  (clempo.history:history-account view address))

(defmethod state:backend-storage ((view clempo.history:history-state) address slot)
  (clempo.history:history-storage view address slot))

(defmethod state:backend-code ((view clempo.history:history-state) code-hash)
  (clempo.history:history-code view code-hash))

(defmethod state:backend-storage-empty-p ((view clempo.history:history-state) address)
  (clempo.history:history-storage-empty-p view address))
