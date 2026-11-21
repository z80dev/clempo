(in-package #:clempo.dev)

(defcommand "rpc" (args)
  "call one JSON-RPC method: rpc <method> [json-param ...] [-url U]"
  (destructuring-bind (method &rest params) (arg args :rest)
    (let ((result (clempo.jsonrpc:rpc-call
                   method (mapcar #'clempo.json:parse-json params)
                   :url (arg args :url (clempo.jsonrpc:local-rpc-url)))))
      (write-line (clempo.json:to-json-string result))
      0)))
