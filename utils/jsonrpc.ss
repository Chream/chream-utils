(import :gerbil/gambit/hash
        "text/json"
        "misc/repr"
        "map/hash")
(export #t)

;; jsonrpc interface.
(defclass (jsonrpc-request jsonable) (id method params))
(defclass (jsonrpc-response jsonable) (id result error))
(defclass (jsonrpc-notification jsonable) (method params))
(defclass (jsonrpc-error jsonable) (code msg data))

(def error-codes
  (let (table (make-hash-table))
    ;; Defined by JSON RPC
    (hash-add! table 'parse-error -32700)
    (hash-add! table 'InvalidRequest -32600)
    (hash-add! table 'MethodNotFound -32601)
    (hash-add! table 'InvalidParams -32602)
    (hash-add! table 'InternalError -32603)
    (hash-add! table 'serverErrorStart -32099)
    (hash-add! table 'serverErrorEnd -32000)
    (hash-add! table 'ServerNotInitialized -32002)
    (hash-add! table 'UnknownErrorCode -32001)
    table))

(defmethod {init! jsonrpc-error}
  (lambda (self datum msg (data #f))
    (jsonrpc-error-code-set! self (hash-get error-codes datum))
    (jsonrpc-error-msg-set! self msg)
    (jsonrpc-error-data-set! self data)))

(def (read-req port)
  "Read a jsonrpc request from char port.
   Return '`jsonrpc-request' object."
  (let ((json (read-json port)))
    (make-jsonrpc-request (json-get json 'id)
                          (json-get json 'method)
                          (json-get json 'params))))

(def (write-resp resp port)
  "Write a jsonrpc response to char port.
   Return positive integer of charactors written.
   It will also pad the message with the jsonrpc
   version."
  (let* ((slots-plist (cddr (object->list resp)))
         (slots-alist
          (map (match <>
                 ([k v] [k . v])
                 ([]    [jsonrpc: . "2.0"]) ; add version number
                 (v     (error "Invalid plist." slots-plist)))
               slots-plist)))
    (write-json slots-alist port)))
