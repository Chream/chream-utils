;;; -*- Gerbil -*-
;;; © Chream

(import :std/sugar
        (only-in :gerbil/gambit display-exception thread-name)
        (only-in :std/format format)
        (only-in :clan/utils/date current-timestamp string<-timestamp)
        (only-in :clan/utils/filesystem subpath)
        (only-in :gerbil/gambit/threads current-thread)
        (only-in :gerbil/gambit/continuations continuation-capture)
        (only-in :clan/utils/logger json-logger)
        "text/json"
        "misc/debug"
        "misc/repr")

(export (except-out #t get-caller logtupe<-level>))

;; This will get the caller of the function ;; through a continuation object.

(def (get-caller cont)
  (let lp ((cont cont))
    (and cont
         (or (##continuation-creator cont)
             (lp (##continuation-next-frame cont #f))))))

;; -----

(def (make-logger path top: top name: name)

  (def (ensure-json-type obj)
    (cond
      ((struct-object? obj)
       (object->json-object obj))
      ((class-object? obj)
       (object->json-object obj))
      (#t obj)))

  (let ((logger (json-logger path top: top name: name)))
    (lambda (cmd . args)
      (continuation-capture
       (lambda (cont)
         (let* ((caller (get-caller cont))
                (caller-name (or (##procedure-friendly-name caller) '?))
                (thread (thread-name (current-thread)))
                (entry (make-json)))
           (try
            (json-add! entry caller: caller-name)
            (json-add! entry thread: thread)
            (json-add! entry type: cmd)
            (case cmd
              ((error warn trace debug info)
               (match args
                 ([msg sym val]
                  (json-add! entry msg: msg)
                  (json-add! entry sym: sym)
                  (json-add! entry val: val))
                 ([msg]
                  (json-add! entry msg: msg)))
               (logger entry))
              ((state) logger)
              ((dir path) (subpath top path))
              (else (error "Uknown command for logger.")))
            (catch (e)
              (display-exception e)
              json-put! (entry type: 'log-error)
              (json-put! entry exception: (ensure-json-type e))
              (logger entry)))))))))

(defrules log-info ()
  ((_ logger msg sym)
   (logger 'info msg 'sym sym))
  ((_ logger msg)
   (logger 'info msg)))
(defrules log-debug ()
  ((_ logger msg sym)
   (logger 'debug msg 'sym sym))
  ((_ logger msg)
   (logger 'debug msg)))
(defrules log-trace ()
  ((_ logger msg sym)
   (logger 'trace msg 'sym sym))
  ((_ logger msg)
   (logger 'trace msg)))
(defrules log-warn ()
  ((_ logger msg sym)
   (logger 'warn msg 'sym sym))
  ((_ logger msg)
   (logger 'warn msg)))
(defrules log-error ()
  ((_ logger msg sym)
   (logger 'error msg 'sym sym))
  ((_ logger msg)
   (logger 'error msg)))
