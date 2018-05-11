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
        "text/json")

(export (except-out #t get-caller logtupe<-level>))

;; This will get the caller of the function ;; through a continuation object.

(def (get-caller cont)
  (let lp ((cont cont))
    (and cont
         (or (##continuation-creator cont)
             (lp (##continuation-next-frame cont #f))))))

;; -----

(defstruct log-entry (thread type msg sym val))

(def (make-logger path top: top name: name)
  (let ((logger (json-logger path top: top name: name)))
    (lambda (cmd . args)
      (continuation-capture
       (lambda (cont)
         (case cmd
           ((error warn trace debug info)
            (try
             (with ([msg sym val] args)
               (let* ((caller (get-caller cont))
                      (caller-name (or (##procedure-friendly-name caller) '?))
                      (thread (thread-name (current-thread)))
                      (info-obj (make-log-entry thread cmd msg sym val))
                      (info-json (object->json-object info-obj)))
                 ;; have to remove the object type json entry.
                 (displayln (json-get info-json __struct:))
                 (logger info-json)))
             (catch (e)
               (display-exception e))))
           ((state) logger)
           ((dir path) (subpath top path))
           (else (error "Uknown command for logger."))))))))

(defrules log-info ()
  ((_ logger msg sym)
   (logger 'info msg 'sym sym)))
(defrules log-debug ()
  ((_ logger msg sym)
   (logger 'debug msg 'sym sym)))
(defrules log-trace ()
  ((_ logger msg sym)
   (logger 'trace msg 'sym sym)))
(defrules log-warn ()
  ((_ logger msg sym)
   (logger 'warn msg 'sym sym)))
(defrules log-error ()
  ((_ logger msg sym)
   (logger 'error msg 'sym sym)))
