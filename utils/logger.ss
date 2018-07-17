;;; -*- Gerbil -*-
;;; © Chream

(import :std/sugar
        :std/format
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

(export (except-out #t get-caller))

(def current-logger
  (make-parameter #f))

;; This will get the caller of the function ;; through a continuation object.

(def (get-caller cont)
  (let lp ((cont cont))
    (and cont
         (or (##continuation-creator cont)
             (lp (##continuation-next-frame cont #f))))))

;; -----

(def (make-logger path top: top name: name)
0123456
  (def (ensure-json-type obj)
    (cond
     ((or (struct-object? obj)
          (class-object? obj))
      (try
       (object->json obj)
       (catch (e)
         (fprintf (current-error-port)
                  "Could not obj to json: ~S" obj)
         (display-exception e))))
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
            (begin
              (json-add! entry caller: caller-name)
              (json-add! entry thread: thread)
              (json-add! entry type: cmd)
              (case cmd
                ((error warn trace debug info)
                 (match args
                   ([msg sym val]
                    (json-add! entry msg: msg)
                    (json-add! entry sym: sym)
                    (json-add! entry val: (ensure-json-type val)))
                   ([msg]
                    (json-add! entry msg: msg)))
                 (logger entry))
                ((state) logger)
                ((dir path) (subpath top path))
                (else (error "Uknown command for logger."))))
            (catch (e)
              (display-exception e)
              (json-put! entry type: 'log-error)
              (let (s (open-output-string))
                (display-exception e s)
                (json-put! entry exception: (get-output-string s))
                ;; send underlying hash-table.
                (logger entry))))))))))

(defrules log (current-logger)
  ((_ level msg sym)
   ((current-logger) level msg 'sym sym))
  ((_ level msg)
   ((current-logger) level msg))
  ((_ level msg sym logger: logger)
   (logger level msg 'sym sym))
  ((_ level msg logger: logger)
   (logger level msg)))
