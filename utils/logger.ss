;;;; gerbil-scheme/chream-utils/src/logger.ss

(import (only-in :std/format format)
        (only-in :clan/utils/date current-timestamp string<-timestamp)
        (only-in :gerbil/gambit/threads current-thread)
        (only-in :gerbil/gambit/continuations continuation-capture)
        (only-in :clan/utils/logger text-logger))

(export (except-out #t get-caller logtupe<-level>))

(def *log-level* (make-parameter 6))
(def (logtype<-level level)
  (case level
    ((1) 'ERROR)
    ((2) 'WARN)
    ((3) 'INFO)
    ((4) 'DEBUG)
    ((5) 'TRACE)
    ((6) 'TEST)))

;; This will get the caller of the function ;; through a continuation object.

(def (get-caller cont)
  (let lp ((cont cont))
    (and cont
         (or (##continuation-creator cont)
             (lp (##continuation-next-frame cont #f))))))

;; -----

(def (make-logger name: (name #f) level: level file: file)
  (let ((logger (text-logger name: name)))
    (lambda (msg . args)
      (continuation-capture
       (lambda (cont)
         (case msg
           ((log) (with ([msg sym obj] args)
                    (let* ((caller (get-caller cont))
                           (text
                            (format "~S || ~S || ~S ||caller: ~40S ||msg: ~40S ||obj-info: ~50S === ~50S ||~n"
                                    (string<-timestamp (current-timestamp))
                                    (current-thread)
                                    (logtype<-level level)
                                    (or (##procedure-friendly-name caller) '?)
                                    msg sym obj)))
                      (cond ((>= (*log-level*) level)
                             (display text)
                             (logger file text)
                             (void))
                            ((<= (*log-level*) level) (void))))))
           ((state) logger)))))))

(defrules log (stx)
  ((log log-proc msg)
   (log-proc 'log msg 'nil 'nil))
  ((log log-proc msg obj)
   (log-proc 'log msg 'obj obj))
  ((log log-proc msg obj)
   (log-proc 'log msg 'obj obj))
  ((log log-proc msg obj)
   (log-proc 'log msg 'obj obj)))


(def log-file "/var/log/funds/funds.log")
(def test (make-logger name: 'default-test-logger level: 6 file: log-file))
(def trace (make-logger name: 'default-trace-logger level: 5 file: log-file))
(def debug (make-logger name: 'default-debug-logger level: 4 file: log-file))
(def info (make-logger name: 'default-info-logger level: 3 file: log-file))
(def warn (make-logger name: 'default-warn-logger level: 2 file: log-file))
(def error (make-logger name: 'default-error-logger level: 1 file: log-file))
