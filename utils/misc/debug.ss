;;; -*- Gerbil -*-
;;;gerbil/chream-utils/utils/misc/debug.ss

(export #t)

(define-syntax logg
  (syntax-rules ()
    ((_ var)
     (begin
       (display (format "~S == " 'var))
       (prn var)))))
