;;; -*- Gerbil -*-
;;;gerbil/chream-utils/utils/misc/debug.ss

(import (only-in :std/format format)
        (only-in :std/misc/repr prn))
(export #t)

(define-syntax logg
  (syntax-rules ()
    ((_ var)
     (begin
       (display (format "~S == " 'var))
       (prn var)))))
