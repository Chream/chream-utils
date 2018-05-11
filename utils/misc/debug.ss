;;; -*- Gerbil -*-
;;; © Chream

(import (only-in :std/format format)
        (only-in :std/misc/repr prn))
(export #t)

(defrules logg ()
  ((_ var)
   (begin
     (display (format "~S == " 'var))
     (prn var)))
  ((_ msg var)
   (begin
     (display (format "~S. ~S == " msg 'var))
     (prn var))))

(defrules let-debug ()
  ((_ ((id exp) rest) body ...)
   (let (id exp)
     (log debug "Let binding." id)
     (gen rest body ...)))

  ((_ ((id exp)) body ...)
   (let (id exp)
     (log debug "Let binding." id)
     body ...))

  ((_ (id exp) body ...)
   (let (id exp)
     (log debug "Let binding." id)
     body ...)))
