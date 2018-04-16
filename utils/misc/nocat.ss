;;; -*- Gerbil -*-
;;; © Chream

(export #t)

(define-syntax dolist
  (syntax-rules ()
    ((_ (e lis) exprs ...)
     (for-each (lambda (e) exprs ...) lis))))
