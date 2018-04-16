;;; -*- Gerbil -*-
;;; © Chream

(import :std/format)
(export #t)

(def (check-type specifier? obj)
  (unless (specifier? obj)
    (error (format "Object ~S does not fulfill ~S"
                   obj specifier?))))
