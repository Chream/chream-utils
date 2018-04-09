;;; -*- Gerbil -*-
;;;gerbil/chream-utils/utils/map/hash.ss

(import :std/format)
(export #t)

(def (hash-add! ht key value)
  (let ((present? (hash-get ht key)))
    (if present?
      (error (format "In hash-add! Hash key allready present key: ~S\n Old-val: ~S\nNew-val: ~S" key present? value))
      (hash-put! ht key value))))

(def (hash-table-length-> ht max-length)
  (call/cc
    (lambda (ret)
      (let (length 0)
        (hash-for-each (lambda (k v)
                         (when (> length max-length)
                           (ret #f)))
                       ht)
        (ret #t)))))

(def (hash-table-length-< ht max-length)
  (call/cc
    (lambda (ret)
      (let (length 0)
        (hash-for-each (lambda (k v)
                         (set! length (fx1+ length))
                         (when (> length max-length)
                           (ret #f)))
                       ht)
        (ret #t)))))

(def (hash-empty? ht)
  (hash-table-length-< ht 0))

(def (hash-single? ht)
  (hash-table-length-< ht 1))

(def (hash-first-value ht)
  (call/cc
    (lambda (ret)
      (hash-for-each (lambda (k v)
                       (ret v))
                     ht)
      (ret #f))))
