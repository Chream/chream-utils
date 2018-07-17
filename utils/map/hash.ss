;;; -*- Gerbil -*-
;;; © Chream

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

(def (alist->hash alist into: (ht (make-hash-table)))
  (for-each (match <>
              ([k . v] (hash-add! ht k v))
              ([] ht))
            alist))

(def (hash->alist ht)
  (let (alist [])
    (hash-for-each (lambda (k v)
                     (set! alist (cons [k . v] alist)))
                   ht)))

(def (plist->hash plist into: (ht (make-hash-table)))
  (let lp ((plist-1 plist))
    (match plist-1
      ([k v . rest]
       (hash-add! ht k v)
       (lp rest))
      ([] ht)
      (else (error "Malformed plist" plist-1)))))

(def (hash->plist ht)
  (let (plist [])
    (hash-for-each (lambda (k v)
                     (set! plist (cons* k v plist)))
                   ht)
    plist))
