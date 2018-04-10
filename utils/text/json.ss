;;; -*- Gerbil -*-
;;;gerbil/chream-utils/utils/text/json.ss

(import :std/srfi/1
        (only-in :std/pregexp pregexp-match)
        (only-in :std/text/json read-json json-symbolic-keys string->json-object json-object->string)
        (only-in :gerbil/gambit/threads spawn)
        (only-in :clan/utils/base if-let when-let)
        (only-in :clan/utils/json pretty-print-json)

        (only-in  "../misc/asserts" check-type)
        "../map/hash")

(export #t)

(defalias pp pretty-print-json)

(def (read-json-equal in)
  (parameterize ((json-symbolic-keys #f))
    (read-json in)))

(def (read-json-equal-file file)
  (call-with-input-file file
    (lambda (in)
      (read-json-equal in))))

(def (make-json) (make-hash-table))

(def (copy-json obj)
  (string->json-object (json-object->string obj)))

(def (json-input-fn-generator get-fn set-fn! force-set-fn! constructor-fn)
  (lambda (table . entry-spec)
    (let lp! ((table-1 table)
              (entry-spec-1 entry-spec))
      (cond ((null? entry-spec-1)
             (error "Invalid number of keys. key-length: : " (length entry-spec)))
            ((= 1 (length entry-spec-1))
             (set-fn! table-1 (car entry-spec-1) (constructor-fn)))
            ((= 2 (length entry-spec-1))
             (set-fn! table-1 (car entry-spec-1) (cadr entry-spec-1)))
            (else
             (let* ((key (car entry-spec-1))
                    (entry (get-fn table-1 key)))
               (cond ((hash-table? entry)
                      (lp! entry (cdr entry-spec-1)))
                     (entry
                      (let (ht-1 (make-json))
                        (set-fn! ht-1 "%%old-value" entry)
                        (force-set-fn! table-1 key ht-1)
                        (lp! ht-1 (cdr entry-spec-1))))
                     (else
                      (let (ht-2 (make-json))
                        (set-fn! table-1 key ht-2)
                        (lp! ht-2 (cdr entry-spec-1)))))))))))

(def json-add! (json-input-fn-generator hash-get hash-add! hash-put! make-json))
(def json-put! (json-input-fn-generator hash-get hash-put! hash-put! make-json))
;; add list version?

(def (json-delete! ht . entry-spec)
  (let lp! ((ht-1 ht)
            (entry-spec-1 entry-spec))
    (cond ((null? entry-spec-1)
           (error "Illegal number of arguments Must be odd. length: "
             (length entry-spec-1))
           ((= 1 (length entry-spec-1))
            (let* ((key (car entry-spec-1))
                   (present? (hash-get ht-1 (car entry-spec-1))))
              (if present?
                (begin
                  (hash-remove! ht-1 (car entry-spec-1))
                  (values present? #t))
                (values #f #f)))))
          (else
           (let* ((key (car entry-spec-1))
                  (entry (hash-get ht-1 key)))
             (if (hash-table? entry)
               (lp! entry (cdr entry-spec-1))
               (error "json-delete: key not present!" (car entry-spec-1))))))))

(def (json-get ht . entry-spec)
  (let lp ((ht-1 ht)
           (entry-spec-1 entry-spec))
    (cond ((null? entry-spec)
           (error "illegal arguments."))
          ((= 1 (length entry-spec-1))
           (hash-get ht-1 (car entry-spec-1)))
          (else
           (let* ((key (car entry-spec-1))
                  (entry (hash-get ht-1 (car entry-spec-1))))
             (if (hash-table? entry)
               (lp entry (cdr entry-spec-1))
               #f))))))

(def (json-append! ht . entry-spec)
  (let lp! ((ht-1 ht)
           (entry-spec-1 entry-spec))
    (let (len (length entry-spec-1))
      (cond ((null? entry-spec)
             (error "JSON-APPEND: illegal arguments." len))
            ((= 1 len)
             (error "JSON-APPEND: illegal arguments" len))
            ((= 2 len)
             (let* ((key (car entry-spec-1))
                    (new-list (cadr entry-spec-1))
                    (old-list (or (hash-get ht-1 key) []))
                    (app-list (append! new-list old-list)))
               (check-type list? old-list)
               (check-type list? new-list)
               (json-put! ht-1 key app-list)))
            (else
             (let* ((key (car entry-spec-1))
                    (entry (json-get ht-1 key)))
               (if (hash-table? entry)
                 (lp! entry (cdr entry-spec-1))
                 (error "json-append!: key not present!" (car entry-spec-1)))))))))

(def (json-merge! ht1 ht2)
  (hash-for-each
   (lambda (k v)
     (cond ((hash-table? v) (json-merge! (json-get ht1 k) v))
           ((list? v)       (json-put! ht1 k (delete-duplicates! (append (json-get ht1 k) v))))
           (else (json-put! ht1 k v))))
   ht2))

(def (json-make-ref! ht ref-cat val)

  (def (make-new-ref! refs val)
    (let (new-ref (number->string (fx1+ (hash-length refs))))
      (json-add! refs new-ref val)
      new-ref))

  (def (make-refs! ht ref-cat)
    (json-add! ht ref-cat (make-json)))

  (if-let (refs (json-get ht ref-cat))
          (begin
            (if-let (ref (call/cc
                           (lambda (ret)
                             (hash-for-each
                              (lambda (k v)
                                (when (equal? v val)
                                  (ret k)))
                              refs)
                             (ret #f))))
                    ref
                    (make-new-ref! refs val)))
          (begin
            (make-refs! ht ref-cat)
            (json-make-ref! ht ref-cat val))))

(def (json-ref-lookup ht key ref)
  (json-get ht key ref))

(def (json-refs-expand! ht obj)
  (call/cc
    (lambda (ret)
      (hash-for-each
       (lambda (k v)
         (when-let (ref-val (json-get ht k v))
                   (json-put! obj k ref-val)
                   (ret ref-val)))
       obj))))
