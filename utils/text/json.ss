;;; -*- Gerbil -*-
;;; © Chream

(import :std/srfi/1
        :std/actor
        :std/sugar
        :clan/utils/subprocess
        (only-in :gerbil/gambit display-exception)
        (only-in :std/misc/ports read-all-as-string)
        (only-in :std/text/json json-symbolic-keys)
        (only-in :std/pregexp pregexp-match)
        (only-in :std/misc/list plist->alist)
        (only-in :std/misc/rtd type-name)
        (only-in  "../misc/asserts" check-type)
        "../map/hash"
        "../misc/repr"
        "../misc/asserts"
        "../misc/debug")

(export #t (import: :std/text/json))

(defstruct json (e) constructor: init!)

(defmethod {init! json}
  (lambda (self (ht (if (json-symbolic-keys)
                 (make-hash-table-eq)
                 (make-hash-table))))
    (json-e-set! self ht)))

(defmethod {:json json}
  (lambda (self)
    (json-e self)))

;; read and write

(def (read-json (port (current-output-port)))
  (let (ht (std/text/json#read-json port))
    (make-json ht)))

(def (read-json-equal in)
  (parameterize ((json-symbolic-keys #f))
    (let (r (read-json in))
      (cond ((eof-object? r)
             (make-json))
            (else
             r)))))

(def (read-json-equal-file file)
  (call-with-input-file file
    (lambda (in)
      (read-json-equal in))))

(def (write-json obj (port (current-output-port)))
  (match obj
    ((json e)
     (std/text/json#write-json e port))))

;; conversions

(def (json<- obj)
  (match obj
    (object? (object->json obj))
    (string? (string->json obj))
    (else (error "Cant convert to json: " obj))))

(def (object->json obj)
  (check-type object? obj)
  (let (json (make-json))
    (with ([type: type slots ...] (object->list obj))
      (when (struct-object? obj)
        (json-add! json __struct: (type-name type)))
      (when (class-object? obj)
        (json-add! json __class: (type-name type)))
      (for-each
        (match <>
          ([k . v]
           (match v
             (hash-table? (json-add! json k v))
             (object?     (json-add! json k
                                     (object->json v)))
            (else (json-add! json k v)))))
        (plist->alist slots)))
    json))

;; Mixin for a trivial method that just lists all slots
(defclass jsonable ())
(defmethod {:json jsonable} json<-)

(def (string->json str)
  (make-json (std/text/json#read-json (open-input-string str))))

(def (json->string obj)
  (match obj
    ((json e)
     (let ((port (open-output-string)))
       (std/text/json#write-json e port)
       (get-output-string port)))))

;; Map

(def (json-input-fn-generator get-fn set-fn! force-set-fn! constructor-fn)
  (lambda (obj . entry-spec)
    (with ((json e) obj)
      (let lp! ((table-1 e)
                (entry-spec-1 entry-spec))
        (cond ((null? entry-spec-1)
               (error "Invalid number of keys. key-length: : " (length entry-spec)))
              ((= 1 (length entry-spec-1))
               (set-fn! table-1 (car entry-spec-1) (constructor-fn))
               obj)
              ((= 2 (length entry-spec-1))
               (set-fn! table-1 (car entry-spec-1) (cadr entry-spec-1))
               obj)
              (else
               (let* ((key (car entry-spec-1))
                      (entry (get-fn table-1 key)))
                 (cond ((hash-table? entry)
                        (lp! entry (cdr entry-spec-1)))
                       (entry
                        (let (ht-1 (make-json))
                          (set-fn! ht-1 "__old-value" entry)
                          (force-set-fn! table-1 key ht-1)
                          (lp! ht-1 (cdr entry-spec-1))))
                       (else
                        (let (ht-2 (make-json))
                          (set-fn! table-1 key ht-2)
                          (lp! ht-2 (cdr entry-spec-1))))))))))))

(def json-add! (json-input-fn-generator hash-get hash-add! hash-put! make-json))
(def json-put! (json-input-fn-generator hash-get hash-put! hash-put! make-json))
;; add list version?

(def (json-delete! obj . entry-spec)
  (with ((json e) obj)
    (let lp! ((ht-1 e)
              (entry-spec-1 entry-spec))
      (cond ((null? entry-spec-1)
             (error "Illegal number of arguments Must be odd. length: "
               (length entry-spec-1)))
            ((= 1 (length entry-spec-1))
             (let* ((key (car entry-spec-1))
                    (present? (hash-get ht-1 key)))
               (if present?
                 (begin
                   (hash-remove! ht-1 key)
                   (values present? #t))
                 (values #f #f))))
            (else
             (let* ((key (car entry-spec-1))
                    (entry (hash-get ht-1 key)))
               (if (hash-table? entry)
                 (lp! entry (cdr entry-spec-1))
                 (error "json-delete: key not present!" (car entry-spec-1)))))))))

(def (json-get obj . entry-spec)
  (with ((json e) obj)
    (let lp ((ht-1 e)
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
                 #f)))))))

(def (json-append! obj . entry-spec)
  (with ((json e) obj)
    (let lp! ((ht-1 e)
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
                   (error "json-append!: key not present!" (car entry-spec-1))))))))))

(def (json-merge! obj1 obj2)
  (let ((e1 (json-e obj1))
        (e2 (json-e obj2)))
    (hash-for-each
     (lambda (k v)
       (cond ((hash-table? v)
              (unless (hash-table? (json-get e1 k))
                (hash-put! e1 k (make-json)))
              (json-merge! (json-get e1 k) v))
             ((list? v)
              (unless (list? (json-get e1 k))
                (hash-put! e1 k (list)))
              (json-put! e1 k (append (json-get e1 k) v)))
             (else (json-put! e1 k v))))
     e2)))

;; misc

(def (json-empty? obj)
  (match obj
    ((json e)
     (hash-empty? e))))

(def (copy-json obj)
  (match obj
    ((json e)
     (make-json (string->json
                 (json->string e))))))

;; Ref interface
;; This enables common used values to be saved in
;; an own entry and only referenced inthe object.
;; This can saves space.

(def (json-make-ref! ht ref-cat val)

  (def (make-new-ref! refs val)
    (let (new-ref (number->string (fx1+ (hash-length refs))))
      (json-add! refs new-ref val)
      new-ref))

  (def (make-refs! ht ref-cat)
    (json-add! ht ref-cat (make-json)))

  (let (refs (json-get ht ref-cat))
    (if refs
      (begin
        (let (ref (call/cc
                    (lambda (ret)
                      (hash-for-each
                       (lambda (k v)
                         (when (equal? v val)
                           (ret k)))
                       refs)
                      (ret #f))))
          (if ref
            ref
            (make-new-ref! refs val))))
      (begin
        (make-refs! ht ref-cat)
        (json-make-ref! ht ref-cat val)))))

(def (json-ref-lookup obj key ref)
  (match obj
    ((json e) (json-get e key ref))))

(def (json-refs-expand! obj refs)
  (match obj
    ((json e)
     (hash-for-each
      (lambda (k v)
        (let (ref-val (json-get e k v))
          (when ref-val
            (json-put! e k ref-val))))
      refs))))

;; Printing.
;; wrappers from std/text/json

(def (pretty-json obj)
  (match obj
    ((json e)
     (filter-with-process
      ["jq" "-M" "."]
      (lambda (port) (std/text/json#write-json e port))
      read-all-as-string))))

(def (pretty-print-json obj (port (current-output-port)))
  (display (pretty-json obj) port)
  (newline port))

(defalias pp pretty-print-json)

;; utils

(def (ensure-json-file-exists! path)
  (unless (file-exists? path)
    (call-with-output-file path
      (lambda (out)
        (write-json (make-json) out)))))

;; actor

(defproto json+
  call:
  (get keys ...)
  event:
  (add! k v)
  (delete! k))

(def (make-json-concurent obj)
  (let ((ht (json-e obj)))
    (let lp ()
      (try
       (<- ((!json+.get keys ... k)
            (apply json-get `(,ht ,@keys))
            (lp))
           ((!json+.add! keys ... val)
            (apply json-add! `(,ht ,@keys ,val))
            (lp))
           ((!json+.delete! keys ...)
            (apply json-delete! `(,ht ,@keys))
            (lp)))
       (catch (e)
         (display-exception e)
         (lp))))))
