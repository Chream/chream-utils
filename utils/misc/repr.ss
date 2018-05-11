;;; -*- Gerbil -*-
;;; © Chream

(import :std/misc/rtd
        :std/sugar
        (only-in :gerbil/gambit display-exception)
        (only-in :std/srfi/1 append-map)
        (only-in "asserts" check-type))
(export #t)

;; object inspection

(def (slot-info obj)
  (check-type class-object? obj)
  (type-descriptor-slots (object-type obj)))

(def (slot-names obj)
  (cdar (type-descriptor-plist (object-type obj))))

(def (struct-object? obj)
  (try
   (let (plist (type-descriptor-plist
                (object-type obj)))
     (match plist
       ([[fields: slots ...]] #t)
       (else #f)))
   (catch (e)
     #f)))

(def (class-object? obj)
  (try
   (let (plist (type-descriptor-plist
                (object-type obj)))
     (match plist
       ([[slots: slots ...]] #t)
       (else #f)))
   (catch (e)
     #f)))

(def (object->list obj)
  (cond ((class-object? obj)
         (cons type: (class->list obj)))
        ((struct-object? obj)
         (let* ((type (object-type obj))
                (slot-names (map symbol->keyword (slot-names obj)))
                (slot-values (cdr (struct->list obj))))
           (cons* type: type (append-map list slot-names slot-values))))
        (else (error "Not an object ~S" obj))))

;; procedure inspection

(def (procedure-name proc)
  (if (procedure? proc)
    (##procedure-name proc)
    (error "Argument not a procedure: " proc)))
