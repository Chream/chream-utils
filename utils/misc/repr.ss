;;; -*- Gerbil -*-
;;;gerbil/chream-utils/utils/misc/repr.ss

(import :std/misc/rtd
        (only-in :std/srfi/1 append-map))
(export #t)

;; object inspection

(def (slot-nr obj)
  (type-descriptor-slots (object-type obj)))

(def (slot-names obj)
  (type-descriptor-plist (object-type obj)))

(def (object->list obj)
  (cond ((class-instance? obj)
         (class->list obj))
        ((struct-instance? obj #f)
         (struct->list obj))))

(def (object-info obj)
  (cons (object-type obj)
        (map (cut cons <> <>)
             (append-map cdr (slot-names obj))
             ;; Works for class as well.
             ;; will not return names though.
             (cdr (struct->list obj)))))

;; procedure inspection

(def (procedure-name proc)
  (if (procedure? proc)
    (##procedure-name proc)
    (error "Argument not a procedure: " proc)))
