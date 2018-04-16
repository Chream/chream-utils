;;; -*- Gerbil -*-
;;; © Chream

(import (only-in :gerbil/expander
                 current-expander-context
                 current-expander-phi
                 expander-context-table
                 expander-context-id
                 core-context-shift
                 module-binding?
                 module-export-key
                 module-export-context
                 module-binding-context
                 module-context-ns
                 import-binding?
                 import-binding-e
                 import-binding-context)
        (only-in :std/pregexp pregexp-match)
        (only-in :std/srfi/13 string-contains)
        (only-in :clan/utils/base nest)
        (only-in :clan/utils/hash hash-filter)
        (only-in "../map/hash" hash-single? hash-first-value hash-empty?))
(export #t)

(def (expander-context-table-all (ctx (current-expander-context)))
  "Finds all bindings for all phi contexts.
   Returns `table' of (key . <binding>)."
  (let lp ((ctx-1 ctx)
           (table (make-hash-table))
           (going-up? #t))
    (let* ((t (expander-context-table ctx-1))
           (table-empty? (hash-empty? t)))
      (cond ((and table-empty? (not going-up?))
             table)
            ((and table-empty? going-up?)
             ;; Resets to phi=0 and goes down.
             (lp (core-context-shift ctx  1)
                 table
                 #f))
            (going-up?
             (lp (core-context-shift ctx-1 1)
                 (hash-merge! t table)
                 going-up?))
            ((not going-up?)
             (lp (core-context-shift ctx-1 -1)
                 (hash-merge! t table)
                 going-up?))
            (else (error "wtf?"))))))

(def (expander-context-table-phi phi)
  "Finds all bindings for given PHI context.
   Returns `table' of (key . <binding>)."
  (expander-context-table
   (core-context-shift (current-expander-context) phi)))

(def (expander-context-table-search-generator eq-fn?)
  "This matches KEY for each bound identifier in the
   current context. Return a `table' of (key . #<import-binding>.)"
  (lambda (key ctx phi)
    (let ((ctx-table (if (eq? phi all:)
                       (expander-context-table-all ctx)
                       (expander-context-table-phi phi))))
      (hash-filter ctx-table
                   (lambda (k v)
                     ;; (logg key)
                     ;; (logg (symbol->string k))
                     (eq-fn? key (symbol->string k)))))))

(def (expander-context-table-search key
                                    (ctx (current-expander-context))
                                    (phi (current-expander-phi)))
  ((expander-context-table-search-generator (lambda (k v) (string-contains v k))) key ctx phi))

(def (expander-context-table-search-regexp key
                                    (ctx (current-expander-context))
                                    (phi (current-expander-phi)))
  ((expander-context-table-search-generator pregexp-match) key ctx phi))

(def (expander-context-table-lookup key
                                    (ctx (current-expander-context))
                                    (phi (current-expander-phi)))
  (let ((r ((expander-context-table-search-generator equal?)  key ctx phi)))
    (if (hash-single? r)
      (hash-first-value r)
      (error "expander context lookup returned multiple values:" r))))

(def (all-modules-exported xport (ctx-table (expander-context-table-all)))
  (let (xport-key (module-export-key xport))
    (let lp ((binding (hash-get ctx-table xport-key))
             (modules (list (string->symbol
                             (module-context-ns
                              (module-export-context xport))))))
      (if (import-binding? binding)
        (let* ((ctx (import-binding-context binding))
               (e (import-binding-e binding))
               (mod-id (expander-context-id ctx)))
          (lp e (cons mod-id modules)))
        modules))))

(def (resolve-module-export-root-module xport (ctx (current-expander-context)))
  "Takes XPORT, a `module-export' and returns
   the module name, a `string', in which it is defined."
  (nest
   (let ((xport-key (module-export-key xport))
         (ctx-table (expander-context-table-all ctx))))
   (let lp ((binding (hash-get ctx-table xport-key))))
   (let (context-id (cond ((import-binding? binding)
                           (if (not (import-binding?
                                     (import-binding-e binding)))
                             (expander-context-id
                              (import-binding-context binding))
                             (lp (import-binding-e binding))))
                          ((module-binding? binding)
                           (expander-context-id
                            (module-binding-context binding)))
                          (else (expander-context-id ctx))))
     (cond ((symbol? context-id) (symbol->string context-id))
           ((string? context-id) context-id)
           (else (error "cant resolve."))))))
