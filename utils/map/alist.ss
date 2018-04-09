;;;; gerbil-scheme/chream-utils/map/alist.ss

(export #t)

;; Alist.
(def (make-alist name: (name 'map) eq-fn: (eq-fn? eq?))
  (let* ((state [name])
         (map (cdr state)) ;Get rid of name.
         (type 'alist-proc))
    (lambda (msg . args)
      (let ((args (append (cons map args) (list test: eq-fn?)))) ;; All need the alist arg.
        (case msg
          ((lookup get fetch ref recall)       (apply lookup args))
          ((drop! del! delete! remove!)   (set! map (apply drop args)))
          ((insert! ins! set! store! install!) (set! map (apply insert args)))
          ((update!)                           (set! map (apply update args)))
          ((clear!)                            (set! map '()))
          ((type)                              type)
          ((state)                             map)
          (else (error "Invalid command passed to alist procedure: ~S" msg)))))))

;; Stateful (proc) interface.
(def (alist-proc? proc)
  (and (procedure? proc)
       (eq? 'alist-proc (proc 'type))))
(def (drop! map-proc k)
  (map-proc 'drop! k))
(def (insert! map-proc k v)
  (map-proc 'insert! k v))
(def (update! map-proc k v)
  (map-proc 'update! k v))

;; Pure interface.
(def (empty-alist) '())
(def (clear alist) '())

(def (lookup alist/map-proc k test: (eq-fn? equal?))
  (if (alist-proc? alist/map-proc)
    (alist/map-proc 'lookup k)
    (let lp ((alist alist/map-proc))
      (cond ((null? alist)
             (values #f #f))
            ((eq-fn? k (caar alist))
             (values (cdar alist) #t))
            (else
             (lp (cdr alist)))))))

(def (drop alist k test: (eq-fn? equal?))
  (let* ((vf #f)
         (f? #f)
         (map-new (filter (match <> ([kcur . vcur]
                                     (if (eq-fn? kcur k)
                                       (begin
                                         (unless f?
                                           (set! vf vcur)
                                           (set! f? #t))
                                         #f)
                                       #t)))
                          map)))
    (values map-new vf f?)))

(def (insert map k v test: (eq-fn? equal?))
  (values (cons [k . v]
                (filter (match <>
                          ([kcur . vcur] (if (eq-fn? kcur k)
                                           #f #t)))
                        map))))

(def (update map k v)
  (cons [k v] map))
