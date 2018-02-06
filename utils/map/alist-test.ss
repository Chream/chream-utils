;;;; gerbil-scheme/funds/src/alist-test.ss

(import "alist" "../logger")

(export run-alist-tests)

;; Setup
(def alist1       '((a . 5) (b . 6) (c . 10)))
(def alist-empty1 '())
(def alist-fail1  '(a (b . 6)))
(def log-str "alist-test")

(def (test-lookup)
  (log test log-str (values->list (lookup alist1 'a)))
  (log test log-str (values->list (lookup alist1 'b)))
  (log test log-str (values->list (lookup alist1 'd)))
  (log test log-str (values->list (lookup alist-empty1 'a)))
  ;; (log test (values->list (lookup alist-fail1 'b)))
  ;; (log test (values->list (lookup alist-fail1 'a)))
  'ok)


;; (5 #t)
;; (6 #t)
;; (#f #f)
;; (#f #f)
;; *** ERROR IN #<procedure #10> -- No clause matching a

(def (test-drop)
  (log test log-str (values->list (drop alist1 'a)))
  (log test log-str (values->list (drop alist1 'b)))
  (log test log-str (values->list (drop alist1 'd)))
  (log test log-str (values->list (drop alist-empty1 'a)))
  ;; (log test (values->list (drop alist-fail1 'b)))
  ;; (log test (values->list (drop alist-fail1 'a)))
  'ok)

;; (((b . 6) (c . 10)))
;; (((a . 5) (c . 10)))
;; (((a . 5) (b . 6) (c . 10)))
;; (())
;; *** ERROR IN filter -- No clause matching a

(def (test-insert)
  (log test log-str (values->list (insert alist1 'a 10)))
  (log test log-str (values->list (insert alist1 'b 20)))
  (log test log-str (values->list (insert alist1 'd 30)))
  (log test log-str (values->list (insert alist-empty1 'a 100)))
  ;; (log test (values->list (insert alist-fail1 'b)))
  ;; (log test (values->list (insert alist-fail1 'a)))
  'ok)

;; (((a . 10) (b . 6) (c . 10)))
;; (((b . 20) (a . 5) (c . 10)))
;; (((d . 30) (a . 5) (b . 6) (c . 10)))
;; (((a . 100)))
;; *** ERROR IN insert-test, (console)@85.28 -- Wrong number of arguments passed to procedure
;; (#<procedure #15> '#<table #18> '(a (b . 6)) 'b)

(def (run-alist-tests)
  (test-lookup)
  (test-lookup)
  (test-insert))