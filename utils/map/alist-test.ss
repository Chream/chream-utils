;;; -*- Gerbil -*-
;;; © Chream

(import "alist" "../logger" "../misc/debug")

(export #t)

;; Setup
(def alist1       '((a . 5) (b . 6) (c . 10)))
(def alist-empty1 '())
(def alist-fail1  '(a (b . 6)))
(def log-str "alist-test")

(def logger (make-logger "test" top: "~/tmp" name: "alist-test-logger"))

(def (test-lookup)
  (log-debug logger log-str (values->list (lookup alist1 'a)))
  (log-debug logger log-str (values->list (lookup alist1 'b)))
  (log-debug logger log-str (values->list (lookup alist1 'd)))
  (log-debug logger log-str (values->list (lookup alist-empty1 'a)))
  ;; (log test (values->list (lookup alist-fail1 'b)))
  ;; (log test (values->list (lookup alist-fail1 'a)))
  'ok)


;; (5 #t)
;; (6 #t)
;; (#f #f)
;; (#f #f)
;; *** ERROR IN #<procedure #10> -- No clause matching a

(def (test-drop)
  (log-debug logger log-str (values->list (drop alist1 'a)))
  (log-debug logger log-str (values->list (drop alist1 'b)))
  (log-debug logger log-str (values->list (drop alist1 'd)))
  (log-debug logger log-str (values->list (drop alist-empty1 'a)))
  ;; (log-debug logger (values->list (drop alist-fail1 'b)))
  ;; (log-debug logger (values->list (drop alist-fail1 'a)))
  'ok)

;; (((b . 6) (c . 10)))
;; (((a . 5) (c . 10)))
;; (((a . 5) (b . 6) (c . 10)))
;; (())
;; *** ERROR IN filter -- No clause matching a

(def (test-insert)
  (let (l (values->list (insert alist1 'a 10)))
    (logg l)
    (log-debug logger log-str l)
    ;; (log-debug logger log-str (values->list (insert alist1 'b 20)))
    ;; (log-debug logger log-str (values->list (insert alist1 'd 30)))
    ;; (log-debug logger log-str (values->list (insert alist-empty1 'a 100)))
    )
  ;; (log-debug logger (values->list (insert alist-fail1 'b)))
  ;; (log-debug logger (values->list (insert alist-fail1 'a)))
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
