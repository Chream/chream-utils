;; -*- Gerbil -*-
;; Path configuration.
;; File taken and modicifed from clan pacakge.

(export #t)

(import
  :clan/utils/base
  :clan/utils/config
  :clan/utils/filesystem
  "text/json"
  "misc/repr")

;; This struct should constructed for each app. It can be passed
;; to the defined procedures or parameterized through 'current-env-context`

(defstruct app-env (name source-var home-var log-var)
  constructor: init!)

(defmethod {init! app-env}
  (lambda (self name: name home: home log: log source: source)
    (app-env-name-set! self name)
    (app-env-source-var-set! self source)
    (app-env-home-var-set! self home)
    (app-env-log-var-set! self log)))

(def (current-env-context)
  (make-parameter #f))

(def (source-directory (env (current-env-context)))
  (let (source-var (app-env-source-var env))
    (displayln source-var)
    (unless source-var
        (error "(application-source-envvar) not set!."))
    (getenv source-var)))

(def (home-directory (env (current-env-context)))
  (cond ((app-env-home-var env) => getenv)
        (else (path-expand "build/home"
                           (source-directory)))))

(def (log-directory (env (current-env-context)))
  (cond ((app-env-log-var env) => getenv)
        (else (path-expand "build/log"
                           (source-directory)))))

(def (bin-directory)
   (path-expand ".build_outputs" (source-directory)));; executables

(def (cache-directory)
   (path-expand "cache" (home-directory)));; dynamic cache

(def (config-directory)
   (path-expand "config" (home-directory)));; configuration files

(def (data-directory)
   (path-expand "data" (home-directory)));; static data

(def (run-directory)
   (path-expand "run" (home-directory))) ;; transient state

(def (bin-path . x) (apply subpath (bin-directory) x))
(def (cache-path . x) (apply subpath (cache-directory) x))
(def (config-path . x) (apply subpath (config-directory) x))
(def (data-path . x) (apply subpath (data-directory) x))
(def (run-path . x) (apply subpath (run-directory) x))
(def (source-path . x) (apply subpath (source-directory) x))
