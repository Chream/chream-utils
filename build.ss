#!/usr/bin/env gxi-script

(import :std/build-script
        :std/make)

(let (srcdir (string-append (path-directory (this-source-file)) "/utils/"))
  (make srcdir: srcdir '("map/alist"
                         "map/alist-test"
                         "logger")))

;; (defbuild-script
;;   (filter-map
;;    (lambda (filename)
;;      (and (equal? (path-extension filename) ".ss")
;;           (path-expand (path-strip-extension filename) "src")))
;;    (directory-files "src")))