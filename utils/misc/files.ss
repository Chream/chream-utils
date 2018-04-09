;;; -*- Gerbil -*-
;;;gerbil/chream-utils/utils/misc/files.ss

(export #t)

(def (file-directory? path)
  (eq? (file-type path) 'directory))
