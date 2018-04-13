;;; -*- Gerbil -*-
;;;gerbil/chream-utils/utils/misc/files.ss

(export #t)

(def (file-directory? path)
  (eq? (file-type path) 'directory))

(def (ensure-file-exists! path)
  (unless (file-exists? path)
    (call-with-output-file [path: path create: #t]
      (lambda (out)
        #t))))
