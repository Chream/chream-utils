;;; -*- Gerbil -*-
;;; © Chream

(export #t)

(def (make-locat path position)
  (##make-locat (path->container path) position))

(def (locat? x)
  (##locat? x))

(def (locat-path locat)
  (if (locat? locat)
    (##locat-container locat)
    (error "Cant get path. Not of type locat" locat)))

(def (locat-position locat)
  (if (locat? locat)
    (##locat-position locat)
    (error "Not locat object: " locat)))

(def (path->container-hook-set! x)
  (##path->container-hook-set! x))

(def (path->container path)
  (##path->container path))

(def (container->path-hook-set! x)
  (##container->path-hook-set! x))

(def (container->path container)
  (##container->path container))

(def (make-filepos line col off)
  (##make-filepos line col off))

(def (filepos-line filepos)
  (##filepos-line filepos))

(def (filepos-col filepos)
  (##filepos-col filepos))
