(defpackage #:diasbruno.configuration
  (:use #:cl)
  (:export
   #:*source*
   #:*destination*
   #:*database*
   #:*opensource*
   #:*markdown-compiler*))

(in-package #:diasbruno.configuration)

(defvar *source*
  #P "/usr/local/src/site-content/contents/")

(defvar *destination*
  #P "/usr/local/src/diasbruno.github.io")

(defvar *database*
  (merge-pathnames #P"database.json" *source*))

(defvar *opensource*
  (merge-pathnames #P"opensource.json" *source*))

(defvar *markdown-compiler*
  "/home/herospark/.local/bin/comrak")
