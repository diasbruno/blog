(defpackage #:diasbruno.configuration
  (:use #:cl)
  (:export
   #:*source*
   #:*destination*
   #:*database*
   #:*opensource*
   #:*markdown-compiler*
   #:is-writing))

(in-package #:diasbruno.configuration)

(defvar *env*
  :writing)

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

(defun is-writing ()
  (equal :writing *env*))
