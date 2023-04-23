(defpackage #:diasbruno
  (:use #:cl #:local-time))

(in-package #:diasbruno)

(defvar *opensource-database* nil)
(setf  *opensource-database*
       (initialize-opensource-database *opensource*))

(defvar *posts-database* nil)
(setf *posts-database*
      (initialize-post-database *database*))

(diasbruno.page.index:render-index-page
 *posts-database*)

(diasbruno.page.article:render-articles-page
 *posts-database*)
