(defpackage #:diasbruno
  (:use #:cl #:local-time))

(in-package #:diasbruno)

(defvar *opensource-database* nil)
(setf  *opensource-database*
       (diasbruno.opensource:initialize-opensource-database
	diasbruno.configuration:*opensource*))

(defvar *posts-database* nil)
(setf *posts-database*
      (diasbruno.post:initialize-post-database
       diasbruno.configuration:*database*))
