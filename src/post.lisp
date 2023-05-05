;;;; diasbruno.post.lisp

(defpackage #:diasbruno.post
  (:use #:cl)
  (:export
   #:post
   #:post-date
   #:post-title
   #:post-status
   #:make-post
   #:is-hidden
   #:initialize-post-database
   #:slugify
   #:date-to-uri-path
   #:date-path-segment
   #:article-datetime
   #:article-date
   #:article-content-filename
   #:article-full-link
   #:create-item
   #:reload
   #:save))

(in-package #:diasbruno.post)

(defstruct post
  date
  title
  status)

(defun is-hidden (post)
  (equal "hidden" (post-status post)))

(defun slugify (str)
  (change-case:lower-case
   (cl-slugify:string-to-slug str)))

(defun date-to-uri-path (date)
  (local-time:format-timestring
   nil
   date
   :format diasbruno.configuration:+yyyymmdd+))

(defun date-path-segment (date)
  (date-to-uri-path date))

(defun article-datetime (post)
  (local-time:format-timestring
   nil
   (post-date post)
   :format diasbruno.configuration:+yyyy-mm-ddthh/mm/ss+))

(defun article-date (date)
  (local-time:format-timestring
   nil
   date
   :format diasbruno.configuration:+post-display-date+))

(defun article-content-filename (post)
  (str:concat
   (local-time:format-timestring
    nil
    (post-date post)
    :format diasbruno.configuration:+yyyymmddthhmmss+)
   "-"
   (slugify (post-title post))
   ".md"))

(defun article-full-link (post &optional (for-uri t))
  (str:concat (or (and for-uri "/") "") "articles/"
	      (date-to-uri-path (post-date post))
	      "/"
	      (slugify (post-title post))))
