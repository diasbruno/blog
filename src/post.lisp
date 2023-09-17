;;;; diasbruno.post.lisp

(defpackage #:diasbruno.post
  (:use #:cl)
  (:export
   #:post
   #:post-date
   #:post-title
   #:post-status
   #:post-slug
   #:make-post
   #:is-hidden
   #:date-to-uri-path
   #:date-path-segment
   #:article-datetime
   #:article-date
   #:article-content-filename
   #:article-full-link
   #:reload
   #:save
   #:article-filename-date))

(in-package #:diasbruno.post)

(defstruct post
  title
  slug
  date
  status)

(defun is-hidden (post)
  (equal "hidden" (post-status post)))

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

(defun article-filename-date (post)
  (local-time:format-timestring
   nil
   (post-date post)
   :format diasbruno.configuration:+yyyymmddthhmmss+))

(defun article-content-filename (post)
  (str:concat
   (article-filename-date post)
   "-"
   (post-slug post)
   ".md"))

(defun article-full-link (post &optional (for-uri t))
  (str:concat (or (and for-uri "/") "") "articles/"
              (date-to-uri-path (post-date post))
              "/"
              (post-slug post)))
