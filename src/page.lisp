(defpackage #:diasbruno.page
  (:use #:cl)
  (:export
   #:page
   #:page-source
   #:page-layout
   #:page-datasource
   #:write-page-to-file))

(in-package #:diasbruno.page)

(defclass page ()
  ())

(defclass page-datasource ()
  ((source :accessor source :initarg :source :initform nil :reader page-source)))

(defun write-page-to-file (rendered path)
  (ensure-directories-exist path)
  (alexandria:write-string-into-file
   rendered
   path
   :if-exists :overwrite
   :if-does-not-exist :create))

(defun navigation ()
  (spinneret:with-html-string
    (:tag :name :nav
	  (list (:div (:a :attrs (list :href "/") (:h1 "diasbruno")))
		(:div :attrs (list :class "nav-links")
		      (list
		       (:a :attrs (list :href "/" :class "nav-links")
			   "articles")
		       (:a :attrs (list :href "/opensource" :class "nav-links")
			   "opensource")
		       (:a :attrs (list :href "https://github.com/diasbruno" :class "nav-links" :target "_blank")
			   "github")))))))

(defmacro page-layout (&body body)
  `(spinneret:with-html
     (:doctype)
     (:html :attrs (list :prefix "og: http://ogp.me/ns#")
       (:head (list
	       (:title "diasbruno")
	       (:tag :name :meta :attrs (list :name "viewport"
					      :content "width=device-width"))
	       (dolist (style-href '("/css/style.css" "/css/highlight.min.css" "/css/milligram.css"))
		 (:tag :name :link :attrs (list :rel "stylesheet" :type "text/css" :href style-href)))))
       (:body
	(list
	 (:tag :name :main (list (:raw (navigation))
				 (:section :attrs (list :class "content")
					   ,@body)))
	 (:script :attrs (list :type "application/javascript" :src "/js/highlight.min.js")))))))

(defgeneric render (page))
