(defpackage #:diasbruno.page.index
  (:use #:cl #:str #:spinneret)
  (:export
   #:render-index-page))

(in-package #:diasbruno.page.index)

(defclass index-page (diasbruno.page:page
		      diasbruno.page:page-datasource)
  ())

(defun index-page-renderer (posts)
  (with-output-to-string (rendered)
    (setf spinneret:*html* rendered)
    (diasbruno.page:page-layout
      (dolist (post diasbruno.page.index::posts)
	(:div :attrs (list :class "article-item")
	      (:a :attrs (list :href (diasbruno.post:article-full-link post))
		  (:h1 (diasbruno.post:post-title post)))
	      (:div
	       (list (:tag :name :time
			   :attrs (list :class "content-datetime"
					:datetime (diasbruno.post:article-datetime post))
			   (diasbruno.post:article-date (diasbruno.post:post-date post))))))))))

(defmethod render ((page index-page))
  (let ((posts (diasbruno.database:database-data
		(diasbruno.page:page-source page))))
    (diasbruno.page:write-page-to-file
     (index-page-renderer
      (remove-if #'diasbruno.post:is-hidden posts))
     (merge-pathnames "index.html"
		      (str:concat (namestring diasbruno.configuration:*destination*)
				  "/")))))

(defun render-index-page (post-database)
  (render (make-instance 'index-page :source post-database)))
