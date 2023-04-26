(defpackage #:diasbruno.page.opensource
  (:use #:cl)
  (:export
   #:render-opensource-page))

(in-package #:diasbruno.page.opensource)

(defclass opensource-page (diasbruno.page:page
			   diasbruno.page:page-datasource)
  ())

(defun group-by (data &key fn)
  (reduce (lambda (acc project)
	    (let* ((lang (funcall fn project))
		   (stack (gethash lang acc)))
	      (setf (gethash lang acc)
		    (append stack (list project)))
	      acc))
	  data
	  :initial-value (make-hash-table :test #'equal)))

(defun opensource-page-renderer (projects)
  (with-output-to-string (rendered)
    (let ((spinneret:*html* rendered))
      (diasbruno.page:page-layout
	(maphash (lambda (key projects-by-lang)
		   (:div :attrs (list :class "opensource-lang")
			 (list
			  (:h5 (:span key))
			  (:div :attrs (list :class "opensource-lang-list")
				(dolist (project projects-by-lang)
				  (with-slots (diasbruno.opensource::name
					       diasbruno.opensource::url
					       diasbruno.opensource::description
					       diasbruno.opensource::language)
				      project
				    (:div :attrs (list :class "opensource-item")
					  (list (:h3 (:a :href diasbruno.opensource::url
							 :attrs (list :target "_blank")
							 diasbruno.opensource::name))
						(:p diasbruno.opensource::description)
						(:p (:a :href diasbruno.opensource::url
							:attrs (list :target "_blank")
							diasbruno.opensource::url)))))))
			  (:hr))))
		 projects)))))

(defmethod render ((page opensource-page))
  (let ((projects (diasbruno.database:database-data
		   (diasbruno.page:page-source page))))
    (diasbruno.page:write-page-to-file
     (opensource-page-renderer
      (group-by projects :fn #'diasbruno.opensource:opensource-language))
     (merge-pathnames "opensource/index.html"
		      (str:concat (namestring diasbruno.configuration:*destination*)
				  "/")))))

(defun render-opensource-page (opensource-database)
  (render (make-instance 'opensource-page :source opensource-database)))
