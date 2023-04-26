(defpackage #:diasbruno.opensource
  (:use #:cl #:diasbruno.database)
  (:export
   #:initialize-opensource-database
   #:opensource
   #:opensource-name
   #:opensource-url
   #:opensource-language
   #:opensource-description))

(in-package #:diasbruno.opensource)

(defstruct opensource
  name
  url
  language
  description)

(defclass opensource-database (diasbruno.database:database)
  ())

(defmethod read-row ((db opensource-database) row)
  (destructuring-bind (name url language description)
      row
    (make-opensource :name (cdr name)
		     :url (cdr url)
		     :language (cdr language)
		     :description (cdr description))))

(defmethod write-row ((db opensource-database) row)
  `((:name . ,(opensource-name row))
    (:url . ,(opensource-url row))
    (:language . ,(change-case:lower-case (opensource-language row)))
    (:description . ,(opensource-description row))))

(defmethod init ((db opensource-database))
  (let ((data-from-json (with-open-file
			    (file (database-source db))
			  (cl-json:decode-json file))))
    (setf (slot-value db 'diasbruno.database::data)
	  (mapcar (lambda (row) (read-row db row))
		  data-from-json))))

(defmethod create-item ((db opensource-database) &key name url description language)
  (let ((project (make-opensource :name name :url url :description description :language language)))
    (setf (database-data db)
	  (append (database-data db) (list project)))))

(defmethod reload ((db opensource-database))
  (init db))

(defmethod save ((db opensource-database))
  (let ((projects (database-data db)))
    (diasbruno.database:write-to-file
     (database-source db)
     (cl-json:encode-json-to-string
      (mapcar (lambda (project)
		(write-row db project))
	      projects)))))

(defun initialize-opensource-database (path)
  (let ((db (make-instance 'opensource-database :source path)))
    (init db)
    db))
