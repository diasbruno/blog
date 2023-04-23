(defpackage #:diasbruno.opensource
  (:use #:cl #:diasbruno.database)
  (:export
   #:initialize-opensource-database))

(in-package #:diasbruno.opensource)

(defclass opensource-database (diasbruno.database:database)
  ())

(defmethod read-row ((db opensource-database) row)
  )

(defmethod write-row ((db opensource-database) row)
  )

(defmethod init ((db opensource-database))
  (let ((data-from-json (with-open-file
			    (file (database-source db))
			  (cl-json:decode-json file))))
    (setf (slot-value db 'diasbruno.database::data)
	  (mapcar (lambda (row) (read-row db row))
		  data-from-json))))

(defun initialize-opensource-database (path)
  (let ((db (make-instance 'opensource-database :source path)))
    (init db)
    db))
