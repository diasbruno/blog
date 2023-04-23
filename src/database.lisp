(defpackage #:diasbruno.database
  (:use #:cl #:cl-json)
  (:export
   #:load-from-file
   #:database
   #:database-data
   #:database-source))

(in-package #:diasbruno.database)

(defclass database ()
  ((data :accessor database-data :initarg :data :initform nil :reader database-data)
   (source :accessor database-source :initarg :source :initform nil :reader database-source)))

(defgeneric init (database))

(defgeneric read-row (database row))
(defgeneric write-row (database row))

(defun write-to-file (destination database each-row)
  (alexandria:write-string-into-file
   (cl-json:encode-json-to-string
    (mapcar (lambda (row) (funcall each-row row))
	    posts))
   source
   :if-exists :overwrite
   :if-does-not-exist :create))
