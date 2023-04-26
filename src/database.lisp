(defpackage #:diasbruno.database
  (:use #:cl #:cl-json)
  (:export
   #:load-from-file
   #:database
   #:database-data
   #:database-source
   #:create-item
   #:save
   #:write-to-file
   #:reload))

(in-package #:diasbruno.database)

(defclass database ()
  ((data :accessor database-data :initarg :data :initform nil :reader database-data)
   (source :accessor database-source :initarg :source :initform nil :reader database-source)))

(defgeneric init (database))

(defgeneric read-row (database row))
(defgeneric write-row (database row))

(defgeneric create-item (database &key))
(defgeneric save (database))

(defgeneric reload (database))

(defun write-to-file (path content)
  (alexandria:write-string-into-file
   content
   path
   :if-exists :overwrite
   :if-does-not-exist :create))
