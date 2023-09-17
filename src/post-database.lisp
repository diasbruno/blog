(defpackage #:diasbruno.database.post
  (:use #:cl)
  (:export
   #:post-database
   #:read-row
   #:write-row
   #:init
   #:reload
   #:create-item
   #:save
   #:initialize-post-database))

(in-package #:diasbruno.database.post)

(defclass post-database (diasbruno.database:database)
  ())

(defmethod read-row ((db post-database) row)
  (destructuring-bind (title slug date status)
      row
    (format t "loading post: ~a ~a ~a ~a~%" title slug date status)
    (diasbruno.post:make-post :title title
                              :status status
                              :date (local-time:parse-timestring date)
                              :slug slug)))

(defmethod write-row ((db post-database) row)
  (error "not implemented for in memory data source"))

(defmethod init ((db post-database))
  (let ((data (mapcar
               (lambda (row) (read-row db row))
               (diasbruno.database:database-source db))))
    (setf (diasbruno.database:database-data db)
          (sort data (lambda (a b)
                       (>
                        (parse-integer (local-time:format-timestring nil (diasbruno.post:post-date a) :format diasbruno.configuration:+yyyymmddthhmmss+))
                        (parse-integer (local-time:format-timestring nil (diasbruno.post:post-date b) :format diasbruno.configuration:+yyyymmddthhmmss+))))))))

(defmethod reload ((db post-database))
  (init db))

(defmethod create-item ((db post-database) &key title date status)
  (let ((post (diasbruno.post:make-post :title title
                                        :date date
                                        :status status)))
    (setf (diasbruno.database:database-data db)
          (append (diasbruno.database:database-data db)
                  (list post)))
    post))

(defmethod save ((db post-database))
  (let ((posts (diasbruno.database:database-data db)))
    (diasbruno.database:write-to-file
     (diasbruno.database:database-source db)
     (cl-json:encode-json-to-string
      (mapcar (lambda (post)
                (write-row db post))
              posts)))))

(defun initialize-post-database (source)
  (let ((db (make-instance 'post-database :source source)))
    (init db)
    db))
