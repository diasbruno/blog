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
  (destructuring-bind (title date status)
      row
    (diasbruno.post:make-post :title (cdr title)
			      :status (cdr status)
			      :date (local-time:parse-timestring (cdr date)))))

(defmethod write-row ((db post-database) row)
  `((:title . ,(diasbruno.post:post-title row))
    (:date . ,(local-time:format-timestring
	       nil
	       (diasbruno.post:post-date row)
	       :format diasbruno.configuration:+iso-8601-format+))
    (:status . ,(change-case:lower-case (diasbruno.post:post-status row)))))

(defmethod init ((db post-database))
  (let* ((data-from-json (with-open-file
			     (file (diasbruno.database:database-source db))
			   (cl-json:decode-json file)))
	 (data (mapcar
		(lambda (row) (read-row db row))
		data-from-json)))
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

(defun initialize-post-database (path)
  (let ((db (make-instance 'post-database :source path)))
    (init db)
    db))
