;;;; diasbruno.post.lisp

(defpackage #:diasbruno.post
  (:use #:cl)
  (:export
   #:post
   #:post-date
   #:post-title
   #:post-status
   #:make-post
   #:is-hidden
   #:initialize-post-database
   #:slugify
   #:date-to-uri-path
   #:date-path-segment
   #:article-datetime
   #:article-date
   #:article-content-filename
   #:article-full-link))

(in-package #:diasbruno.post)

(defstruct post
  date
  title
  status)

(defun is-hidden (post)
  (equal "hidden" (post-status post)))

(defclass post-database (diasbruno.database:database)
  ())

(defmethod read-row ((db post-database) row)
  (destructuring-bind (title date status)
      row
    (make-post :title (cdr title)
	       :status (cdr status)
	       :date (local-time:parse-timestring (cdr date)))))

(defmethod write-row ((db post-database) row)
  `((:title . ,(post-title row))
    (:date . ,(local-time:format-timestring
	       nil
	       (post-date row)
	       :format +iso-8601-format+))
    (:status . ,(change-case:lower-case (post-status row)))))

(defmethod init ((db post-database))
  (let ((data-from-json (with-open-file
			    (file (diasbruno.database:database-source db))
			  (cl-json:decode-json file))))
    (setf (diasbruno.database:database-data db)
	  (mapcar (lambda (row) (read-row db row))
		  data-from-json))))

(defmethod reload ((db post-database))
  (init db))

(defmethod create-item ((db post-database) &key title date status)
  (let ((post (make-post :title title
			 :date date
			 :status status)))
    (setf (diasbruno.database:database-data db)
	  (append (diasbruno.database:database-data db)
		  (list post)))
    (print (diasbruno.database:database-data db))
    post))

(defmethod save ((db post-database))
  (let ((posts (diasbruno.database:database-data db)))
    (diasbruno.database:write-to-file
     (database-source db)
     (cl-json:encode-json-to-string
      (mapcar (lambda (post)
		(write-row db post))
	      posts)))))

(defun initialize-post-database (path)
  (let ((db (make-instance 'post-database :source path)))
    (init db)
    db))

(defparameter +iso-8601-time-no-usec-format+
  '((:hour 2) #\: (:min 2) #\: (:sec 2)))

(defparameter +iso-8601-format+
  (append local-time:+iso-8601-date-format+
	  (list #\T)
	  +iso-8601-time-no-usec-format+
	  (list :gmt-offset-or-z)))

(defparameter +rfc3339-format/date-only/path+
  '((:year 4) #\/ (:month 2) #\/ (:day 2)))

(defparameter +fsdate-format+
  '((:year 4) (:month 2) (:day 2) (:hour 2) (:min 2) (:sec 2)))

(defun slugify (str)
  (change-case:lower-case
   (cl-slugify:string-to-slug str)))

(defun date-to-uri-path (date)
  (local-time:format-timestring nil
				date
				:format +rfc3339-format/date-only/path+))

(defun date-path-segment (date)
  (date-to-uri-path date))

(defun article-datetime (post)
  (local-time:format-timestring nil
				(post-date post)
				:format +iso-8601-format+))

(defun article-date (date)
  (local-time:format-timestring nil
				date
				:format '(:short-month #\SPACE (:day 2) #\, #\SPACE (:year 4))))

(defun article-content-filename (post)
  (str:concat
   (local-time:format-timestring nil
				 (post-date post)
				 :format +fsdate-format+)
   "-"
   (slugify (post-title post))
   ".md"))

(defun article-full-link (post &optional (for-uri t))
  (str:concat (or (and for-uri "/") "") "articles/"
	      (date-to-uri-path (post-date post))
	      "/"
	      (slugify (post-title post))))
