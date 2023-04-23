(ql:quickload :cl-ini)

(defvar *metadatas* nil)
(setf *metadatas* nil)
(map-directory
 (lambda (x)
   (let ((ini (ini:parse-ini (open (merge-pathnames x #P"/metadata.sh")))))
     (setf *metadatas* (push ini *metadatas*))))
 "/usr/local/src/site-content/contents/"
 :directories t)

; convert metadata into lisp make-post
(flet ((from-ini-string (str)
	 (remove #\" (remove #\" str :start 0) :start 0 :from-end t)))
  (map 'list (lambda (metadata)
	       (let ((data (cdr (car metadata))))
		 `(make-post :title ,(from-ini-string (cdr (assoc :title data)))
			     :date ,(from-ini-string (cdr (assoc :date data)))
			     :status ,(from-ini-string (cdr (assoc :status data))))))
       *metadatas*))

; convert all content to slug .md
(map-directory
 (lambda (x)
   (let* ((source-content "/usr/local/src/site-content/contents/")
	  (ini (ini:parse-ini (open (merge-pathnames x #P"/metadata.sh"))))
	  (content (uiop:read-file-string (merge-pathnames x #P"/content.md")))
	  (metadata (cdar ini))
	  (slug (remove #\" (cdr (assoc :slug metadata))))
	  (date (remove #\" (cdr (assoc :date metadata))))
	  (renamed-content-file (merge-pathnames (pathname (str:concat
							    (car (last (str:split #\/ (remove #\/ (namestring x) :count 1 :from-end t))))
							    ".md"))
						 source-content)))

     (print renamed-content-file)
     (alexandria:write-string-into-file
      content
      renamed-content-file)))
 "/usr/local/src/site-content/contents/"
 :directories t)
