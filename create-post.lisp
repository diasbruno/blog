(load "./bootstrap.lisp")

(let* ((post (car (diasbruno.database:database-data *posts-database*)))
       (content-filename (merge-pathnames
                          (diasbruno.post:article-content-filename post)
                          diasbruno.configuration:*source*)))
  (with-open-file (f content-filename :direction :output)))

(ql:quickload :cl-fad)
