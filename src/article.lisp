(defpackage #:diasbruno.page.article
  (:use #:cl)
  (:export
   #:render-articles-page))

(in-package #:diasbruno.page.article)

(defclass article-page (diasbruno.page:page)
  ((post :accessor article-post :initarg :post
         :initform nil :reader article-post)))

(defun article-filename (post)
  (merge-pathnames
   (diasbruno.post:article-content-filename post)
   (diasbruno.configuration:post-contents-path)))

(defun render-article-content (post)
  (let ((command (str:concat
                  diasbruno.configuration:*markdown-compiler*
                  " --unsafe "
                  (namestring (article-filename post)))))
    (trivial-shell:shell-command command)))

(defun article-page-renderer (post)
  (with-output-to-string (rendered)
    (let ((spinneret:*html* rendered))
      (diasbruno.page:page-layout
        (:div :attrs (list :class "article-section")
              (list
               (:h1 (diasbruno.post:post-title post))
               (:div :attrs (list :class "content-info")
                     (:span "article")
                     " - "
                     (:span (change-case:lower-case
                             (diasbruno.post:post-status post)))
                     " - "
                     (:tag :name :time
                           :attrs (list :class "content-datetime"
                                        :datetime (diasbruno.post:article-datetime post))
                           (diasbruno.post:article-date (diasbruno.post:post-date post))))
               (:raw (render-article-content post))))))))

(defmethod render ((page article-page))
  (let ((post (article-post page)))
    (diasbruno.page:write-page-to-file
     (article-page-renderer (article-post page))
     (merge-pathnames (str:concat
                       (diasbruno.post:article-full-link post nil)
                       "/index.html")
                      (str:concat
                       (namestring
                        (diasbruno.configuration:destination-path))
                       "/")))))

(defclass articles-page (diasbruno.page:page
                         diasbruno.page:page-datasource)
  ())

(defmethod render ((page articles-page))
  (let* ((posts-data (diasbruno.database:database-data
                      (diasbruno.page:page-source page)))
         (posts (if (diasbruno.configuration:is-writing)
                    posts-data
                    (remove-if #'diasbruno.post:is-hidden
                               posts-data))))
    (mapcar (lambda (post)
              (render (make-instance 'article-page :post post)))
            posts)))

(defun render-articles-page (post-database)
  (render (make-instance 'articles-page :source post-database)))
