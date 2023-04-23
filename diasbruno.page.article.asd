;;;; diasbruno.page.article.asd

(asdf:defsystem #:diasbruno.page.article
  :description "Describe diasbruno.page.article here"
  :author "Bruno Dias"
  :license  "unlicense"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-shell
	       #:diasbruno.configuration)
  :components ((:module "src"
		:components ((:file "article")))))
