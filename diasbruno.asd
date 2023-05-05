;;;; diasbruno.asd

(asdf:defsystem #:diasbruno
  :description "Describe diasbruno here"
  :author "Bruno Dias"
  :license  "unlicense"
  :version "0.0.1"
  :serial t
  :depends-on (#:str
	       #:local-time
	       #:cl-json
	       #:cl-slugify
	       #:spinneret
	       #:diasbruno.configuration
	       #:diasbruno.database
	       #:diasbruno.post
	       #:diasbruno.database.post
	       #:diasbruno.opensource
	       #:diasbruno.page
	       #:diasbruno.page.index
	       #:diasbruno.page.article
	       #:diasbruno.page.opensource)
  :components ((:module "src"
		:components ((:file "personal-site")))))
