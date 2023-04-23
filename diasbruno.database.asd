;;;; diasbruno.database.asd

(asdf:defsystem #:diasbruno.database
  :description "Describe diasbruno.database here"
  :author "Bruno Dias"
  :license  "unlicense"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
		:components ((:file "database")))))
