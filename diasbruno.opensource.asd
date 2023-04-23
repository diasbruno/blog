;;;; diasbruno.opensource.asd

(asdf:defsystem #:diasbruno.opensource
  :description "Describe diasbruno.opensource here"
  :author "Bruno Dias"
  :license  "unlicense"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-json)
  :components ((:module "src"
		:components ((:file "opensource")))))
