;;;; diasbruno.configuration.asd

(asdf:defsystem #:diasbruno.configuration
  :depends-on (:chameleon)
  :description "Describe diasbruno.configuration here"
  :author "Bruno Dias"
  :license  "unlicense"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :components ((:file "configuration")))))
