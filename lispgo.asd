;;;; lispgo.asd

(asdf:defsystem #:lispgo
  :description "Describe lispgo here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ()
  :components ((:file "package")
               (:file "zen")
               (:file "lispgo")
               (:file "euler")))
