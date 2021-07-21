;;;; dot-stuff.asd

(asdf:defsystem #:dot-stuff
  :description "Describe dot-stuff here"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("cz.zellerin.doc")
  :components ((:file "package")
               (:file "dot-stuff")))
