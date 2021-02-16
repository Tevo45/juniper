;;;; jyoon.asd

(asdf:defsystem #:jyoon
  :description "A macro-based swagger code generator for Common Lisp"
  :author "Estevan Castilho <estevan.cps@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria" "cl-json" "drakma" "kebab")
  :components ((:file "package")
               (:file "jyoon")))
