;;;; juniper.asd

(asdf:defsystem #:juniper
  :description "A macro-based swagger code generator for Common Lisp"
  :author "Estevan Castilho <estevan.cps@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria" "cl-json" "drakma" "kebab" "puri")
  :components ((:file "package")
               (:file "juniper")))
