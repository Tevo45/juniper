;;;; package.lisp

(defpackage #:juniper
  (:use #:cl #:alexandria)
  (:export :*drakma-extra-args*
           :*host*
           :*port*

           :generate-bindings
	   
	   :bindings-from-file
	   :bindings-from-json
	   :bindings-from-url))
