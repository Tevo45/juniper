;;;; package.lisp

(defpackage #:juniper
  (:use #:cl #:alexandria)
  (:export :*drakma-extra-args*
	   :*url*

           :generate-bindings
	   
	   :bindings-from-file
	   :bindings-from-json
	   :bindings-from-url))
