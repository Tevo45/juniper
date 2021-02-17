;;;; package.lisp

(defpackage #:juniper
  (:use #:cl #:alexandria)
  (:export :generate-bindings
	   
	   :bindings-from-file
	   :bindings-from-json
	   :bindings-from-url))
