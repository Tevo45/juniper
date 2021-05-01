;;;; package.lisp

(defpackage #:juniper
  (:use #:cl #:alexandria)
  (:export :*drakma-extra-args*
           :*host*
           :*port*

           :bindings-from-stream
	   
	   :bindings-from-file
	   :bindings-from-json
	   :bindings-from-url))
