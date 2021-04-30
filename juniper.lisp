;;;; juniper.lisp

(in-package #:juniper)

;; those are used by the generator internally and should be globally unbound
(defvar *schema*)
(defvar *proto*)
(defvar *url*)
(defvar *base-path*)
(defvar *accept-header*)
(defvar *endpoint*)
(defvar *path-params*)

;; those can be used to change the behaviour of the generated functions at runtime
(defvar *host* nil)
(defvar *port* nil)
(defvar *drakma-extra-args* ())

(defun lisp-sym (str)
  (read-from-string (kebab:to-lisp-case str)))

(defun genparams (op urlsym hdrsym paramssym bodysym formsym)
  (let ((required '())
	(optional '())

	(code '()))
    (loop
      for param in (append *path-params* (cdr (assoc :|parameters| (cdr op))))
      do (let* ((name (cdr (assoc :|name| param)))
		(namesym (lisp-sym name))
		(namesym-p (lisp-sym (concatenate 'string name "-supplied-p")))
		(isrequired (cdr (assoc :|required| param)))
		(in (cdr (assoc :|in| param))))
	   (if isrequired
	       (push namesym required)
	       (push `(,namesym nil ,namesym-p) optional))
	   (push
	    `(if ,(if isrequired t namesym-p)
		 ,(switch (in :test #'string=)
		    ("path"
		     `(setf ,urlsym
			    (cl-ppcre:regex-replace ,(format nil "{~a}" name)
						    ,urlsym
						    (format nil "~a" ,namesym))))
		    ("query"
		     `(push (cons ,name (format nil "~a" ,namesym))
			    ,paramssym))
		    ("header"
		     `(push (cons ,name (format nil "~a" ,namesym))
			    ,hdrsym))
		    ("body"
		     `(setf ,bodysym
			    (concatenate 'string ,bodysym
					 (json:encode-json-to-string ,namesym))))
		    ("formData"
		     `(progn
			(setf ,formsym t)
			(push (cons ,name (format nil "~a" ,namesym))
			      ,paramssym)))
		    (otherwise
		     (warn "Don't know how to handle parameters in ~a." in))))
		 code)))
    (if (> (length optional) 0)
	(push '&key optional))
    ;; FIXME we sometimes somehow generate duplicate parameters, specifics still unclear
    ;; reproducible with the LCU schema for version 11.2.353.8505, removing duplicates on
    ;; the loop does not work
    ;; below call to remove-duplicates on the parametes is a workaround so that we can
    ;; at least generate valid function params, but there's still duplicate handling code
    (values (remove-duplicates (append required optional)) code)))

(defun opmethod (op) ; FIXME is there a better way to "uppercase a symbol"?
  (read-from-string (concatenate 'string ":" (string (car op)))))

(defun ophelp (op)
  (cdr (assoc :|summary| (cdr op))))

(defun path-funcname (pathop)
  (lisp-sym (cdr (assoc :|operationId| (cdr pathop)))))

(defun genfunc (op)
  (with-gensyms (urlsym hdrsym paramssym bodysym formsym responsesym streamsym parsedsym resstrsym)
    (multiple-value-bind (params code) (genparams op urlsym hdrsym paramssym bodysym formsym)
      `(defun ,(path-funcname op) ,params ; FIXME
	 ,(ophelp op)
	 (let ((,urlsym ,*url*)
	       (,hdrsym '())
	       (,paramssym '())
	       (,bodysym nil)
	       (,formsym nil))
	   ,@code
	   (let ((,parsedsym (puri:uri ,urlsym)))
	     (if juniper:*host*
		 (setf (puri:uri-host ,parsedsym) juniper:*host*))
	     (if juniper:*port*
		 (setf (puri:uri-port ,parsedsym) juniper:*port*))
	     (let* ((,responsesym (apply #'drakma:http-request
					(puri:render-uri ,parsedsym nil)
					:method ,(opmethod op)
					:parameters ,paramssym
					:additional-headers ,hdrsym
					:form-data ,formsym
					:content-type "application/json" ; FIXME
					:content ,bodysym
					:accept ,*accept-header*
					juniper:*drakma-extra-args*))
		    (,resstrsym (flexi-streams:octets-to-string ,responsesym :external-format :utf-8)))
	       (if (> (length ,resstrsym) 0)
		   (with-input-from-string (,streamsym ,resstrsym)
		     (json:decode-json ,streamsym)))))))))) ; FIXME we just assume this returns json, it might not

(defun swagger-bindings ()
  `(progn
     ,@(loop
	 for path in (cdr (assoc :|paths| *schema*))
	 append (let* ((*endpoint*    (string (car path)))
		       (*url*         (format nil "~a://~a~a~a" *proto* *host* *base-path* *endpoint*))
		       (*path-params* (cdr (assoc :|parameters| (cdr path)))))
		  (loop
		    for op in (cdr path)
		    collect (genfunc op))))))

(defun generate-bindings (jsonstream &key proto host base-path accept-header)
  "Generates Swagger/OpenAPI bindings based on JSON from `jsonstream`. Optional parameters can be used to override specific fields from the schema"
  (let* ((cl-json:*json-identifier-name-to-lisp* (lambda (x) x))
	 (*schema*        (json:decode-json jsonstream))
	 
	 (version         (or (cdr (assoc :|swagger| *schema*)) (cdr (assoc :|openapi| *schema*))))
	 
	 (*proto*         (or proto         (cadr (assoc :|schemes| *schema*))    ))
	 (*host*          (or host          (cdr (assoc :|host| *schema*))        ))
	 (*base-path*     (or base-path     (cdr (assoc :|basePath| *schema*)) "/"))
	 (*accept-header* (or accept-header "application/json"                    )))
    (switch (version :test #'string=)
      ("2.0" (swagger-bindings))
      (otherwise
       (error "Unsupported swagger version ~a." version)))))
  
;;; lazy and sloppy

(defmacro defsource ((name options) &body body)
  `(defmacro ,(read-from-string (concatenate 'string "bindings-from-" (string name)))
       (,name &rest ,options &key proto host base-path (accept-header "application/json"))
     (declare (ignore proto host base-path accept-header))
     ,@body))

(defsource (file opts)
  "Generates bindings from local file at `file`"
  (with-open-file (stream (eval file))
    (apply #'generate-bindings stream opts)))

(defsource (json opts)
  "Generates bindings from a literal J1SON string"
  (with-input-from-string (stream (eval json))
    (apply #'generate-bindings stream opts)))

(defsource (url opts) ; FIXME there has to be a better way to do this
  "Generates bindings for remote schema at `url`"
  (with-input-from-string (stream (flexi-streams:octets-to-string (drakma:http-request url)))
    (apply #'generate-bindings stream opts)))
