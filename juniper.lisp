;;;; juniper.lisp

(in-package #:juniper)

;; those are used by the generator internally and should be globally unbound
(defvar *schema*)
(defvar *accept-header*)
(defvar *endpoint*)
(defvar *path-params*)
(defvar *required-as-keyword*)
(defvar *content-type*)
(defvar *export*)

;; can't have them as parameters to the generated functions lest them conflict
;; with a parameter in the schema; unsure if using dynamic variables for this
;; is very ideal though
;; *drakma-extra-args* also breaks the abstraction and ties us up to drakma

;; those can be used to change the behaviour of the generated functions at runtime
(defvar *proto*)
(defvar *base-path*)
(defvar *host*)
(defvar *port*)
(defvar *default-headers*)
(defvar *drakma-extra-args* nil)

;;; utilities
;; `mkstr` and `symb` are from Let over Lambda, which I believe were taken from On Lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun lisp-symbol (val)
    (intern (string-upcase (kebab:to-lisp-case (string val))))))

(defun assoc-field (item alist)
  "Looks up the value associated with `item` in `alist`"
  (cdr (assoc item alist)))

(defun merge-alist (defaults specifics &optional (test #'equal))
  "Non-destructively merge defaults into specifics."
  (loop for default in defaults
        do (pushnew default specifics :test test :key #'car))
  specifics)

(defun replicate (item &aux (cons (list item)))
  "Returns an infinite list of `item`"
  (setf (cdr cons) cons))

(defun fetch-and-parse-json (url)
  (cl-json:decode-json-from-string
   (flexi-streams:octets-to-string
    (drakma:http-request url))))

(defun resolve-ref (target &optional root
		    &aux (cl-json:*json-identifier-name-to-lisp* (lambda (x) x)))
  (labels ((recursive-resolve-ref (target root)
	     (if (null target)
		 root
		 (recursive-resolve-ref
		  (cdr target)
		  (assoc-field (car target) root)))))
    (destructuring-bind (url location) (cl-ppcre:split "#" target :limit 2)
      (unless (zerop (length url))
	(setf root (fetch-and-parse-json url)))
      (recursive-resolve-ref
       ; FIXME validate string first
       (mapcar #'intern (cdr (cl-ppcre:split "/" location))
	       (replicate 'keyword))
       root))))

(defun field (item
	      &optional (alist *schema*)
		(root (or (when (boundp '*schema*) *schema*) alist))
	      &aux (ref (assoc-field :|$ref| alist)))
  "Looks up the value associated with `item` in `alist` (defaults to schema currently being processed), follows (and automatically fetches and parses) `$ref`s as needed"
  (assoc-field item
	       (if (null ref)
		   alist
		   (resolve-ref ref root))))

(defun build-url (proto host port base-path endpoint
		  &aux (url (puri:uri "")))
  (setf (puri:uri-scheme url) (intern proto 'keyword))
  (setf (puri:uri-host url) host)
  (when port
    (setf (puri:uri-port url) port))
  ; FIXME pretty sure this isn't the proper way to concatenate the paths
  (setf (puri:uri-parsed-path url)
	(remove-if
	 (lambda (x)
	   (when (typep x 'sequence)
	     (zerop (length x))))
	 (append
	  '(:absolute)
	  (cdr
	   (puri:uri-parsed-path
	    (puri:parse-uri base-path)))
	  (cdr
	   (puri:uri-parsed-path
	    (puri:parse-uri endpoint))))))
  (puri:render-uri url nil))

;;; generator code

; FIXME barely readable mess
(defun function-for-op (op &aux required optional assistance-code)
  (with-gensyms (url query-params headers body uses-form proto host port base-path
                     endpoint response-string status-code response-headers default-headers)
    (labels ((parse-parameter (param)
	       (let* ((name (field :|name| param))
		      (symbolic-name (lisp-symbol name))
		      (supplied-p (symb symbolic-name '-supplied-p))
		      (is-required (field :|required| param))
		      (in (field :|in| param)))
		 (if is-required
		     (push (if *required-as-keyword*
			       `(,symbolic-name (error "~a is required." ',symbolic-name))
			       symbolic-name)
			   required)
		     (push `(,symbolic-name nil ,supplied-p) optional))
		 (push
		  `(if ,(if is-required t supplied-p)
		       ,(switch (in :test #'string=)
			  ("path"
			   `(setf ,endpoint
                                  (cl-ppcre:regex-replace ,(format nil "{~a}" name)
                                                          ,endpoint (mkstr ,symbolic-name))))
			  ("query"
			   `(push (cons ,name (mkstr ,symbolic-name))
                                  ,query-params))
			  ("header"
			   `(push (cons ,name (mkstr ,symbolic-name))
				  ,headers))
			  ("body"
			   `(setf ,body
				  (concatenate 'string ,body
					       (json:encode-json-to-string ,symbolic-name))))
			  ("formData"
			   `(progn
			      (setf ,uses-form t)
			      (push (cons ,name (mkstr ,symbolic-name))
				    ,query-params)))
			  (otherwise
			   (warn "Don't know how to handle parameters in ~a." in))))
		  assistance-code))))
      (mapcar #'parse-parameter (append *path-params*
					(field :|parameters| (cdr op))))
      (unless (or *required-as-keyword* (zerop (length optional)))
	(push '&key optional))
      (when *required-as-keyword*
	(push '&key required)) ; required comes first so applies to optional as well
      ;; maybe split this into many functions?
      `(progn (defun ,(lisp-symbol (field :|operationId| (cdr op)))
                  ,(append required optional
                    `(&aux
                        ,@(macrolet ((replaceable (x)
                                       ``(if (boundp ',',x) ,',x ,,x)))
                            `((,proto ,(replaceable *proto*))
                              (,host ,(replaceable *host*))
                              (,port ,(replaceable *port*))
                              (,base-path ,(replaceable *base-path*))
                              (,default-headers ,(replaceable *default-headers*))))
                        (,endpoint ,*endpoint*)
                        ,headers ,query-params ,body ,uses-form))
                ,(field :|summary| (cdr op))
                ,@assistance-code
                (let ((,url (build-url ,proto ,host ,port ,base-path ,endpoint))
                      (drakma:*text-content-types* '(("text" . nil)
                                                     (nil . "json"))))
                  ;; FIXME unused variable `status-code'
                  (multiple-value-setq (,response-string ,status-code ,response-headers)
                    (apply #'drakma:http-request ,url
                           :method ,(intern (string-upcase
                                             (string (car op)))
                                            'keyword)
                           :parameters ,query-params
                           :additional-headers (merge-alist ,default-headers
                                                            ,headers)
                           :form-data ,uses-form
                           :content-type ,*content-type*
                           :content ,body
                           :accept ,*accept-header*
                           juniper:*drakma-extra-args*))
                  (if (not (zerop (length ,response-string)))
                      (when (string= (nth-value 1 (drakma:get-content-type ,response-headers))
                                     "json")
                        (json:decode-json-from-string ,response-string))
                      ,response-string)))
              ,@(when *export* `((export ',(lisp-symbol (field :|operationId| (cdr op))))))))))

(defun swagger-path-bindings (path &aux (name (car path)) (ops (cdr path)))
  (let* ((*endpoint* (string name))
	 (*path-params* (field :|parameters| ops)))
    `(progn
       ,@(mapcar #'function-for-op ops))))

(defun swagger-bindings ()
  `(progn
     ,@(mapcar #'swagger-path-bindings (field :|paths|))))

(defun bindings-from-stream (stream &key proto host base-path accept-header required-as-keyword content-type default-headers export)
  (let* ((cl-json:*json-identifier-name-to-lisp* (lambda (x) x)) ; avoid mangling names by accident
	 (*schema* (json:decode-json stream))

	 (version (or (field :|swagger|)
		      (field :|openapi|)
		      (error "Cannot find version field in schema.")))

         ;; FIXME we only use the first protocol presented
	 (*proto* (or proto (car (field :|schemes|))
		      (error "Cannot find protocol in schema.")))
	 (*host* (or host (field :|host|)
		     (error "Cannot find host in schema.")))
	 (*base-path* (or base-path (field :|basePath|) "/"))
	 (*accept-header* (or accept-header "application/json"))
         (*content-type* (or content-type "application/json"))
	 (*required-as-keyword* required-as-keyword)
	 (*port*)
         (*default-headers* default-headers)
         (*export* export))
    (switch (version :test #'string=)
      ("2.0" (swagger-bindings))
      (otherwise
       (error "Unsupported swagger/OpenAPI version ~a." version)))))

;;;

(defmacro defsource (name args &body body
		     &aux (gen-opts '(proto host base-path required-as-keyword content-type default-headers export)))
  (with-gensyms (dispatched options return)
    (setf args (cons name args))
    `(defmacro ,(symb 'bindings-from- name) (,@args &rest ,options
					     &key ,@gen-opts
					     &aux ,dispatched ,return)
       (declare (ignore ,@gen-opts))
       (labels ((dispatch-bindings (stream)
		  (when ,dispatched
		    (error "Trying to dispatch bindings more than once, this is a bug on Juniper."))
		  (setf ,dispatched t)
		  (apply #'bindings-from-stream stream ,options)))
	 (setf ,return (progn ,@body))
	 (unless ,dispatched
	   (error "Source never dispatched stream to generator, this is a bug on Juniper."))
	 ,return))))

(defsource file ()
  "Generates bindings from local file at `file`"
  (with-open-file (stream (eval file))
    (dispatch-bindings stream)))

(defsource json ()
  "Generates bindings from a literal JSON string"
  (with-input-from-string (stream (eval json))
    (dispatch-bindings stream)))

(defsource url ()
  "Generates bindings for remote schema at `url`"
   ; FIXME there has to be a better way to do this
  (with-input-from-string (stream (flexi-streams:octets-to-string
				   (drakma:http-request (eval url))))
    (dispatch-bindings stream)))
