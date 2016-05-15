;; -*- lisp -*-

;; This file is part of cl-parametric-types.
;; Copyright (c) 2016 Massimiliano Ghilardi
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the Lisp Lesser General Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Lisp Lesser General Public License for more details.

#|

Machinery to instantiate templates: INSTANTIATE function and friends

|#

(in-package #:cl-parametric-types)

(defconstant name!  'name!)
(defconstant quote! 'quote!)

(defun kind-name (kind)
  (string-downcase (symbol-name kind)))


(defmethod instantiate-definition (kind name actual-types)
  (declare (type list actual-types))
  (let ((concrete (concretize kind name actual-types)))
    (multiple-value-bind (definition pattern template-types template-constraints)
	(get-definition kind name actual-types)

      (unless definition
	(error "~A ~S has no definition,
cannot instantiate ~S"
	       (kind-name kind) name (cons name actual-types)))

      (let ((template-params (mapcar #'car template-constraints))
	    (template-values (mapcar #'cdr template-constraints)))
	;; paranoia: in case template-constraints is incomplete...
	(dolist (template-type template-types)
	  (unless (member template-type template-params)
	    (push template-type template-params)
	    (push t             template-values)))
	    
	(log.debug "~&; instantiating ~a ~s
;   as ~s
;   using ~s specialization for ~s~&"
		   (kind-name kind) (cons name actual-types) concrete name pattern)

	(let ((forms (cddr definition)))
	  (values
	   (multi-subst (cons concrete template-values)
			(cons 'name!   template-params)
			(if (rest forms)
			    `(progn ,@forms)
			    (first forms))
			:quote-symbol 'quote! :eval-symbol 'eval!
			:eval-splice-symbol 'eval-splice!)
	   concrete))))))
      

(defmethod instantiate* (kind name actual-types)
  (declare (type list actual-types))
  (multiple-value-bind (to-eval concrete) (instantiate-definition kind name actual-types)
    ;; restore *package* after EVAL.
    ;; reason: template-struct definitions need to set *package*
    (let ((*package* *package*))
      (eval to-eval))
    concrete))


(defmethod instantiate (kind name actual-types)
  (declare (type list actual-types))
  (let ((concrete (concretize kind name actual-types))
	(just-instantiated nil))
    (case kind
      ((template-accessor template-constructor)
       ;; struct accessors and constructors cannot be instantiated manually...
       ;; they should be already instantiated by typexpanding the struct name
       ;; in the call to CONCRETIZE above.
       (fdefinition concrete)) ;; signals error if not found
      (otherwise
       (handler-case
	   (ecase kind
	     (template-type     (find-class concrete))
	     (template-function (fdefinition concrete)))
	 (condition ()
	   (setf concrete (instantiate* kind name actual-types)
		 just-instantiated t)))))
    (values concrete just-instantiated)))
       

(defun instantiate-type (name-and-actual-types)
  (declare (type cons name-and-actual-types))
  (instantiate 'template-type (first name-and-actual-types)
               (rest name-and-actual-types)))

(defun instantiate-function (name actual-types)
  (declare (type symbol name)
           (type list actual-types))
  (instantiate 'template-function name actual-types))

(defun instantiate-accessor (name struct-name-and-actual-types)
  (declare (type symbol name)
           (type cons struct-name-and-actual-types))
  (instantiate 'template-accessor name struct-name-and-actual-types))

(defun instantiate-constructor (name struct-name-and-actual-types)
  (declare (type symbol name)
           (type cons struct-name-and-actual-types))
  (instantiate 'template-constructor name struct-name-and-actual-types))
