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

This file does XXX.


|#

(in-package #:cl-parametric-types)

(defmacro template-function
    ((&rest template-args)
	(&key declaims specialized-for)
	   (defun name lambda-list &body body))
  
  (let* ((template-types (lambda-list->args template-args))
	 (function-args  (lambda-list->args lambda-list)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get-definition 'template-function ',name ',specialized-for)
	       '(template-function ,template-args
		 ,@(parse-function-declaims name declaims)
		 (,defun ,name ,lambda-list
		   ,@body))))
       ,(if specialized-for
	    `',name
	    ;; rely on DEFMACRO to parse the TEMPLATE-ARGS lambda list
	    `(defmacro ,name (,template-args ,@lambda-list)
	       `(,(instantiate 'template-function ',name `(,,@template-types))
		  ,,@function-args))))))


(defmacro template-struct
    ((&rest template-args)
	(&key declaims specialized-for)
	(defstruct name-and-options &rest slot-descriptions))

  (let* ((template-types   (lambda-list->args template-args))
	 (name-and-options (parse-struct-name-and-options  name-and-options))
	 (name             (struct-name-and-options->name  name-and-options))
	 (superclass-name  (struct-name-and-options->superclass-name name-and-options))
         (template-superclass-name? (or specialized-for
					(type-is-template-of superclass-name template-types)))
	 (template-superclass-name    (if template-superclass-name? superclass-name nil))
	 (nontemplate-superclass-name (if template-superclass-name? nil superclass-name))
	 (template-slot-names    (when specialized-for (mapcar #'first-atom slot-descriptions)))
	 (nontemplate-slot-names nil))
    (unless specialized-for
      (dolist (slot-description slot-descriptions)
	(let ((slot-name (first-atom slot-description)))
	  (if (member slot-name template-types)
	      (push slot-name template-slot-names)
	      (push slot-name nontemplate-slot-names))))
      (setf template-slot-names (nreverse template-slot-names)
	    nontemplate-slot-names (nreverse nontemplate-slot-names)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get-definition 'template-type ',name ',specialized-for)
	       '(template-struct ,template-args
		 ;; set *package*
		 ;; reason: it may have changed in the meantime,
		 ;; and (defstruct A::FOO ...) defines functions in *package*, not in A!
		 (in-package ,(package-name *package*))
		 ,@(parse-struct-declaims name declaims)
		 (,defstruct ,name-and-options ,@slot-descriptions)
                 ,@(when (or specialized-for template-superclass-name template-slot-names)
                         `((define-struct-accessors! (quote! ,name) (quote! ,specialized-for)
			     ,template-superclass-name
			     ,template-slot-names)
			   ',name)))))
       ,@(if specialized-for
	     `',name
	     `((define-struct-make&copy! ,name)
	       (define-struct-accessors! ,name nil
		   ;; if superclass is a template that depends on TEMPLATE-TYPES,
		   ;; we will know superclass slots only after instantiating
		   ;; the struct itself...
		   ,nontemplate-superclass-name
		 ;; structure type predicate (foo-p ...) behaves exactly like a slot named P
		 (p ,@nontemplate-slot-names))
	       ;; rely on DEFTYPE to parse the TEMPLATE-ARGS lambda list
	       (deftype ,name ,template-args
		 (instantiate 'template-type ',name `(,,@template-types))))))))


(defmacro template-class
    ((&rest template-args)
	(&key declaims specialized-for)
	(defclass name direct-superclasses slot-descriptions
	  &rest options))

  (let ((template-types (lambda-list->args template-args)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get-definition 'template-type ',name ',specialized-for)
	       '(template-class ,template-args
		 ,@(parse-class-declaims name declaims)
		 (,defclass ,name ,direct-superclasses
		   ,slot-descriptions
		   ,@options))))
       ;; rely on DEFTYPE to parse the TEMPLATE-ARGS lambda list
       (deftype ,name ,template-args
	 (instantiate 'template-type ',name `(,,@template-types))))))


(defmacro template-type
    ((&rest template-args) (&rest options)
	(defclass-defstruct name &body body))
  (ecase defclass-defstruct
    ((defclass)
     `(template-class ,template-args ,options
	(defclass ,name ,@body)))
    ((defstruct)
     `(template-struct ,template-args ,options
	(defstruct ,name ,@body)))))


(defmacro template*
    ((&rest template-args) (&rest options)
     &body template-definitions)
  (let ((forms nil)
	(declaims nil))
    (dolist (definition template-definitions)
      (destructuring-bind (macro name &body body) definition
	(case macro
	  ((declaim) (push definition declaims))
	  (otherwise
	   (setf declaims (nreverse declaims))
	   (push
	    (ecase macro
	      ((defclass)
	       `(template-class ,template-args (:declaims ,declaims ,@options)
				(defclass ,name ,@body)))
	      ((defstruct)
	       `(template-struct ,template-args (:declaims ,declaims ,@options)
				 (defstruct ,name ,@body)))
	      ((defun)
	       `(template-function ,template-args (:declaims ,declaims ,@options)
				   (defun ,name ,@body))))
	    forms)
	   (setf declaims nil)))))
    (setf forms (nreverse forms))
    (if (rest forms)
        `(progn ,@forms)
        (first forms))))


(defmacro template
    ((&rest template-args)
     &body template-definitions)
  (let ((options))
    (loop :for form = (first template-definitions)
       :while (and (consp form) (keywordp (first form)))
       :do
       (dolist (option form)
         (push option options))
       (pop template-definitions))
    `(template* ,template-args ,(nreverse options)
       ,@template-definitions)))
