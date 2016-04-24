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

(defun parse-function-declaims (name declaims)
  (declare (type list declaims))
  (dolist (form declaims)
    (destructuring-bind (declaim-name &rest specifiers) form
      (unless (eq 'declaim declaim-name)
	(error "CL-PARAMETRIC-TYPES: unsupported declaim
  for template-function ~S: expecting (DECLAIM ...)
  found ~S" name form))

      (dolist (specifier specifiers)
	(destructuring-bind (inline-or-ftype &rest args) specifier
	  (case inline-or-ftype
	    ((inline)
	     (unless (equal `(,name) args)
	       (error "CL-PARAMETRIC-TYPES: unsupported (DECLAIM (INLINE ...))
  before template-function ~S:
  expecting (DECLAIM (INLINE ~S))
  found (DECLAIM ~S)" name name specifier)))

	    ((notinline)
	     (unless (equal `(,name) args)
	       (error "CL-PARAMETRIC-TYPES: unsupported (DECLAIM (NOTINLINE ...))
  before template-function ~S:
  expecting (DECLAIM (NOTINLINE ~S))
  found (DECLAIM ~S)" name name specifier)))

	    ((ftype)
	     (unless (equal `(,name) (rest args))
	       (error "CL-PARAMETRIC-TYPES: unsupported (DECLAIM (FTYPE ...))
  before template-function ~S:
  expecting (DECLAIM (FTYPE (...) ~S))
  found (DECLAIM ~S)" name name specifier))))))))
  declaims)


(defun parse-struct-declaims (name declaims)
  (declare (type list declaims))
  (when declaims
    (error "CL-PARAMETRIC-TYPES: error defining template-struct ~S:
  declaims are not (yet) supported for template-structs,
  found ~S" name declaims)))


(defun parse-struct-name-and-options (name-and-options)
  (declare (type (or symbol cons) name-and-options))
  (etypecase name-and-options
    (symbol name-and-options)
    (cons
     (let ((name (first name-and-options))
	   (opts (rest name-and-options)))
       (dolist (opt opts)
	 (let ((key (first-atom opt)))
	   (case key
	     (:include t)
	     (otherwise
	      (error "CL-PARAMETRIC-TYPES: error defining template-struct ~S:
 DEFSTRUCT option ~S is not yet supported" name key))))))
     name-and-options)))


(defun parse-class-declaims (name declaims)
  (declare (type list declaims))
  (when declaims
    (error "CL-PARAMETRIC-TYPES: error defining template-class ~S:
  declaims are not (yet) supported for template-classs,
  found ~S" name declaims)))


(defmacro template-function
    ((&rest template-args)
	(&key declaims)
	   (defun name lambda-list &body body))
  
  (let* ((template-types (lambda-list->args template-args))
	 (function-args (lambda-list->args lambda-list)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get-definition 'template-function ',name)
	       '(template-function ,template-types
		 ,@(parse-function-declaims name declaims)
		 (,defun ,name ,lambda-list
		   ,@body))))
       ;; rely on DEFMACRO to parse the TEMPLATE-ARGS lambda list
       (defmacro ,name (,template-args ,@lambda-list)
	 (let ((concrete-function (instantiate* 'template-function ',name `(,,@template-types))))
	   `(,concrete-function ,,@function-args))))))


(defmacro template-struct
    ((&rest template-args)
	(&key declaims)
	(defstruct name-and-options &rest slot-descriptions))

  (let ((name (first-atom name-and-options))
	(template-types (lambda-list->args template-args))
	(name-and-options (parse-struct-name-and-options name-and-options)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get-definition 'template-type ',name)
	       '(template-struct ,template-types
		 ,@(parse-struct-declaims name declaims)
		 (,defstruct ,name-and-options
		   ,@slot-descriptions))))
       ,@(define-struct-make&copy name template-args template-types)
       ,@(define-struct-accessors name template-args template-types slot-descriptions)
       ;; rely on DEFTYPE to parse the TEMPLATE-ARGS lambda list
       (deftype ,name ,template-args
	 (instantiate* 'template-type ',name `(,,@template-types))))))


(defmacro template-class
    ((&rest template-args)
	(&key declaims)
	(defclass name direct-superclasses slot-descriptions
	  &rest options))

  (let ((template-types (lambda-list->args template-args)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get-definition 'template-type ',name)
	       '(template-class ,template-types
		 ,@(parse-class-declaims name declaims)
		 (,defclass ,name ,direct-superclasses
		   ,slot-descriptions
		   ,@options))))
       ;; rely on DEFTYPE to parse the TEMPLATE-ARGS lambda list
       (deftype ,name ,template-args
	 (instantiate* 'template-type ',name `(,,@template-types))))))


(defmacro template-type
    ((&rest template-args) (&key declaims)
	(defclass-defstruct name &body body))
  (ecase defclass-defstruct
    ((defclass)
     `(template-class ,template-args (:declaims ,declaims)
	(defclass ,name ,@body)))
    ((defstruct)
     `(template-struct ,template-args (:declaims ,declaims)
	(defstruct ,name ,@body)))))


(defmacro template*
    ((&rest template-args) (&rest options)
     &body template-definitions)
  (declare (ignore options))
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
	       `(template-class ,template-args (:declaims ,declaims)
				(defclass ,name ,@body)))
	      ((defstruct)
	       `(template-struct ,template-args (:declaims ,declaims)
				 (defstruct ,name ,@body)))
	      ((defun)
	       `(template-function ,template-args (:declaims ,declaims)
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
  `(template* ,template-args ()
     ,@template-definitions))

