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
  (let ((new-declaims nil)
	(cons-name `(,name)))
    (dolist (form declaims)
      (destructuring-bind (declaim-name &rest specifiers) form
	(unless (eq 'declaim declaim-name)
	  (error "CL-PARAMETRIC-TYPES: unsupported declaim
  for template-function ~S: expecting (DECLAIM ...)
  found ~S" name form))

	(dolist (specifier specifiers)
	  (destructuring-bind (inline-or-ftype &rest args) specifier
	    (ecase inline-or-ftype
	      ((inline)
	       (if (equal cons-name args)
		   (push '(inline name!) new-declaims)
		   (error "CL-PARAMETRIC-TYPES: unsupported (DECLAIM (INLINE ...))
  before template-function ~S:
  expecting (DECLAIM (INLINE ~S))
  found (DECLAIM ~S)" name name specifier)))

	      ((notinline)
	       (if (equal cons-name args)
		   (push '(notinline name!) new-declaims)
		   (error "CL-PARAMETRIC-TYPES: unsupported (DECLAIM (NOTINLINE ...))
  before template-function ~S:
  expecting (DECLAIM (NOTINLINE ~S))
  found (DECLAIM ~S)" name name specifier)))

	      ((ftype)
	       (if (equal cons-name (rest args))
		   (push `(ftype ,(first args) name!) new-declaims)
		   (error "CL-PARAMETRIC-TYPES: unsupported (DECLAIM (FTYPE ...))
  before template-function ~S:
  expecting (DECLAIM (FTYPE (...) ~S))
  found (DECLAIM ~S)" name name specifier))))))))
    `((declaim ,@(nreverse new-declaims)))))


(defun parse-type-declaims (name declaims)
  (declare (type list declaims))
  (when declaims
    (error "CL-PARAMETRIC-TYPES: error defining template-type ~S:
  declaims are not (yet) supported for template-types,
  found ~S" name declaims)))


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


