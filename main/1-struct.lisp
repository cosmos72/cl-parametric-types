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


(declaim (inline struct-name-and-options->name))
(defun struct-name-and-options->name (name-and-options)
  (first-atom name-and-options))


(defun struct-name-and-options->option (name-and-options keyword)
  (declare (type (or symbol cons) name-and-options)
	   (type keyword keyword))
  (when (consp name-and-options)
    (block nil
      (dolist (option (rest name-and-options))
	(when (eq keyword (first-atom option))
	  (return option))))))


(defun struct-name-and-options->superclass-name (name-and-options)
  (declare (type (or symbol cons) name-and-options))
  (let ((option (struct-name-and-options->option name-and-options :include)))
    (when (consp option)
      (second option))))
    

(defun define-struct-accessor (struct-name template-args template-types
			       slot-description)
  (declare (type symbol struct-name))
  (let* ((package (symbol-package struct-name))
         (struct-name-s (symbol-name struct-name))
         (slot-name (first-atom slot-description))
	 (slot-name-s (symbol-name slot-name))
         (reader-name-s (concatenate 'string struct-name-s "-" slot-name-s))
         (reader-name (intern reader-name-s package))
         (instance (gensym (symbol-name 'instance)))
         (concrete-function (gensym (symbol-name 'concrete-function))))
    `(defmacro ,reader-name ((,@template-args) ,instance)
       (let ((,concrete-function (instantiate* 'template-accessor ',slot-name
                                               `((,',struct-name ,,@template-types)))))
         `(,,concrete-function ,,instance)))))


(defun define-struct-direct-accessors (name template-args template-types
				       slot-descriptions)
  (declare (type symbol name)
	   (type list template-args template-types slot-descriptions))
  ;; structure type predicate (foo-p ...) behaves exactly like a slot named P
  (loop :for slot-description :in (cons 'p slot-descriptions)
     :collect
     (define-struct-accessor name template-args template-types slot-description)))
     

(defun slot-definition->name (slot-definition)
  (typecase slot-definition
    (symbol    slot-definition)
    (cons      (first slot-definition))
    (otherwise (closer-mop:slot-definition-name slot-definition))))


;; this works only for non-template superclasses!
(defun define-struct-accessors (name-and-options template-args template-types
				slot-descriptions)
  (declare (type (or symbol cons) name-and-options)
	   (type list template-args template-types slot-descriptions))
  (let ((name (struct-name-and-options->name name-and-options))
	(superclass-name (struct-name-and-options->superclass-name name-and-options)))
    (when superclass-name
      (let ((superclass-slots (closer-mop:class-slots (find-class superclass-name))))
	(dolist (superclass-slot (reverse superclass-slots))
	  (push (slot-definition->name superclass-slot) slot-descriptions))))
    (define-struct-direct-accessors name template-args template-types
				    slot-descriptions)))


(defmacro make ((&rest template-args) &rest function-args)
  (let ((concrete-function (instantiate* 'template-constructor 'make `(,@template-args))))
    `(,concrete-function ,@function-args)))


(defmacro copy ((&rest template-args) &rest function-args)
  (let ((concrete-function (instantiate* 'template-constructor 'copy `(,@template-args))))
    `(,concrete-function ,@function-args)))


(defun define-struct-make&copy (struct-name template-args template-types)
  (declare (type symbol struct-name)
	   (type list template-args template-types))
  (let* ((package (symbol-package struct-name))
         (struct-name-s (symbol-name struct-name))
         (make-name-s (concatenate 'string (symbol-name 'make) "-" struct-name-s))
         (make-name (intern make-name-s package))
         (copy-name-s (concatenate 'string (symbol-name 'copy) "-" struct-name-s))
         (copy-name (intern copy-name-s package))
         (function-args (gensym (symbol-name 'function-args))))
    `((defmacro ,make-name ((,@template-args) &rest ,function-args)
        `(make ((,',struct-name ,,@template-types)) ,@,function-args))
      (defmacro ,copy-name ((,@template-args) &rest ,function-args)
        `(copy ((,',struct-name ,,@template-types)) ,@,function-args)))))
