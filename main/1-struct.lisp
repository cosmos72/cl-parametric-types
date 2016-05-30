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


(defun type-is-template-of (name template-types)
  (declare (type (or symbol cons) name)
           (type list template-types))
  (when (tree-find name template-types :test
                   (lambda (item template-types) (member item template-types)))
    t))


(defun define-struct-accessor (struct-name template-args template-types
			       slot-name)
  (declare (type symbol struct-name slot-name)
	   (type list template-args template-types))
  (let* ((package *package*)
         (struct-name-s (symbol-name struct-name))
	 (slot-name-s (symbol-name slot-name))
         (reader-name-s (concatenate 'string struct-name-s "-" slot-name-s))
         (reader-name (intern reader-name-s package))
         (instance (gensym (symbol-name 'instance))))
    `(defmacro ,reader-name ((,@template-args) ,instance)
       `(,(instantiate 'template-accessor ',slot-name `((,',struct-name ,,@template-types)))
          ,,instance))))


(defun define-struct-direct-accessors (name template-args slot-names)
  "define accessor macros for specified slots of template-struct"
  (declare (type symbol name)
	   (type list template-args slot-names))
  (let ((template-types (lambda-list->params template-args)))
    (loop :for slot-name :in slot-names
       :collect
       (define-struct-accessor name template-args template-types slot-name))))
     

(defun slot-definition->name (slot-definition)
  (typecase slot-definition
    (symbol    slot-definition)
    (cons      (first slot-definition))
    (otherwise (closer-mop:slot-definition-name slot-definition))))


(defun define-struct-inherited-accessors (name template-args superclass-name)
  (declare (type symbol name)
	   (type list template-args)
	   (type (or symbol cons) superclass-name))
  (let ((superclass-name (typexpand superclass-name)))
    (when superclass-name
      (if (not (symbolp superclass-name))
          (log.debug "~&; template-struct ~S has superclass ~S, expecting a symbol instead...
; inherited accessors will *not* be defined.~&" name superclass-name)

          (let ((superclass-slots (closer-mop:class-slots (find-class superclass-name)))
		(template-types (lambda-list->params template-args)))
            (loop :for superclass-slot :in superclass-slots
               :for superclass-slot-name = (slot-definition->name superclass-slot)
               :collect (define-struct-accessor name template-args template-types 
                                                superclass-slot-name)))))))
          

(defun define-struct-accessors (name specialized-for superclass-name slot-names)
  "define accessor macros for template-struct slots and superclass slots.
SPECIAL CASE: if superclass is a template involving TEMPLATE-TYPES,
then accessors for superclass slots are NOT defined."
  (declare (type symbol name)
           (type (or symbol cons) superclass-name)
	   (type list specialized-for slot-names))
  ;; if superclass is a template that depends on TEMPLATE-TYPES,
  ;; we cannot know superclass accessors now:
  ;; they will be available only after instantiating the struct itself...
  (let* ((template-args (get-definition-template-args 'template-type name specialized-for))
	 (inherited-accessors
	  (when superclass-name
	    (define-struct-inherited-accessors name template-args superclass-name)))
	 (direct-accessors
	  (when slot-names
	    (define-struct-direct-accessors name template-args slot-names))))
    (append inherited-accessors direct-accessors)))


(defmacro define-struct-accessors! (name specialized-for superclass-name slot-names)
  `(progn
     ,@(define-struct-accessors name specialized-for superclass-name slot-names)))


(defmacro make ((&rest template-args) &rest function-args)
  (let ((concrete-function (instantiate 'template-constructor 'make `(,@template-args))))
    `(,concrete-function ,@function-args)))


(defmacro copy ((&rest template-args) &rest function-args)
  (let ((concrete-function (instantiate 'template-constructor 'copy `(,@template-args))))
    `(,concrete-function ,@function-args)))


(defun define-struct-make&copy (struct-name constructor-prefix copier-prefix)
  (declare (type symbol struct-name))
  (flet ((prefix->name (prefix)
           (when prefix
             (values
              (intern (concatenate 'string (symbol-name prefix) "-" (symbol-name struct-name))
                      *package*)))))
    (let* ((template-args    (get-definition-template-args 'template-type struct-name nil))
           (template-types   (lambda-list->args template-args))
           (template-rest    (lambda-list->rest template-args))
           (function-args    (gensym (symbol-name 'function-args)))
           (constructor-name (prefix->name constructor-prefix))
           (copier-name      (prefix->name copier-prefix))
           (forms))
      (when constructor-name
        (push
         `(defmacro ,constructor-name ((,@template-args) &rest ,function-args)
            ;; (instantiate 'template-type ',struct-name `(,,@template-types))
            `(,(concretize 'template-constructor ',constructor-prefix
                           `((,',struct-name ,,@template-types ,@,@template-rest)))
               ,@,function-args))
         forms))
      (when copier-name
        (push
         `(defmacro ,copier-name ((,@template-args) &rest ,function-args)
            ;; (instantiate 'template-type ',struct-name `(,,@template-types))
            `(,(concretize 'template-constructor ',copier-prefix
                           `((,',struct-name ,,@template-types ,@,@template-rest)))
               ,@,function-args))
         forms))
      (nreverse forms))))

(defmacro define-struct-make&copy! (struct-name constructor-name copier-name)
  `(progn
     ,@(define-struct-make&copy struct-name constructor-name copier-name)))
