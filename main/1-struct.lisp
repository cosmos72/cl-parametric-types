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
			       slot-description)
  (declare (type symbol struct-name))
  (let* ((package (symbol-package struct-name))
         (struct-name-s (symbol-name struct-name))
         (slot-name (first-atom slot-description))
	 (slot-name-s (symbol-name slot-name))
         (reader-name-s (concatenate 'string struct-name-s "-" slot-name-s))
         (reader-name (intern reader-name-s package))
         (instance (gensym (symbol-name 'instance))))
    `(defmacro ,reader-name ((,@template-args) ,instance)
       `(,(instantiate 'template-accessor ',slot-name `((,',struct-name ,,@template-types)))
          ,,instance))))


(defun define-struct-direct-accessors (name slot-descriptions)
  "define accessor macros for specified slots of template-struct"
  (declare (type symbol name)
	   (type list slot-descriptions))
  (let* ((template-args (get-definition-template-args 'template-type name))
         (template-types (lambda-list->args template-args)))
    (loop :for slot-description :in slot-descriptions
       :collect
       (define-struct-accessor name template-args template-types slot-description))))
     

(defun slot-definition->name (slot-definition)
  (typecase slot-definition
    (symbol    slot-definition)
    (cons      (first slot-definition))
    (otherwise (closer-mop:slot-definition-name slot-definition))))


(defun define-struct-inherited-accessors (name superclass-name)
  (let ((superclass-name (typexpand superclass-name)))
    (when superclass-name
      (if (not (symbolp superclass-name))
          (log.debug "~&; template-struct ~S has superclass ~S, expecting a symbol instead...
; inherited accessors will *not* be defined.~&" name superclass-name)

          (let* ((superclass-slots (closer-mop:class-slots (find-class superclass-name)))
                 (template-args  (get-definition-template-args 'template-type name))
                 (template-types (lambda-list->args template-args)))
            (loop :for superclass-slot :in superclass-slots
               :for superclass-slot-name = (slot-definition->name superclass-slot)
               :collect (define-struct-accessor name template-args template-types 
                                                superclass-slot-name)))))))
          



(defmacro define-struct-inherited-accessors! (name superclass-name)
  `(progn
     ,@(define-struct-inherited-accessors name superclass-name)))


(defun define-struct-accessors (name superclass-name
                                slot-descriptions &key (define-struct-predicate t))
  "define accessor macros for template-struct slots and superclass slots.
SPECIAL CASE: if superclass is a template involving TEMPLATE-TYPES,
then accessors for superclass slots are NOT defined."
  (declare (type symbol name)
           (type (or symbol cons) superclass-name)
	   (type list slot-descriptions))
  ;; structure type predicate (foo-p ...) behaves exactly like a slot named P
  (when define-struct-predicate
    (push 'p slot-descriptions))
  ;; if superclass is a template that depends on TEMPLATE-TYPES,
  ;; we cannot know superclass accessors now:
  ;; they will be available only after instantiating the struct itself...
  (let ((inherited-accessors
         (when superclass-name
           (let ((template-types (get-definition-template-types 'template-type name)))
             (unless (type-is-template-of superclass-name template-types)
               (define-struct-inherited-accessors name superclass-name)))))
        (direct-accessors
         (define-struct-direct-accessors name slot-descriptions)))
    (append inherited-accessors direct-accessors)))


(defmacro define-struct-accessors! (name superclass-name
                                    slot-descriptions &key (define-struct-predicate t))
  `(progn
     ,@(define-struct-accessors name superclass-name slot-descriptions
                                :define-struct-predicate define-struct-predicate)))

(defmacro make ((&rest template-args) &rest function-args)
  (let ((concrete-function (instantiate 'template-constructor 'make `(,@template-args))))
    `(,concrete-function ,@function-args)))


(defmacro copy ((&rest template-args) &rest function-args)
  (let ((concrete-function (instantiate 'template-constructor 'copy `(,@template-args))))
    `(,concrete-function ,@function-args)))


(defun define-struct-make&copy (struct-name)
  (declare (type symbol struct-name))
  (let* ((package (symbol-package struct-name))
         (struct-name-s (symbol-name struct-name))
         (make-name-s (concatenate 'string (symbol-name 'make) "-" struct-name-s))
         (make-name (intern make-name-s package))
         (copy-name-s (concatenate 'string (symbol-name 'copy) "-" struct-name-s))
         (copy-name (intern copy-name-s package))
         (template-args (get-definition-template-args 'template-type struct-name))
         (template-types (lambda-list->args template-args))
         (function-args (gensym (symbol-name 'function-args))))
    `((defmacro ,make-name ((,@template-args) &rest ,function-args)
        `(make ((,',struct-name ,,@template-types)) ,@,function-args))
      (defmacro ,copy-name ((,@template-args) &rest ,function-args)
        `(copy ((,',struct-name ,,@template-types)) ,@,function-args)))))

(defmacro define-struct-make&copy! (struct-name)
  `(progn
     ,@(define-struct-make&copy struct-name)))
