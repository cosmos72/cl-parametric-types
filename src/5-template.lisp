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


(defmacro template-declaim
    ((&rest template-args)
	(declaim &rest declaration-specifiers)))


(defmacro template-function
    ((&rest template-args)
	(defun name lambda-list &body body))
  
  (let* ((template-types (lambda-list->args template-args))
	 (function-args (lambda-list->args lambda-list)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get-definition 'template-function ',name)
	       '(template-function ,template-types
		 (,defun ,name ,lambda-list
		   ,@body))))
       ;; rely on DEFMACRO to parse the TEMPLATE-ARGS lambda list
       (defmacro ,name (,template-args ,@lambda-list)
	 (let ((concrete-function (instantiate* 'template-function ',name `(,,@template-types))))
	   `(,concrete-function ,,@function-args))))))


(defmacro template-struct
    ((&rest template-args)
	(defstruct name-and-options &rest slot-descriptions))

  (let ((name (first-atom name-and-options))
	(template-types (lambda-list->args template-args)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get-definition 'template-type ',name)
	       '(template-struct ,template-types
		 (,defstruct ,name-and-options
		   ,@slot-descriptions))))
       ,@(define-struct-make&copy name template-args template-types)
       ,@(define-struct-accessors name template-args template-types slot-descriptions)
       ;; rely on DEFTYPE to parse the TEMPLATE-ARGS lambda list
       (deftype ,name ,template-args
	 (instantiate* 'template-type ',name `(,,@template-types))))))


(defmacro template-class
    ((&rest template-args)
	(defclass name direct-superclasses slot-descriptions
	  &rest options))

  (let ((template-types (lambda-list->args template-args)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get-definition 'template-type ',name)
	       '(template-class ,template-types
		 (,defclass ,name ,direct-superclasses
		   ,slot-descriptions
		   ,@options))))
       ;; rely on DEFTYPE to parse the TEMPLATE-ARGS lambda list
       (deftype ,name ,template-args
	 (instantiate* 'template-type ',name `(,,@template-types))))))


(defmacro template-type
    ((&rest template-args)
	(defclass-defstruct name &body body))
  (ecase defclass-defstruct
    ((defclass)
     `(template-class ,template-args
	(defclass ,name ,@body)))
    ((defstruct)
     `(template-struct ,template-args
	(defstruct ,name ,@body)))))

(defmacro template*
    ((&rest template-args) (&rest options)
     &body template-definitions)
  (declare (ignore options))
  (let ((forms
         (loop :for definition :in template-definitions
            :collect
            (destructuring-bind (defclass-defstruct-or-defun name &body body) definition
              (ecase defclass-defstruct-or-defun
                ((defclass)
                 `(template-class ,template-args
                                  (defclass ,name ,@body)))
                ((defstruct)
                 `(template-struct ,template-args
                                   (defstruct ,name ,@body)))
                ((defun)
                 `(template-function ,template-args
                                     (defun ,name ,@body))))))))
    (if (rest forms)
        `(progn ,@forms)
        (first forms))))


(defmacro template
    ((&rest template-args)
     &body template-definitions)
  `(template* ,template-args ()
     ,@template-definitions))

