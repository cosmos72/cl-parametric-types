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

(defun define-struct-accessor (struct-name template-args template-types slot-description)
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


(defun define-struct-accessors (name template-args template-types slot-descriptions)
  (declare (type symbol name)
	   (type list template-args template-types slot-descriptions))
  (loop :for slot-description :in slot-descriptions
     :collect
     (define-struct-accessor name template-args template-types slot-description)))

(defun define-struct-make&copy (struct-name template-args template-types)
  (declare (type symbol struct-name)
	   (type list template-args template-types))
  (let* ((package (symbol-package struct-name))
         (struct-name-s (symbol-name struct-name))
         (make-name-s (concatenate 'string (symbol-name 'make) "-" struct-name-s))
         (make-name (intern make-name-s package))
         (copy-name-s (concatenate 'string (symbol-name 'copy) "-" struct-name-s))
         (copy-name (intern copy-name-s package))
         (args (gensym (symbol-name 'args)))
         (concrete-function (gensym (symbol-name 'concrete-function))))
    `((defmacro ,make-name ((,@template-args) &rest ,args)
       (let ((,concrete-function (instantiate* 'template-function 'make
                                               `((,',struct-name ,,@template-types)))))
         `(,,concrete-function ,@,args)))
      (defmacro ,copy-name ((,@template-args) &rest ,args)
        (let ((,concrete-function (instantiate* 'template-function 'copy
                                                `((,',struct-name ,,@template-types)))))
          `(,,concrete-function ,@,args))))))
        

(defmacro make ((&rest template-args) &rest function-args)
  (let ((concrete-function (instantiate* 'template-function 'make `(,@template-args))))
    `(,concrete-function ,@function-args)))

(defmacro copy ((&rest template-args) &rest function-args)
  (let ((concrete-function (instantiate* 'template-function 'copy `(,@template-args))))
    `(,concrete-function ,@function-args)))
