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


(in-package #:cl-parametric-types)

(defun define-struct-accessor (name slot-description)
  (declare (type symbol name))
  (let* ((name-s (symbol-name name))
	 (slot-name (first-atom slot-description))
	 (slot-name-s (symbol-name slot-name))
	 (reader-name-s (concatenate 'string
				     name-s "-" slot-name-s))
	 (reader-name (intern reader-name-s (symbol-package name))))
    `(defmethod mangle ((kind (eql 'template-function))
			(name (eql ',reader-name))
			actual-types &key (normalize t))
       (concatenate 'string
		    ,(concatenate 'string reader-name-s "-")
		    (mangle-cons-type actual-types :normalize normalize)))))


(defun define-struct-accessors (name slot-descriptions)
  (declare (type symbol name)
	   (type list slot-descriptions))
  (loop :for slot-description :in slot-descriptions
     :collect
     (define-struct-accessor name slot-description)))

(defmacro make (template-args &rest function-args)
  (let ((concrete-function (instantiate* 'template-function 'make `(,@template-args))))
    `(,concrete-function ,@function-args)))
