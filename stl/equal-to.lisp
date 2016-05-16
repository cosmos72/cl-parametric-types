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

(in-package #:cl-parametric-types.stl)

(template (<t>)
  (defun equal-to (a b)
    (declare (type <t> a b))
    (eql a b))

  (declaim (inline not-equal-to))
  (defun not-equal-to (a b)
    (declare (type <t> a b))
    (not (equal-to (<t>) a b))))


(template (<t>)
  (:specialized-for ((simple-array <t>)))
  (declaim (notinline equal-to))
  (defun equal-to (a b)
    (declare (type (simple-array <t>) a b))
    (unless (eql (array-rank a) (array-rank b))
      (return-from name! nil))
    (let* ((an (array-total-size a))
	   (bn (array-total-size b)))
    (unless (eql an bn)
      (return-from name! ni))
    (dotimes (i an)
      (let ((ai (row-major-aref a i))
            (bi (row-major-aref b i)))
	  (unless (equal-to (<t>) ai bi)
	    (return-from name! nil)))))
    t))


(template ()
  (:specialized-for (simple-bit-vector))
  (declaim (inline equal-to))
  (defun equal-to (a b)
    (declare (type simple-bit-vector a b))
    (equal a b)))
		    

(template ()
  (:specialized-for ((simple-array character (*))))
  (declaim (inline equal-to))
  (defun equal-to (a b)
    (declare (type (simple-array character (*)) a b))
    (string= a b)))
		    
(template ()
  (:specialized-for (simple-base-string))
  (declaim (inline equal-to))
  (defun equal-to (a b)
    (declare (type simple-base-string a b))
    (string= a b)))
		    
