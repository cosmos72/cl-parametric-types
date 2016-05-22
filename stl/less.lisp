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

(template (&optional (<t> 'real))
  (declaim (inline less))
  (defun less (x y)
    (declare (type <t> x y))
    (< x y))

  (declaim (inline less-equal))
  (defun less-equal (x y)
    (declare (type <t> x y))
    (not (less (<t>) y x)))

  (declaim (inline greater))
  (defun greater (x y)
    (declare (type <t> x y))
    (less (<t>) y x))

  (declaim (inline greater-equal))
  (defun greater-equal (x y)
    (declare (type <t> x y))
    (not (less (<t>) x y))))


(template (<t>)
  (:specialized-for ((simple-array <t>)))
  (declaim (notinline less))
  (defun less (x y)
    (declare (type (simple-array <t>) x y))
    (let ((rank-x (array-rank x))
	  (rank-y (array-rank y)))
      (cond
	;; array with smaller rank is "less"
	((< rank-x rank-y) (return-from name! t))
	((> rank-x rank-y) (return-from name! nil))))
    (let* ((xn (array-total-size x))
	   (yn (array-total-size y))
	   (n  (min xn yn)))
      (dotimes (i n)
	(let ((xi (row-major-aref x i))
	      (yi (row-major-aref y i)))
	  (cond
	    ((less (<t>) xi yi) (return-from name! t))
	    ((less (<t>) yi xi) (return-from name! nil)))))
      ;; common initial elements are equal -> shorter array is "less"
      (< xn yn))))


(template ()
  (:specialized-for ((simple-array character (*))))
  (defun less (x y)
    (declare (type (simple-array character (*)) x y))
    (string< x y)))
		    
(template ()
  (:specialized-for (simple-base-string))
  (defun less (x y)
    (declare (type simple-base-string x y))
    (string< x y)))
		    
