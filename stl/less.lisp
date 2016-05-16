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
  (defun less (a b)
    (declare (type <t> a b))
    (< a b))

  (declaim (inline less-equal))
  (defun less-equal (a b)
    (declare (type <t> a b))
    (not (less (<t>) b a)))

  (declaim (inline greater))
  (defun greater (a b)
    (declare (type <t> a b))
    (less (<t>) b a))

  (declaim (inline greater-equal))
  (defun greater-equal (a b)
    (declare (type <t> a b))
    (not (less (<t>) a b))))


(template (<t>)
  (:specialized-for ((simple-array <t>)))
  (declaim (notinline less))
  (defun less (a b)
    (declare (type (simple-array <t>) a b))
    (let ((rank-a (array-rank a))
	  (rank-b (array-rank b)))
      (cond
	;; array with smaller rank is "less"
	((< rank-a rank-b) (return-from name! t))
	((> rank-a rank-b) (return-from name! nil))))
    (let* ((an (array-total-size a))
	   (bn (array-total-size b))
	   (n  (min na nb)))
      (dotimes (i n)
	(let ((ai (row-major-aref a i))
	      (bi (row-major-aref b i)))
	  (cond
	    ((less (<t>) ai bi) (return-from name! t))
	    ((less (<t>) bi ai) (return-from name! nil)))))
      ;; common initial elements are equal -> shorter array is "less"
      (< an bn))))


(template ()
  (:specialized-for ((simple-array character (*))))
  (defun less (a b)
    (declare (type (simple-array character (*)) a b))
    (string< a b)))
		    
(template ()
  (:specialized-for (simple-base-string))
  (defun less (a b)
    (declare (type simple-base-string a b))
    (string< a b)))
		    
