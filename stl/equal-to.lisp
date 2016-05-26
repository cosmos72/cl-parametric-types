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
  (declaim (inline equal-to))
  (defun equal-to (x y)
    (declare (type <t> x y))
    (eql x y))

  (declaim (inline not-equal-to))
  (defun not-equal-to (x y)
    (declare (type <t> x y))
    (not (equal-to (<t>) x y))))


(template (<t>)
  (:specialized-for ((simple-array <t>)))
  (declaim (notinline equal-to))
  (defun equal-to (x y)
    (declare (type (simple-array <t>) x y))
    (unless (eql (array-rank x) (array-rank y))
      (return-from name! nil))
    (let* ((an (array-total-size x))
	   (bn (array-total-size y)))
    (unless (eql an bn)
      (return-from name! nil))
    (dotimes (i an)
      (let ((xi (row-major-aref x i))
            (yi (row-major-aref y i)))
	  (unless (equal-to (<t>) xi yi)
	    (return-from name! nil)))))
    t))


(template ()
  (:specialized-for (simple-bit-vector))
  (declaim (inline equal-to))
  (defun equal-to (x y)
    (declare (type simple-bit-vector x y))
    (equal x y)))
		    

(template ()
  (:specialized-for (simple-char-string))
  (declaim (inline equal-to))
  (defun equal-to (x y)
    (declare (type simple-char-string x y))
    (string= x y)))


(template ()
  (:specialized-for (simple-base-string))
  (declaim (inline equal-to))
  (defun equal-to (x y)
    (declare (type simple-base-string x y))
    (string= x y)))
		    
