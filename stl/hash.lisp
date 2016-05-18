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

(deftype ufixnum () '(integer 0 #.most-positive-fixnum))

(declaim (inline combine-hash))
(defun combine-hash (h1 h2)
  (declare (type ufixnum h1 h2))
  (the ufixnum
       (logand
        (+ h2 (logand (* 31 h1) most-positive-fixnum))
        most-positive-fixnum)))


(template (<t>)
  (defun hash (a)
    (declare (type <t> a))
    (sxhash a)))


(template (<t>)
  (:specialized-for ((simple-array <t>)))
  (declaim (notinline hash))
  (defun hash (a)
    (declare (type (simple-array <t>) a))
    (let ((h (hash (fixnum) (array-rank a)))
          (an (array-total-size a)))
      (declare (type ufixnum h))
      (setf h (combine-hash h (array-rank a)))
      (dotimes (i an)
        (setf h (combine-hash h (row-major-aref a i))))
      h)))


(template ()
  (:specialized-for (simple-bit-vector))
  (declaim (inline hash))
  (defun hash (a b)
    (declare (type simple-bit-vector a))
    (sxhash a)))
		    

(template ()
  (:specialized-for (simple-char-string))
  (declaim (inline hash))
  (defun hash (a)
    (declare (type simple-char-string a))
    (sxhash a)))


(template ()
  (:specialized-for (simple-base-string))
  (declaim (inline hash))
  (defun hash (a)
    (declare (type simple-base-string a))
    (sxhash a)))
		    
