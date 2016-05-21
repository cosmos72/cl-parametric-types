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

(declaim (inline combine-hash))
(defun combine-hash (h1 h2)
  (declare (type ufixnum h1 h2))
  (the ufixnum
       (logand
        (+ h2 (logand (* 31 h1) most-positive-fixnum))
        most-positive-fixnum)))


(defmacro combine-hashes (&rest hs)
  (cond
    ((null hs)         1)
    ((null (cdr hs))  (first hs))
    ((null (cddr hs)) `(combine-hash ,(first hs) ,(second hs)))
    (t                `(combine-hash ,(first hs) (combine-hashes ,@(rest hs))))))


(template (<t>)
  (defun hash (object)
    "Return the hash code of an object, which is non-negative fixnum.
If you define a specialization for the template-function EQUAL-TO on a type <T>,
you should also specialize HASH on the same <T>."
    (declare (type <t> object))
    (sxhash object)))


(template (<t>)
  (:specialized-for ((simple-array <t>)))
  (declaim (notinline hash))
  (defun hash (a)
    (declare (type (simple-array <t>) a))
    (let* ((ar (array-rank a))
	   (an (array-total-size a))
	   (h  (combine-hashes (sxhash '(simple-array <t>))
			       (hash (fixnum) ar)
			       (hash (fixnum) an))))
      (declare (type ufixnum h))
      (dotimes (i an)
        (setf h (combine-hash h (hash (<t>) (row-major-aref a i)))))
      h)))


(template ()
  (:specialized-for (simple-bit-vector))
  (declaim (notinline hash))
  (defun hash (a)
    (declare (type simple-bit-vector a))
    (combine-hash (sxhash 'simple-bit-vector) (sxhash a))))
		    

(template ()
  (:specialized-for (simple-char-string))
  (declaim (notinline hash))
  (defun hash (a)
    (declare (type simple-char-string a))
    (combine-hash (sxhash 'simple-char-string) (sxhash a))))


(template ()
  (:specialized-for (simple-base-string))
  (declaim (notinline hash))
  (defun hash (a)
    (declare (type simple-base-string a))
    (combine-hash (sxhash 'simple-base-string) (sxhash a))))
		    
