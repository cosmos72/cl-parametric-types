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

#|

## Syntax:

`(equal-to (<t>) x y) => generalized-boolean`

## Arguments and values:

`<t>`      a type  
x:         an object of type `<t>` or some subtype  
y:         an object of type `<t>` or some subtype  
generalized-boolean: a generalized-boolean, i.e. NIL represents false
           and all other objects represent true.

## Description:

EQUAL-TO returns true, i.e. non-nil, if the objects X and Y are equal,
otherwise returns NIL. Commonly used together with [HASH](hash.md)
to implement [hash associative containers](hash-set-or-map.md)

The algorithm underlying EQUAL-TO is intended to compare only the *contents*
of the two objects X and Y, i.e. it is expected to return true
if the two objects are of the same type and contains the same data.

It satisfies the classical properties of equivalence relations:

1. reflective: an object is EQUAL-TO itself.

   Formally, `(EQUAL-TO <T> X X)` is always true

2. symmetric: the arguments order is irrelevant.

   Formally, `(EQUAL-TO <T> X Y)` implies `(EQUAL-TO <T> Y X)`

3. transitive: if `(EQUAL-TO <T> X Y)` and `(EQUAL-TO <T> Y Z)` are both true,
   then `(EQUAL-TO <T> X Y)` is true too.

It must also satisfy the additional property:

4. consistency between HASH and EQUAL-TO: equal objects have equal hash code.

   Formally, `(EQUAL-TO <T> X Y)` implies `(= (HASH <T> X) (HASH <T> Y))`

## Notes:

Definining partial template specializations on EQUAL-TO to compare
the contents of user-defined types is up to the programmer - the general
template simply calls EQL, i.e. an object is EQUAL-TO only to itself.

In order to maintain the consistency between EQUAL-TO and HASH, if you define
a partial template specialization for the template-function EQUAL-TO,
you *should* also define the corresponding specializazion for [HASH](hash.md).

## Side effects:

none

## Exceptional situations:

An implementation can, but is not required to, signal a TYPE-ERROR if the objects
passed to `(EQUAL-TO (<T>) ...)` are not of type `<T>`.

## Specializations:

CL-PARAMETRIC-TYPES.STL predefines the following specializations:

        (EQUAL-TO (<T>))
        (EQUAL-TO ((BIVECTOR <T>)))
        (EQUAL-TO ((ITERATOR (BIVECTOR <T>))))
        (EQUAL-TO ((PAIR <T1> <T2>)))
        (EQUAL-TO ((SIMPLE-ARRAY <T>)))
        (EQUAL-TO (SIMPLE-BIT-VECTOR))
        (EQUAL-TO (SIMPLE-BASE-STRING))
        (EQUAL-TO (SIMPLE-CHAR-STRING)) ;; i.e. (EQUAL-TO ((SIMPLE-ARRAY CHARACTER (*))))
        (EQUAL-TO ((TRIPLE <T1> <T2> <T3>)))

## Examples:

To be written...

|#


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
		    
