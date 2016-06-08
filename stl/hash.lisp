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

`(hash (<t>) object) => hash-code`

## Arguments and values:

`<t>`      a type  
object:    an object of type `<t>` or some subtype  
hash-code: a non-negative fixnum  

## Description:

HASH returns the [hash code](http://en.wikipedia.com/wiki/Hash_function) of an object.
Commonly used together with [EQUAL-TO](equal-to.md)
to implement [hash associative containers](hash-set-or-map.md)

Unlike CL:SXHASH, values returned by HASH on different Lisp images
for the same objects *can* be different - HASH is only guaranteed
to produce consistent values within a *single* Lisp image;
this allows salted hashes that prevent collision DoS attacks.

The algorithm underlying HASH is implementation-dependent,
and provides the following guarantees:

1. consistency between HASH and EQUAL-TO: equal objects have equal hash code.

   Formally: given a type `<T>` and two objects A, B of type `<T>` or some subtype,
   `(EQUAL-TO <T> A B)` implies `(= (HASH <T> A) (HASH <T> B))`

2. invariability: The hash-code for an object is always the same within a single
   Lisp image, provided that the object is not visibly modified with regard to
   the equivalence test EQUAL-TO. This is a logical consequence of item 1 above.

3. discrimination: with very high probability, different objects
   have different hash codes.
  
   Formally: given a type `<T>` and two objects A, B of type `<T>` or some subtype,
   `(NOT (EQUAL-TO <T> A B))` implies that, with a probability approaching
   1/(1+ MOST-POSITIVE-FIXNUM), `(/= (HASH <T> A) (HASH <T> B))`
   
4. uniformity: the implementation should make a good-faith effort to produce
   hash codes that are well distributed between 0 and MOST-POSITIVE-FIXNUM

5. not intended for cryptography: the algorithms used by HASH do *not* attempt
   to be resistant to cryptographic attacks.

   Formally:
   Constructing HASH collisions, i.e. different objects that have the same hash code,
   is probably an easy task.
   Similarly, given a hash code, constructing an object that has such hash code,
   is probably an easy task too.

## Notes:

In order to maintain the consistency between HASH and EQUAL-TO, if you define
a partial template specialization for the template-function [EQUAL-TO](equal-to.md),
you *should* also define the corresponding specializazion for HASH.

The type `<T>` *can* be part of the hash code computation, i.e. if an object A is both
of type `<T1>` and `<T2>`, an implementation is allowed to produce different results
for `(HASH <T1> A)` and `(HASH <T2> A)`.

## Side effects:

none

## Exceptional situations:

An implementation can, but is not required to, signal a TYPE-ERROR if the object
passed to `(HASH (<T>) ...)` is not of type `<T>`.

## Specializations:

CL-PARAMETRIC-TYPES.STL predefines the following specializations:

        (HASH (<T>))
        (HASH ((PAIR <T1> <T2>)))
        (HASH ((SIMPLE-ARRAY <T>)))
        (HASH (SIMPLE-BIT-VECTOR))
        (HASH (SIMPLE-BASE-STRING))
        (HASH (SIMPLE-CHAR-STRING)) ;; i.e. (HASH ((SIMPLE-ARRAY CHARACTER (*))))
        (HASH ((TRIPLE <T1> <T2> <T3>)))

## Examples:

To be written...
|#

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
		    
