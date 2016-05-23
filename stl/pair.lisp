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

(template (&optional (<t1> t) (<t2> t))
  (defstruct pair
    (first nil  :type <t1>)
    (second nil :type <t2>)))


#|
  The code below is equivalent to C++:

  template<class T1, class T2>
      less<pair<T1,T2> >(const pair<T1,T2> & x, const pair<T1,T2> & y)
  {
     T1 x1 = x.first, y1 = y.first;
     if (x1 < y1)
         return true;
     if (x1 == y1)
         return x.second < y.second;
     return false;
  }
|#


(template (&optional (<t1> t) (<t2> t))
  (:specialized-for ((pair <t1> <t2>)))

  (declaim (notinline less))
  (defun less (x y)
    (declare (type (pair <t1> <t2>) x y))
    ;; struct accessors want the FULL list of template arguments, in this case (<t1> <t2>)
    ;; even if in practice the accessor type alone could suffice...
    ;;
    ;; i.e. one must write (PAIR-FIRST (<T1> <T2>) ...)
    ;; because PAIR is defined as (PAIR <T1> <T2>),
    ;; even though PAIR-FIRST actually only uses <T1>, not <T2>
    (let ((x1 (pair-first (<t1> <t2>) x))
          (y1 (pair-first (<t1> <t2>) y)))
      (cond
        ((less (<t1>) x1 y1) t)
        ((less (<t1>) y1 x1) nil)
        (t
         (less (<t2>)
	       (pair-second (<t1> <t2>) x)
	       (pair-second (<t1> <t2>) y))))))


  (declaim (notinline hash))
  (defun hash (x)
    (declare (type (pair <t1> <t2>) x))
    (combine-hashes (sxhash '(pair <t1> <t2>))
		    (hash (<t1>) (pair-first  (<t1> <t2>) x))
		    (hash (<t2>) (pair-second (<t1> <t2>) x))))


  (declaim (notinline equal-to))
  (defun equal-to (x y)
    (declare (type (pair <t1> <t2>) x y))
    (and
     (equal-to (<t1>)
               (pair-first (<t1> <t2>) x)
               (pair-first (<t1> <t2>) y))
     (equal-to (<t2>)
	       (pair-second (<t1> <t2>) x)
	       (pair-second (<t1> <t2>) y)))))
