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
      less<pair<T1,T2> >(const pair<T1,T2> & a, const pair<T1,T2> & b)
  {
     T1 a1 = a.first, b1 = b.first;
     if (a1 < b1)
         return true;
     if (a1 == b1)
         return a.second < b.second;
     return false;
  }
|#

(template (&optional (<t1> t) (<t2> t))
  (:specialized-for ((pair <t1> <t2>)))

  (declaim (notinline less))
  (defun less (a b)
    (declare (type (pair <t1> <t2>) a b))
    ;; struct accessors want the FULL list of template arguments, in this case (<t1> <t2>)
    ;; even if in practice the accessor type alone could suffice...
    ;;
    ;; i.e. one must write (pair-first (<t1> <t2>) ...)
    ;; because PAIR is defined as (pair <t1> <t2>),
    ;; even though PAIR-FIRST actually only uses <t1>, not <t2>
    (let ((a1 (pair-first (<t1> <t2>) a))
          (b1 (pair-first (<t1> <t2>) b)))
      (cond
        ((less (<t1>) a1 b1) t)
        ((less (<t1>) b1 a1) nil)
        (t
         (less (<t2>)
	       (pair-second (<t1> <t2>) a)
	       (pair-second (<t1> <t2>) b))))))


  (declaim (notinline hash))
  (defun hash (a)
    (declare (type (pair <t1> <t2>) a))
    (combine-hashes (sxhash '(pair <t1> <t2>))
		    (hash (<t1>) (pair-first  (<t1> <t2>) a))
		    (hash (<t1>) (pair-second (<t1> <t2>) a))))

  (declaim (notinline equal-to))
  (defun equal-to (a b)
    (declare (type (pair <t1> <t2>) a b))
    (and
     (equal-to (<t1>)
               (pair-first (<t1> <t2>) a)
               (pair-first (<t1> <t2>) b))
     (equal-to (<t2>)
	       (pair-second (<t1> <t2>) a)
	       (pair-second (<t1> <t2>) b)))))
