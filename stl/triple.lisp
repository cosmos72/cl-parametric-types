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

(template (&optional (<t1> t) (<t2> t) (<t3> t))
  (defstruct (triple (:include (pair <t1> <t2>)))
    (third nil  :type <t3>)))


(template (&optional (<t1> t) (<t2> t) (<t3> t))
  (:specialized-for ((triple <t1> <t2> <t3>)))
  (declaim (notinline less))
  (defun less (x y)
    (declare (type (triple <t1> <t2> <t3>) x y))
    ;; see pair.lisp for the reason one must write
    ;; (TRIPLE-FIRST (<T1> <T2> <T3>) X)
    ;; instead of (TRIPLE-FIRST (<T1>) X)
    (let ((x1 (triple-first (<t1> <t2> <t3>) x))
          (y1 (triple-first (<t1> <t2> <t3>) y)))
      (cond
        ((less (<t1>) x1 y1) t)
        ((less (<t1>) y1 x1) nil)
        (t
         (let ((x2 (triple-second (<t1> <t2> <t3>) x))
	       (y2 (triple-second (<t1> <t2> <t3>) y)))
	   (cond
	     ((less (<t2>) x2 y2) t)
	     ((less (<t2>) y2 x2) nil)
	     (t
	      (less (<t3>)
		    (triple-third (<t1> <t2> <t3>) x)
		    (triple-third (<t1> <t2> <t3>) y)))))))))


  (declaim (notinline hash))
  (defun hash (x)
    (declare (type (triple <t1> <t2> <t3>) x))
    (combine-hashes (sxhash '(triple <t1> <t2> <t3>))
		    (hash (<t1>) (triple-first  (<t1> <t2> <t3>) x))
		    (hash (<t2>) (triple-second (<t1> <t2> <t3>) x))
		    (hash (<t3>) (triple-third  (<t1> <t2> <t3>) x))))


  (declaim (notinline equal-to))
  (defun equal-to (x y)
    (declare (type (triple <t1> <t2> <t3>) x y))
    (and
     (equal-to (<t1>)
               (triple-first (<t1> <t2> <t3>) x)
               (triple-first (<t1> <t2> <t3>) y))
     (equal-to (<t2>)
	       (triple-second (<t1> <t2> <t3>) x)
	       (triple-second (<t1> <t2> <t3>) y))
     (equal-to (<t3>)
	       (triple-third (<t1> <t2> <t3>) x)
	       (triple-third (<t1> <t2> <t3>) y)))))

