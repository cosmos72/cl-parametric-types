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
  (defun less (a b)
    (declare (type (triple <t1> <t2> <t3>) a b))
    ;; see pair.lisp for the reason one must write
    ;; (TRIPLE-FIRST (<T1> <T2> <T3>) A)
    ;; instead of (TRIPLE-FIRST (<T1>) A)
    (let ((a1 (triple-first (<t1> <t2> <t3>) a))
          (b1 (triple-first (<t1> <t2> <t3>) b)))
      (cond
        ((< a1 b1) t)
        ((= a1 b1)
         (let ((a2 (triple-second (<t1> <t2> <t3>) a))
	       (b2 (triple-second (<t1> <t2> <t3>) b)))
	   (cond
	     ((< a2 b2) t)
	     ((= a2 b2)
	      (< (triple-third (<t1> <t2> <t3>) a)
		 (triple-third (<t1> <t2> <t3>) b))))))))))


