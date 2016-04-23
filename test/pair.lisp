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

(in-package :cl-parametric-types.test)

(def-suite pair :in suite)
(in-suite pair)

(template (&optional (<t1> t) (<t2> t))
  (defstruct pair
    (first  nil :type <t1>)
    (second nil :type <t2>))
  (defun pair-less (a b)
    (and (< (pair-first (<t1>) a) (pair-first (<t2>) b))
         (< (pair-second (<t1>) a) (pair-second (<t2>) b)))))

(def-test pair (:compile-at :definition-time)
  (is-true
   (progn
     (pair-less
      (fixnum fixnum)
      (make-pair (fixnum fixnum) :first 1 :second 2)
      (make-pair (fixnum fixnum) :first 3 :second 4)))))


