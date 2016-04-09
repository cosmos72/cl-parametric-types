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


(in-package #:cl-parametric-types)

(template-struct (&optional (<t1> t) (<t2> t))
  (defstruct pair
    (first nil  :type <t1>)
    (second nil :type <t2>)))

(template-function (&optional (<t> t))
  (defun less (a b)
    (declare (type <t> a b))
    (< a b)))
		     
