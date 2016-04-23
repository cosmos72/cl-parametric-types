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

(template (&optional (<t> 'real))
  (defun less (a b)
    (declare (type <t> a b))
    (< a b))

  (defun less-eql (a b)
    (declare (type <t> a b))
    (not (< b a)))

  (defun greater (a b)
    (declare (type <t> a b))
    (< b a))

  (defun greater-eql (a b)
    (declare (type <t> a b))
    (not (< a b))))


