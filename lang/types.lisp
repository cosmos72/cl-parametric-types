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

#|

aliases for common types

|#

(in-package :cl-parametric-types.lang)

(deftype sarray (&optional (element-type '*) (dimensions '*))
  "simple-array"
  `(simple-array ,element-type ,dimensions))

(deftype sarray-t (&optional (dimensions '*))
  "simple-array of T"
  `(simple-array t ,dimensions))

(deftype svector (&optional (element-type '*) (size '*))
  "one-dimensional simple-array"
  `(simple-array ,element-type (,size)))

(deftype svector-t (&optional (size '*))
  "one-dimensional simple-array of T. Equivalent to misnamed CL:SIMPLE-VECTOR"
  `(simple-array t (,size)))

(deftype sstring (&optional (size '*))
  "one-dimensional simple-array of CHARACTER"
  `(svector character ,size))

(deftype sbstring (&optional (size '*))
  "one-dimensional simple-array of BASE-CHAR. Equivalent to CL:SIMPLE-BASE-STRING"
  `(svector base-char ,size))

