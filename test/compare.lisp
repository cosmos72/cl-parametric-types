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

(def-suite compare :in suite)
(in-suite compare)

(def-test less (:compile-at :definition-time)
  (is-true
   (progn
     (less (float) 0.0 1.0)))
  (is-true
   (progn
     (less (fixnum) 0 1)))
  (is-false
   (progn
     (less (float) 1.0 0.0)))
  (is-false
   (progn
     (less (fixnum) 1 0))))
