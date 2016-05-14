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

(def-suite triple :in suite)
(in-suite triple)

(def-test triple (:compile-at :definition-time)
  (is-true
   (less ((triple bit fixnum integer))
     (make-triple (bit fixnum integer) :first 1 :second 2 :third most-positive-fixnum)
     (make-triple (bit fixnum integer) :first 1 :second 2 :third (1+ most-positive-fixnum)))))
