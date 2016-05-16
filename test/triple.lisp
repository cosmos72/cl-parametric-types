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

(alias ((<triple> (triple bit fixnum integer)))
  (def-test triple (:compile-at :definition-time)
    (let ((a (make-triple (bit fixnum integer) :first 1 :second 2 :third most-positive-fixnum))
          (b (make-triple (bit fixnum integer) :first 1 :second 2 :third (1+ most-positive-fixnum))))
      
      (is-false (less     (<triple>) a a))
      (is-true  (less     (<triple>) a b))
      (is-true  (equal-to (<triple>) a a))
      (is-false (equal-to (<triple>) a b)))))
