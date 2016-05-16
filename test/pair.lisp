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

(alias ((<pair> (pair fixnum fixnum)))
  (def-test pair (:compile-at :definition-time)
    (let ((a (make-pair (fixnum fixnum) :first 1 :second 2))
          (b (make-pair (fixnum fixnum) :first 1 :second 3)))

      (is-false (less     (<pair>) a a))
      (is-true  (less     (<pair>) a b))
      (is-true  (equal-to (<pair>) a a))
      (is-false (equal-to (<pair>) a b)))))
