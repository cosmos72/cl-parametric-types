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

(def-suite bivector :in suite)
(in-suite bivector)

(alias ((<bivector> (bivector fixnum)))
  (def-test bivector (:compile-at :definition-time)
    (let ((v (new-bivector (fixnum) 7 :initial-contents #(0 1 2 3 4 5 6))))
      (is-false (empty?   (<bivector>) v))
      (is  (= 7 (size     (<bivector>) v)))
      (is  (= 0 (front    (<bivector>) v)))
      (is  (= 6 (back     (<bivector>) v)))
      (let ((iter (begin^ (<bivector>) v))
            (end  (end^   (<bivector>) v)))
        (dotimes (i 7)
          (is (= i (iter-value (<bivector>) iter)))
          (incf-iter (<bivector>) iter))
        (is-true (equal-to ((iterator <bivector>)) iter end))))))

