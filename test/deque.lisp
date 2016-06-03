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

(def-suite deque :in suite)
(in-suite deque)

(alias ((<deque> (deque fixnum))
        (<iterator> (iterator (deque fixnum))))
  (def-test deque (:compile-at :definition-time)
    (let* ((v (new-deque  (fixnum) :initial-contents #(-4 -3 -2 -1 0 1 2 3 4)))
           (w (new-deque  (fixnum))))
      (is-false  (empty?   (<deque>) v))
      (is-true   (empty?   (<deque>) w))
      (is  (= 9  (size     (<deque>) v)))
      (is  (= -4 (front    (<deque>) v)))
      (is  (= 4  (back     (<deque>) v)))
      (is-false (equal-to  (<deque>) v w))

      (let ((iter (begin^ (<deque>) v))
            (end  (end^   (<deque>) v)))

        (dotimes (i 9)
          (let ((element (iter-value (<deque>) iter)))
            (is (= (- i 4) element))
            (incf-iter (<deque>) iter)))
        (is-true (equal-to (<iterator>) iter end)))

      (dotimes (i 5)
        (push-back (<deque>) w i)
        (unless (zerop i)
          (push-front (<deque>) w (- i))))

      (is-true (equal-to  (<deque>) v w)))))
    

