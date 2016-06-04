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

(def-suite vector :in suite)
(in-suite vector)

(alias ((<vector> (vector* fixnum))
        (<iterator> (iterator (vector* fixnum))))
  (def-test vector (:compile-at :definition-time)
    (let ((v (new-vector*  (fixnum) :initial-contents #(0 1 2 3 4 5 6)))
          (w (new-vector*  (fixnum))))
      (is-false (empty?   (<vector>) v))
      (is  (= 7 (size     (<vector>) v)))
      (is  (= 0 (front    (<vector>) v)))
      (is  (= 6 (back     (<vector>) v)))
      
      (is-true  (empty?   (<vector>) w))
      (is-false (equal-to (<vector>) v w))

      (let ((iter (begin^ (<vector>) v))
            (end  (end^   (<vector>) v)))
        (is-false (equal-to (<iterator>) iter end))
        (is-true  (valid-iter? (<vector>) iter))
        (is-false (valid-iter? (<vector>) end))

        (dotimes (i 7)
          (is (= i (iter-value (<vector>) iter)))
          (incf-iter (<vector>) iter)
          (push-back (<vector>) w i))
        (is-true (equal-to (<iterator>) iter end)))
      
      (is  (equal-to      (<vector>) v w)))))


(defun grow-vector (count &optional (n0 count))
  (declare (type fixnum n0 count))
  (let ((v (make-array n0 :element-type t :adjustable t :fill-pointer 0)))
    (dotimes (i count)
      (vector-push i v))
    (length v)))


(defun grow-vector* (count &optional (n0 count))
  (declare (type fixnum n0 count))
  (let ((v (new-vector* (t) :initial-capacity n0)))
    (dotimes (i count)
      (push-back ((vector* t)) v i))
    (size ((vector* t)) v)))


(defun grow-deque (count &optional (n0 count))
  (declare (type fixnum n0 count))
  (let ((v (new-deque (t))))
    (reserve ((deque t)) v n0 :at :back)
    (dotimes (i count)
      (push-front ((deque t)) v i))
    (size ((deque t)) v)))
