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

(template (&optional (<t1> t) (<t2> t) (<t3> t))
  (defun triple-less (a b)
    (declare (type (cpt.stl:triple <t1> <t2> <t3>) a b))
    ;; struct accessors want the FULL list of template arguments, in this case (<t1> <t2> <t3>)
    ;; even if in practice the accessor type alone could suffice...
    (let ((a1 (triple-first (<t1> <t2> <t3>) a))
          (b1 (triple-first (<t1> <t2> <t3>) b)))
      (block nil
        (when (< a1 b1)
          (return t))
        (when (= a1 b1)
          (let ((a2 (triple-second (<t1> <t2> <t3>) a))
                (b2 (triple-second (<t1> <t2> <t3>) b)))
            (when (< a2 b2)
              (return t))
            (when (= a2 b2)
              (< (triple-third (<t1> <t2> <t3>) a)
                 (triple-third (<t1> <t2> <t3>) b)))))))))

(def-test triple (:compile-at :definition-time)
  (is-true
   (triple-less
    (bit fixnum integer)
    (make-triple (bit fixnum integer) :first 1 :second 2 :third most-positive-fixnum)
    (make-triple (bit fixnum integer) :first 1 :second 2 :third (1+ most-positive-fixnum)))))
