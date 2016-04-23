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

(template (&optional (<t1> t) (<t2> t))
  (defun pair-less (a b)
    (declare (type (pair <t1> <t2>) a b))
    ;; struct accessors want the FULL list of template arguments, in this case (<t1> <t2>)
    ;; even if in practice the accessor type alone could suffice...
    ;;
    ;; i.e. one must write (pair-first (<t1> <t2>) ...)
    ;; because PAIR is defined as (pair <t1> <t2>),
    ;; even though PAIR-FIRST actually only uses <t1>, not <t2>
    (let ((a1 (pair-first (<t1> <t2>) a))
          (b1 (pair-first (<t1> <t2>) b)))
      (cond
        ((< a1 b1) t)
        ((= a1 b1)
         (< (pair-second (<t1> <t2>) a)
            (pair-second (<t1> <t2>) b)))))))

(def-test pair (:compile-at :definition-time)
  (is-true
   (pair-less
    (fixnum fixnum)
    (make-pair (fixnum fixnum) :first 1 :second 2)
    (make-pair (fixnum fixnum) :first 1 :second 3))))
