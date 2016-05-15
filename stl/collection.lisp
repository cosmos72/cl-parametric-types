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

(in-package #:cl-parametric-types.stl)

;;;; ** Functions related to collections

(template (<t>)
  (defstruct iterator))


(template (<t>)
  (defun clear (collection)
    "Remove all elements from COLLECTION."
    (error! "No specialization of ~s for (~s)" 'clear '<t>)))


(template (<t>)
  (declaim (inline empty?))
  (defun empty? (collection)
    "Return t if COLLECTION is empty, otherwise return nil.
Default implementation: (ZEROP (SIZE (<T>) COLLECTION))"
    (declare (type <t> collection))
    (zerop (size (<t>) collection))))


(template (<t>)
  (defun size (collection)
    "Return number of elements in COLLECTION."
    (declare (type <t> collection))
    (error! "No specialization of ~s for (~s)" 'size '<t>)))


(template (<t>)
  (defun lookup (set-or-map element)
    "Return iterator-to-element if SET-OR-MAP contains ELEMENT,
otherwise return nil."
    (error! "No specialization of ~s for (~s)" 'lookup '<t>)))


(template (<t>)
  (declaim (inline get-value))
  (defun get-value (map key &optional default)
    "If there is a VALUE associated to KEY in MAP, return (values VALUE T).
otherwise return (values DEFAULT NIL).
The default implementation calls (LOOKUP <T> MAP KEY)."
    (let ((iter (lookup <t> map key)))
      (if iter
	  (values (iter-second (iterator <t>) iter) t)
	  (values default nil)))))


(template (<t>)
  (defun set-value (map key value)
    "If there is a VALUE associated to KEY in MAP, return (values VALUE T).
otherwise return (values DEFAULT NIL)."
    (error! "No specialization of ~s for (~s)" 'set-value '<t>)))


(defsetf get-value set-value)


(template (<t>)
  (defun rem-value (map key &optional default)
    "If there is a VALUE associated to KEY in MAP, remove it and return (values VALUE T).
otherwise return (values DEFAULT NIL)."
    (error! "No specialization of ~s for (~s)" 'rem-value '<t>)))
