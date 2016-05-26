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

;;;; ** Functions related to iterators


(template (<collection>)

  (deftype iterator ()
    (error "No specialization of ~s for (~s)" 'iterator '<collection>))

  (defun valid-iter? (iterator)
    "Return non-nil if ITERATOR is valid, i.e. different from END^ iterator.
Note: the template argument <COLLECTION> is the collection type, not the iterator type."
    (error! "No specialization of ~s for (~s)" 'valid-iter? '<collection>))

  (declaim (inline end-iter?))
  (defun end-iter? (iterator)
    "Return non-nil if ITERATOR is invalid, i.e. equal to END^ iterator.
Note: the template argument <COLLECTION> is the collection type, not the iterator type.
The default implementation calls (NOT (VALID-ITER? (<COLLECTION>) ITERATOR))"
    (declare (type (iterator <collection>) iterator))
    (not (valid-iter? (<collection>) iterator)))

  (defun iter-key (iterator)
    "Return the KEY pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
    (error! "No specialization of ~s for (~s)" 'iter-key '<collection>))

  (defun set-iter-key (iterator value)
    "Set the VALUE pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
    (error! "No specialization of ~s for (~s)" 'set-iter-key '<collection>))

  (defun iter-value (iterator)
    "Return the VALUE pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
    (error! "No specialization of ~s for (~s)" 'iter-value '<collection>))

  (defun set-iter-value (iterator value)
    "Set the VALUE pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
    (error! "No specialization of ~s for (~s)" 'set-iter-value '<collection>))

  (defmacro incf-iter (iterator &optional (delta 1))
    "Move ITERATOR forward by DELTA positions (default is 1)"
    (error! "No specialization of ~s for (~s)" 'incf-iter '<collection>))

  (defmacro decf-iter (iterator &optional (delta 1))
    "Move ITERATOR backward by DELTA positions (default is 1)"
    (error! "No specialization of ~s for (~s)" 'decf-iter '<collection>)))

  
(defsetf iter-key  set-iter-key)
(defsetf iter-value set-iter-value)


;; this function exists solely because the default template definition
;; of ITERATOR is a DEFTYPE, not a DEFSTRUCT
(defmacro make-iterator ((<collection>) &rest function-args)
  "Instantiate an iterator"
  ;; (typexpand `(iterator ,<collection>))
  `(,(instantiate 'template-constructor 'make `((iterator ,<collection>))) ,@function-args))

