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

  (defstruct iterator)


  (defun clear (collection)
    "Remove all elements from COLLECTION."
    (error! "No specialization of ~s for (~s)" 'clear '<t>))


  (declaim (inline empty?))
  (defun empty? (collection)
    "Return t if COLLECTION is empty, otherwise return nil.
Default implementation: (ZEROP (SIZE (<T>) COLLECTION))"
    (declare (type <t> collection))
    (zerop (size (<t>) collection)))


  (defun size (collection)
    "Return number of elements in COLLECTION."
    (declare (type <t> collection))
    (error! "No specialization of ~s for (~s)" 'size '<t>))


  (defun find-iter (set-or-map element)
    "Return iterator-to-element if SET-OR-MAP contains ELEMENT,
otherwise return nil."
    (error! "No specialization of ~s for (~s)" 'find-iter '<t>))


  (defun first-iter (collection)
    "if COLLECTION is not empty, return iterator to first element.
Otherwise return nil."
    (error! "No specialization of ~s for (~s)" 'first-iter '<t>))


  (defun last-iter (collection)
    "if COLLECTION is not empty, return iterator to last element.
Otherwise return nil."
    (error! "No specialization of ~s for (~s)" 'last-iter '<t>))


  (declaim (inline get-value))
  (defun get-value (collection key &optional default)
    "If there is a VALUE associated to KEY in COLLECTION, return (values VALUE T).
otherwise return (values DEFAULT NIL).
The default implementation calls (FIND-ITER <T> MAP KEY)."
    (declare (type <t> collection))
    (let ((iter (find-iter <t> collection key)))
      (if iter
	  (values (iter-second (iterator <t>) iter) t)
	  (values default nil))))


  (declaim (inline first-value))
  (defun first-value (collection &optional default-key default-value)
    "If COLLECTION is not empty, return (values KEY VALUE T)
where KEY is the first entry in COLLECTION.
Otherwise return (values DEFAULT-KEY DEFAULT-VALUE NIL)
The default implementation calls (FIRST-ITER <T> MAP KEY)."
    (declare (type <t> collection))
    (let ((iter (first-iter <t> collection)))
      (if iter
          (values (iter-first  (iterator <t>) iter)
                  (iter-second (iterator <t>) iter)
                  t)
          (values default-key default-value nil))))


  (declaim (inline last-value))
  (defun last-value (collection &optional default-key default-value)
    "If COLLECTION is not empty, return (values KEY VALUE T)
where KEY is the last entry in COLLECTION.
Otherwise return (values DEFAULT-KEY DEFAULT-VALUE NIL)
The default implementation calls (LAST-ITER <T> MAP KEY)."
    (declare (type <t> collection))
    (let ((iter (last-iter <t> collection)))
      (if iter
          (values (iter-first  (iterator <t>) iter)
                  (iter-second (iterator <t>) iter)
                  t)
          (values default-key default-value nil))))


  (defun rem-value (collection key &optional default)
    "If there is a VALUE associated to KEY in COLLECTION, remove it and return (values VALUE T).
otherwise return (values DEFAULT NIL)."
    (error! "No specialization of ~s for (~s)" 'rem-value '<t>))


  (defun put-value (collection key value)
    "Add KEY and VALUE in COLLECTION. If COLLECTION already contains KEY, overwrite its VALUE.
Return VALUE."
    (error! "No specialization of ~s for (~s)" 'put-value '<t>)))




(defsetf get-value put-value)

