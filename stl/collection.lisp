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

(template (<collection>)

  ;; capacity
  
  (declaim (inline empty?))
  (defun empty? (collection)
    "Return T if COLLECTION is empty, otherwise return NIL.
Default implementation: (ZEROP (SIZE (<COLLECTION>) COLLECTION))"
    (declare (type <collection> collection))
    (zerop (size (<collection>) collection)))


  (defun size (collection)
    "Return number of elements in COLLECTION."
    (declare (type <collection> collection))
    (error! "No specialization of ~s for (~s)" 'size '<collection>))


  ;; iterators

  (defun begin^ (collection)
    "If COLLECTION is not empty, return iterator to first element.
Otherwise return END^"
    (error! "No specialization of ~s for (~s)" 'begin^ '<collection>))


  (defun end^ (collection)
    "Return an iterator pointing to \"one past last element\" of COLLECTION.
Some specializations may return NIL.
Note: to compare iterators, use (EQUAL-TO ((ITERATOR (<COLLECTION>)) ...) or,
to compare an iterator against the END^ iterator, use VALID-ITER? or END-ITER?"
    (error! "No specialization of ~s for (~s)" 'end^ '<collection>))

  
  (declaim (inline front^))
  (defun front^ (collection)
    "If COLLECTION is not empty, return iterator to first element.
Otherwise *may* signal an error"
    (declare (type <collection> collection))
    (check-if-safe (not (empty? (<collection>) collection)))
    (begin^ (<collection>) collection))


  (defun back^ (collection)
    "If COLLECTION is not empty, return iterator to last element.
Otherwise return END^"
    (error! "No specialization of ~s for (~s)" 'back^ '<collection>))


  (declaim (inline front))
  (defun front (collection)
    "If COLLECTION is not empty, return (values KEY VALUE)
where KEY and VALUE are the first entry in COLLECTION.
Otherwise *may* signal an error.
The default implementation calls FRONT^"
    (declare (type <collection> collection))
    (let ((iter (front^ (<collection>) collection)))
      (values (iter-key  (<collection>) iter)
              (iter-value (<collection>) iter))))


  (declaim (inline back))
  (defun back (collection)
    "If COLLECTION is not empty, return (values KEY VALUE)
where KEY and VALUE are the last entry in COLLECTION.
Otherwise *may* signal an error.
The default implementation calls BACK^"
    (declare (type <collection> collection))
    (let ((iter (back^ (<collection>) collection)))
      (values (iter-key  (<collection>) iter)
              (iter-value (<collection>) iter))))

  
  ;; lookup

  (defun find^ (set-or-map element)
    "If SET-OR-MAP contains ELEMENT, return iterator to it.
Otherwise return END^ iterator"
    (error! "No specialization of ~s for (~s)" 'find^ '<collection>))


  (defun at^ (collection key-or-index)
    "AT^ has different semantics depending on the type of collection:
For associative collections:
  if there is a VALUE associated to KEY-OR-INDEX, return iterator to it.
  otherwise return END^ iterator.
For random-access collections:
  return the N-th element, where N is equal to KEY-OR-INDEX."
    (error! "No specialization of ~s for (~s)" 'at^ '<collection>))


  (declaim (inline get-value))
  (defun get-value (collection key-or-index &optional default)
    "GET-VALUE has different semantics depending on the type of collection:
For associative collections:
  if there is a VALUE associated to KEY-OR-INDEX, return (values VALUE T),
  otherwise return (values DEFAULT NIL).
For random-access collections:
  return the N-th element, where N is equal to KEY-OR-INDEX.
The default implementation calls AT^"
    (declare (type <collection> collection))
    (let ((iter (at^ (<collection>) collection key)))
      (if (valid-iter? (<collection>) iter)
	  (values (iter-value (<collection>) iter) t)
	  (values default nil))))


  ;; modifiers

  (defun clear (collection)
    "Remove all elements from COLLECTION.
Return COLLECTION"
    (error! "No specialization of ~s for (~s)" 'clear '<collection>))


  (defun insert^ (set-or-map key &optional value)
    "If SET-OR-MAP contains KEY, do nothing and return (values ITERATOR NIL).
Otherwise insert KEY and VALUE, and return (values ITERATOR T).
In both cases, ITERATOR is the iterator pointing to KEY."
    (error! "No specialization of ~s for (~s)" 'insert^ '<collection>))


  (defun put^ (set-or-map key &optional value)
    "If SET-OR-MAP contains KEY, overwrite its VALUE and return (values ITERATOR NIL).
Otherwise insert KEY and VALUE, and return (values ITERATOR T).
In both cases, ITERATOR is the iterator pointing to KEY.
The default implementation calls FIND^ and INSERT^"
    (declare (type <collection> set-or-map))
    (let ((iter (find^ (<collection>) set-or-map)))
      (if (end-iter? (<collection>) iter)
          (insert^ (<collection>) set-or-map key value)
          (progn
            (setf (iter-value (<collection>) iter) value)
            (values iter nil)))))


  (declaim (inline set-value))
  (defun set-value (collection key-or-index value)
    "SET-VALUE has different semantics depending on the type of collection:
For associative collections:
  Add KEY and VALUE in COLLECTION. If COLLECTION already contains KEY, overwrite its VALUE.
  Return VALUE.
For random-access collections:
  set the N-th element to VALUE, where N is equal to KEY-OR-INDEX.
The default implementation calls PUT^"
    (declare (type <collection> collection))
    (put^ (<collection>) collection key value)
    value)
  

  (defun erase^ (collection iterator)
    "Erase element at ITERATOR in COLLECTION.
Return iterator following the last removed element."
    (error! "No specialization of ~s for (~s)" 'erase^ '<collection>))


  (defun erase (collection key-or-index &optional default)
    "If there is a VALUE associated to KEY-OR-INDEX in COLLECTION,
remove it and return (values VALUE T).
Otherwise return (values DEFAULT NIL).
The default implementation calls FIND^ and ERASE^"
    (declare (type <collection> collection))
    (let ((iter (find^ (<collection>) collection)))
      (if (valid-iter? (<collection>) iter)
          (let ((value (iter-value (<collection>) iter)))
            (erase^ (<collection>) collection iter)
            (values value t))
          (values default nil)))))





(defmacro [] ((<collection>) collection key)
  "Equivalent to (GET-VALUE (<COLLECTION>) COLLECTION KEY)"
  `(get-value (,<collection>) ,collection ,key))


(defmacro set-[] ((<collection>) collection key value)
  "Equivalent to (SET-VALUE (<COLLECTION>) COLLECTION KEY VALUE)"
  `(set-value (,<collection>) ,collection ,key ,value))


(defsetf get-value set-value)
(defsetf []        set-[])

