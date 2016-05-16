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

  

  ;; capacity
  
  (declaim (inline empty?))
  (defun empty? (collection)
    "Return T if COLLECTION is empty, otherwise return NIL.
Default implementation: (ZEROP (SIZE (<T>) COLLECTION))"
    (declare (type <t> collection))
    (zerop (size (<t>) collection)))

  (defun size (collection)
    "Return number of elements in COLLECTION."
    (declare (type <t> collection))
    (error! "No specialization of ~s for (~s)" 'size '<t>))



  ;; iterators

  (defun begin^ (collection)
    "If COLLECTION is not empty, return iterator to first element.
Otherwise return END^"
    (error! "No specialization of ~s for (~s)" 'begin^ '<t>))

  (defun end^ (collection)
    "Return an iterator pointing to \"one past last element\" of COLLECTION.
Some specializations may return NIL."
    (error! "No specialization of ~s for (~s)" 'end^ '<t>))
  
  (declaim (inline front^))
  (defun front^ (collection)
    "Alias for BEGIN^"
    (begin^ <t> collection))

  (defun back^ (collection)
    "If COLLECTION is not empty, return iterator to last element.
Otherwise return END^"
    (error! "No specialization of ~s for (~s)" 'back^ '<t>))

  (declaim (inline front))
  (defun front (collection &optional default-key default-value)
    "If COLLECTION is not empty, return (values KEY VALUE T)
where KEY and VALUE are the first entry in COLLECTION.
Otherwise return (values DEFAULT-KEY DEFAULT-VALUE NIL)
The default implementation calls FRONT^"
    (declare (type <t> collection))
    (let ((iter (front^ <t> collection)))
      (if (eq iter (end^ <t> collection))
          (values default-key default-value nil)
          (values (iter-first  (iterator <t>) iter)
                  (iter-second (iterator <t>) iter)
                  t))))

  (declaim (inline back))
  (defun back (collection &optional default-key default-value)
    "If COLLECTION is not empty, return (values KEY VALUE T)
where KEY and VALUE are the last entry in COLLECTION.
Otherwise return (values DEFAULT-KEY DEFAULT-VALUE NIL)
The default implementation calls BACK^"
    (declare (type <t> collection))
    (let ((iter (back^ <t> collection)))
      (if (eq iter (end^ <t> collection))
          (values default-key default-value nil)
          (values (iter-first  (iterator <t>) iter)
                  (iter-second (iterator <t>) iter)
                  t))))



  ;; lookup

  (defun find^ (set-or-map element)
    "If SET-OR-MAP contains ELEMENT, return iterator to it.
Otherwise return END^"
    (error! "No specialization of ~s for (~s)" 'find^ '<t>))


  (declaim (inline get-value))
  (defun get-value (collection key &optional default)
    "If there is a VALUE associated to KEY in COLLECTION, return (values VALUE T).
otherwise return (values DEFAULT NIL).
The default implementation calls FIND^"
    (declare (type <t> collection))
    (let ((iter (find^ <t> collection key)))
      (if (eq iter (end^ <t> collection))
	  (values default nil)
	  (values (iter-second (iterator <t>) iter) t))))



  ;; modifiers

  (defun clear (collection)
    "Remove all elements from COLLECTION.
Return COLLECTION"
    (error! "No specialization of ~s for (~s)" 'clear '<t>))

  (defun insert^ (set-or-map key &optional value)
    "If SET-OR-MAP contains KEY, do nothing and return (values ITERATOR NIL).
Otherwise insert KEY and VALUE, and return (values ITERATOR T).
In both cases, ITERATOR is the iterator pointing to KEY."
    (error! "No specialization of ~s for (~s)" 'insert^ '<t>))

  (defun put^ (set-or-map key &optional value)
    "If SET-OR-MAP contains KEY, overwrite its VALUE and return (values ITERATOR NIL).
Otherwise insert KEY and VALUE, and return (values ITERATOR T).
In both cases, ITERATOR is the iterator pointing to KEY.
The default implementation calls FIND^ and INSERT^"
    (declare (type <t> set-or-map))
    (let ((iter (find^ <t> set-or-map)))
      (if (eq iter (end^ <t> set-or-map))
          (insert^ <t> set-or-map key value)
          (progn
            (setf (iter-second (iterator <t>) iter) value)
            (values iter nil)))))

  (declaim (inline set-value))
  (defun set-value (collection key &optional value)
    "Add KEY and VALUE in COLLECTION. If COLLECTION already contains KEY, overwrite its VALUE.
Return VALUE.
The default implementation calls PUT^"
    (declare (type <t> collection))
    (put^ <t> collection key value)
    value)
  
  (defun erase^ (collection iterator)
    "Erase element at ITERATOR in COLLECTION.
Return iterator following the last removed element."
    (error! "No specialization of ~s for (~s)" 'erase^ '<t>))

  (defun erase (collection key &optional default)
    "If there is a VALUE associated to KEY in COLLECTION,
remove it and return (values VALUE T).
Otherwise return (values DEFAULT NIL).
The default implementation calls FIND^ and ERASE^"
    (declare (type <t> collection))
    (let ((iter (find^ <t> collection)))
      (if (eq iter (end^ <t> collection))
          (values default nil)
          (let ((value (iter-second (iterator <t>) iter)))
            (erase^ collection iter)
            (values value t))))))



(defsetf get-value set-value)


