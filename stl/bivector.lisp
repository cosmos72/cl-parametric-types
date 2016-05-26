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

#|

BIVECTOR: a template-struct implementing resizeable, one-dimensional array.
          It is slightly more general than C++ std::vector<T>,
          since it supports efficient (i.e. amortized O(1))
          element insert and erase both ends.

|#

(template (&optional (<t> t))

  (defstruct bivector
    "(BIVECTOR <T>): a template-struct implementing resizeable, one-dimensional array.
  It is slightly more general than C++ std::vector<T>, since it supports
  efficient (i.e. amortized O(1)) element insertion and removal both ends."
    (data  (make-array 0 :element-type '<t>) :type (simple-array-1 <t>))
    (start 0 :type ufixnum)
    (end   0 :type ufixnum))


  (defstruct (biiterator (:include (bivector <t>)))
    (cursor 0 :type ufixnum))


  ;; TEMPLATE DEFSTRUCT above defines MAKE-BIVECTOR, and does not yet support :CONSTRUCTOR,
  ;; so we must pick a different name for the public constructor :(
  (defun new-bivector (size &key initial-element initial-contents)
    (make-bivector (<t>)
                   :data (if initial-contents
                             (make-array (list size)
                                         :element-type '<t>
                                         :initial-contents initial-contents)
                             (make-array (list size)
                                         :element-type '<t>
                                         :initial-element initial-element))
                   :start 0
                   :end   size))


  (declaim (inline biref))
  (defun biref (bivector index)
    (declare (type (bivector <t>) bivector)
             (type ufixnum index))
    (aref (bivector-data (<t>) bivector)
          (the ufixnum (+ index (bivector-start (<t>) bivector)))))


  (declaim (inline set-biref))
  (defun set-biref (bivector index value)
    (declare (type (bivector <t>) bivector)
             (type ufixnum index)
             (type <t> value))
    (setf (aref (bivector-data (<t>) bivector)
                (the ufixnum (+ index (bivector-start (<t>) bivector))))
          value)))


(defsetf biref set-biref)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(template (&optional (<t> t))
  (:specialized-for ((bivector <t>)))

  (deftype iterator () '(biiterator <t>)))


(alias ((<bivector> (bivector <t>))
        (<iterator> (iterator (bivector <t>))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (template (&optional (<t> t))
    (:specialized-for ((iterator (bivector <t>))))
    
    (declaim (inline equal-to))
    (defun equal-to (x y)
      (declare (type <iterator> x y))
      (and
       (eq (biiterator-data (<t>) x)
           (biiterator-data (<t>) y))
       (=  (biiterator-cursor (<t>) x)
           (biiterator-cursor (<t>) y)))))
         

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (template (&optional (<t> t))
    (defun bivector-mkiter (bivector index)
      (declare (type <bivector> bivector)
               (type ufixnum index))
      (let ((start (bivector-start (<t>) bivector)))
        (make-biiterator (<t>)
                         :data   (bivector-data (<t>) bivector)
                         :start  start
                         :end    (bivector-end (<t>) bivector)
                         :cursor (the ufixnum (+ start index)))))
    
    (declaim (inline bivector-mkbegin))
    (defun bivector-mkbegin (bivector)
      (declare (type <bivector> bivector))
      (bivector-mkiter (<t>) bivector 0))
    
    (defun bivector-mkend (bivector)
      (declare (type <bivector> bivector))
      (let ((end (bivector-end (<t>) bivector)))
        (make-biiterator (<t>)
                         :data   (bivector-data (<t>) bivector)
                         :start  (bivector-start (<t>) bivector)
                         :end    end
                         :cursor end))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (template (&optional (<t> t))
    (:specialized-for ((bivector <t>)))


    (declaim (inline empty?))
    (defun empty? (bivector)
      (declare (type <bivector> bivector))
      (>= (bivector-start (<t>) bivector)
          (bivector-end   (<t>) bivector)))


    (declaim (inline size))
    (defun size (bivector)
      (declare (type <bivector> bivector))
      (the ufixnum
           (- (bivector-end   (<t>) bivector)
              (bivector-start (<t>) bivector))))

    
    (declaim (inline begin^))
    (defun begin^ (bivector)
      (declare (type <bivector> bivector))
      (bivector-mkbegin (<t>) bivector))


    (declaim (inline end^))
    (defun end^ (bivector)
      (declare (type <bivector> bivector))
      (bivector-mkend (<t>) bivector))


    (declaim (inline valid-iter?))
    (defun valid-iter? (iterator)
      (declare (type <iterator> iterator))
      (let ((cursor (biiterator-cursor (<t>) iterator)))
        (and
         (>= cursor (biiterator-start (<t>) iterator))
         (<  cursor (biiterator-end   (<t>) iterator)))))


    ;; no need to specialize FRONT^

    (defun back^ (bivector)
      "If BIVECTOR is not empty, return iterator to last element.
Otherwise *may* signal an error"
      (declare (type <bivector> bivector))
      (check-if-safe (not (empty? (<bivector>) bivector)))
      (let ((size (size (<bivector>) bivector)))
        (bivector-mkiter (<t>) bivector (1- size))))

    
    (defun front (bivector)
      "If BIVECTOR is not empty, return its first element.
Otherwise *may* signal an error"
      (declare (type <bivector> bivector))
      (check-if-safe (not (empty? (<bivector>) bivector)))
      (biref (<t>) bivector 0))

    
    (defun back (bivector)
      "If BIVECTOR is not empty, return its last element.
Otherwise *may* signal an error"
      (declare (type <bivector> bivector))
      (check-if-safe (not (empty? (<bivector>) bivector)))
      (aref (bivector-data (<t>) bivector)
            (the ufixnum (1- (bivector-end (<t>) bivector)))))
       

    (declaim (inline iter-key))
    (defun iter-key (iterator)
      "Return the KEY pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (declare (type <iterator> iterator))
      (check-if-safe (valid-iter? (<bivector>) iterator))
      (aref (biiterator-data   (<t>) iterator)
            (biiterator-cursor (<t>) iterator)))

    
    (declaim (inline set-iter-key))
    (defun set-iter-key (iterator value)
      "Set the VALUE pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (declare (type <iterator> iterator)
               (type <t> value))
      (check-if-safe (valid-iter? (<bivector>) iterator))
      (setf (aref (biiterator-data   (<t>) iterator)
                  (biiterator-cursor (<t>) iterator))
            value))

    
    (declaim (inline iter-value))
    (defun iter-value (iterator)
      "Return the VALUE pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (iter-key (<bivector>) iterator))

    
    (declaim (inline set-iter-value))
    (defun set-iter-value (iterator value)
      "Set the VALUE pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (set-iter-key iterator value))

    
    (defmacro incf-iter (iterator &optional (delta 1))
      "Move ITERATOR forward by DELTA positions (default is 1)"
      (let ((viter (gensym)))
        `(let ((,viter ,iterator))
           (declare (type <iterator> ,viter))
           (incf (biiterator-cursor (<t>) ,viter) ,delta)
           ,viter)))

    
    (defmacro decf-iter (iterator &optional (delta 1))
      "Move ITERATOR backward by DELTA positions (default is 1)"
      (let ((viter (gensym)))
        `(let ((,viter ,iterator))
           (declare (type <iterator> ,viter))
           (decf (iterator-cursor (<bivector>) ,viter) ,delta)
           ,viter)))

    ))


