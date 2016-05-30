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

DEQUE: a template-struct implementing resizeable, one-dimensional array
       with constant-time random access to elements
       and efficient insertion/removal of elements at both ends.
       It is equivalent to C++ std::deque<T>, with the difference that
       it internally stores the elements in continuous storage
|#

(alias ((<deque> (deque <t>))
        (<iterator> (iterator (deque <t>))))

  (template (&optional (<t> t))

    (defstruct deque
      "(DEQUE <T>): a template-struct implementing resizeable, one-dimensional array,
  with constant-time random access to elements and efficient insertion/removal
  of elements at both ends.
  It is equivalent to C++ std::deque<T>, with the difference that
  it internally stores the elements in continuous storage"
      (data  (make-array 0 :element-type '<t>) :type (simple-array-1 <t>))
      (start 0 :type ufixnum)
      (end   0 :type ufixnum))


    ;; TEMPLATE DEFSTRUCT above defines MAKE-DEQUE, and does not yet support :CONSTRUCTOR,
    ;; so we must pick a different name for the public constructor :(
    (defun new-deque (size &key initial-element initial-contents)
      (make-deque (<t>)
                  :data (if initial-contents
                            (make-array size
                                        :element-type '<t>
                                        :initial-contents initial-contents)
                            (make-array size
                                        :element-type '<t>
                                        :initial-element initial-element))
                  :start 0
                  :end   size)))
    

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (template (&optional (<t> t))
    (:specialized-for ((deque <t>)))

    ;; style guideline: define iterators with DEFSTRUCT rather than DEFTYPE
    ;; because #S(<ITERATOR.<MYCOLLECTION.T>> ...) is more readable
    ;; than #S(<MYCOLLECTION-ITERATOR> ...)
    (defstruct (iterator (:include (deque <t>)))
      (cursor 0 :type ufixnum))

    (declaim (notinline equal-to))
    (defun equal-to (x y)
      (declare (type <deque> x y))
      (or (eq x y)
          (let ((xstart (deque-start (<t>) x))
                (xend   (deque-end   (<t>) x))
                (ystart (deque-start (<t>) y))
                (yend   (deque-end   (<t>) y)))
            (and
             (= (the ufixnum (- xend xstart))
                (the ufixnum (- yend ystart)))
               
             (let ((xdata  (deque-data  (<t>) x))
                   (ydata  (deque-data  (<t>) y)))

               (loop :for xi :from xstart :below xend
                  :for yi :from ystart
                  :always
                  (let ((xe (aref xdata xi))
                        (ye (aref ydata yi)))
                    (equal-to (<t>) xe ye)))))))))
      
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (template (&optional (<t> t))
    (:specialized-for ((iterator (deque <t>))))
    
    (declaim (inline equal-to))
    (defun equal-to (x y)
      (declare (type <iterator> x y))
      (and
       (eq (iterator-data (<deque>) x)
           (iterator-data (<deque>) y))
       (=  (iterator-cursor (<deque>) x)
           (iterator-cursor (<deque>) y)))))
         

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (template (&optional (<t> t))
    (:specialized-for ((deque <t>)))


    (declaim (inline empty?))
    (defun empty? (deque)
      (declare (type <deque> deque))
      (>= (deque-start (<t>) deque)
          (deque-end   (<t>) deque)))
    
    
    (declaim (inline size))
    (defun size (deque)
      (declare (type <deque> deque))
      (the ufixnum
           (- (deque-end   (<t>) deque)
              (deque-start (<t>) deque))))
    
    
    (defun at^ (deque index)
      (declare (type <deque> deque)
               (type ufixnum index))
      (let ((start (deque-start (<t>) deque)))
        (make-iterator (<deque>)
                       :data   (deque-data (<t>) deque)
                       :start  start
                       :end    (deque-end (<t>) deque)
                       :cursor (the ufixnum (+ start index)))))
    
    (declaim (inline get-value))
    (defun get-value (deque index)
      (declare (type (deque <t>) deque)
               (type ufixnum index))
      (aref (deque-data (<t>) deque)
            (the ufixnum (+ index (deque-start (<t>) deque)))))
    
    
    (declaim (inline set-value))
    (defun set-value (deque index value)
      (declare (type (deque <t>) deque)
               (type ufixnum index)
               (type <t> value))
      (setf (aref (deque-data (<t>) deque)
                  (the ufixnum (+ index (deque-start (<t>) deque))))
            value))
    

    (declaim (inline begin^))
    (defun begin^ (deque)
      (declare (type <deque> deque))
      (at^ (<deque>) deque 0))
    

    (declaim (inline end^))
    (defun end^ (deque)
      (declare (type <deque> deque))
      (let ((end (deque-end (<t>) deque)))
        (make-iterator (<deque>)
                       :data   (deque-data (<t>) deque)
                       :start  (deque-start (<t>) deque)
                       :end    end
                       :cursor end)))


    (declaim (inline valid-iter?))
    (defun valid-iter? (iterator)
      (declare (type <iterator> iterator))
      (let ((cursor (iterator-cursor (<deque>) iterator)))
        (and
         (>= cursor (iterator-start (<deque>) iterator))
         (<  cursor (iterator-end   (<deque>) iterator)))))


    (defun front (deque)
      "If DEQUE is not empty, return its first element.
Otherwise *may* signal an error"
      (declare (type <deque> deque))
      (check-if-safe (not (empty? (<deque>) deque)))
      (aref (deque-data (<t>) deque)
            (deque-start (<t>) deque)))

    
    (defun back (deque)
      "If DEQUE is not empty, return its last element.
Otherwise *may* signal an error"
      (declare (type <deque> deque))
      (check-if-safe (not (empty? (<deque>) deque)))
      (aref (deque-data (<t>) deque)
            (the ufixnum (1- (deque-end (<t>) deque)))))
       

    (declaim (inline iter-key))
    (defun iter-key (iterator)
      "Return the KEY pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (declare (type <iterator> iterator))
      (check-if-safe (valid-iter? (<deque>) iterator))
      (aref (iterator-data   (<deque>) iterator)
            (iterator-cursor (<deque>) iterator)))

    
    (declaim (inline set-iter-key))
    (defun set-iter-key (iterator value)
      "Set the VALUE pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (declare (type <iterator> iterator)
               (type <t> value))
      (check-if-safe (valid-iter? (<deque>) iterator))
      (setf (aref (iterator-data   (<deque>) iterator)
                  (iterator-cursor (<deque>) iterator))
            value))

    
    (declaim (inline iter-value))
    (defun iter-value (iterator)
      "Return the VALUE pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (iter-key (<deque>) iterator))

    
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
           (incf (iterator-cursor (<deque>) ,viter) ,delta)
           ,viter)))

    
    (defmacro decf-iter (iterator &optional (delta 1))
      "Move ITERATOR backward by DELTA positions (default is 1)"
      (let ((viter (gensym)))
        `(let ((,viter ,iterator))
           (declare (type <iterator> ,viter))
           (decf (iterator-cursor (<deque>) ,viter) ,delta)
           ,viter)))

    ))



