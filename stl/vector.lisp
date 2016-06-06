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

VECTOR*: a template-struct implementing resizeable, one-dimensional array
         with constant-time random access to elements
         and efficient insertion/removal of elements at the end.
         It is equivalent to C++ std::vector<T>
|#

(alias ((<vector>   (vector* <t>))
        (<iterator> (iterator (vector* <t>)))
        (<vdata>    (vector*-data (<t>) vector*))
        (<vsize>    (vector*-size (<t>) vector*)))
  
  (template (&optional (<t> t))

    (defstruct vector*
      "(VECTOR* <T>): a template-struct implementing resizeable, one-dimensional array,
  with constant-time random access to elements and efficient insertion/removal
  of elements at the end. It is equivalent to C++ std::vector<T>"
      (data  (make-array 0 :element-type '<t>) :type (simple-array-1 <t>))
      (size  0 :type ufixnum))
    

    (declaim (notinline new-vector*))
    (defun new-vector* (&key (initial-size 0) (initial-capacity 0)
                          (initial-element nil initial-element?)
                          (initial-contents nil initial-contents?))
      (let* ((size (if initial-contents? (length initial-contents) initial-size))
             (capacity (max size initial-capacity)))
        (make-vector* (<t>)
                      :data (cond
                              (initial-contents?
                               (make-array size
                                           :element-type '<t>
                                           :initial-contents initial-contents))
                              (initial-element?
                               (make-array capacity
                                           :element-type '<t>
                                           :initial-element initial-element))
                              (t
                               (make-array capacity
                                           :element-type '<t>)))
                      :size size))))
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (template (&optional (<t> t))
    (:specialized-for ((vector* <t>)))

    (defmacro new (&rest args)
      "NEW specialization for (VECTOR* <T>)"
      `(new-vector* (<t>) ,@args))

    ;; style guideline: define iterators with DEFSTRUCT rather than DEFTYPE
    ;; because #S(<ITERATOR.<MYCOLLECTION.T>> ...) is more readable
    ;; than #S(<MYITERATOR-FOR-MYCOLLECTION.T> ...)
    (defstruct (iterator (:include (vector* <t>)))
      (cursor 0 :type ufixnum))
    
    (declaim (notinline equal-to))
    (defun equal-to (x y)
      (declare (type <vector> x y))
      (or (eq x y)
          (let ((xsize (vector*-size (<t>) x))
                (ysize (vector*-size (<t>) y)))
            (and
             (= xsize ysize)
             (let ((xdata (vector*-data (<t>) x))
                   (ydata (vector*-data (<t>) y)))
               
               (loop :for i :from 0 :below xsize
                  :always
                  (equal-to (<t>) (aref xdata i) (aref ydata i)))))))))
      
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (template (&optional (<t> t))
    (:specialized-for ((iterator (vector* <t>))))
    
    (declaim (inline equal-to))
    (defun equal-to (x y)
      (declare (type <iterator> x y))
      (and
       (eq (iterator-data (<vector>) x)
           (iterator-data (<vector>) y))
       (=  (iterator-cursor (<vector>) x)
           (iterator-cursor (<vector>) y)))))
         

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (template (&optional (<t> t))
    (:specialized-for ((vector* <t>)))


    ;; size and capacity
    
    (declaim (inline empty?))
    (defun empty? (vector*)
      (declare (type <vector> vector*))
      (zerop <vsize>))
    
    
    (declaim (inline size))
    (defun size (vector*)
      (declare (type <vector> vector*))
      <vsize>)


    (declaim (inline capacity))
    (defun capacity (vector*)
      (declare (type <vector> vector*))
      (array-dimension <vdata> 0))


    (defun resize (vector* new-size)
      "Set the VECTOR* size. Return new size."
      (declare (type <vector> vector*)
               (type ufixnum new-size))
      (let ((old-capacity (capacity (<vector>) vector*)))
        (when (> new-size old-capacity)
          (let ((new-capacity
                 (if (<= old-capacity (truncate array-dimension-limit 2))
                     (ash old-capacity 1)
                     array-dimension-limit)))
            (reserve (<vector>) vector* (max 4 new-size new-capacity)))))
      (setf <vsize> new-size))
    

    (defun reserve (vector* new-capacity)
      "Set the VECTOR* capacity. Return new capacity.
Does not alter vector size, and minimum capacity is the size."
      (declare (type <vector> vector*)
               (type ufixnum new-capacity))
      (let* ((size         <vsize>)
             (old-data     <vdata>)
             (old-capacity (array-dimension old-data 0))
             (new-capacity (max new-capacity size)))
        (unless (= old-capacity new-capacity)
          (let ((new-data (make-array new-capacity :element-type '<t>)))
            (dotimes (i size)
              (setf (aref new-data i) (aref old-data i)))
            (setf <vdata> new-data)))
        new-capacity))
    


    ;; iterators
    
    
    (defun at^ (vector* index)
      (declare (type <vector> vector*)
               (type ufixnum index))
      (make-iterator (<vector>)
                     :data   <vdata>
                     :size   <vsize>
                     :cursor index))
    

    (declaim (inline begin^))
    (defun begin^ (vector*)
      (declare (type <vector> vector*))
      (at^ (<vector>) vector* 0))
    

    (declaim (inline end^))
    (defun end^ (vector*)
      (declare (type <vector> vector*))
      (at^ (<vector>) vector* <vsize>))


    (declaim (inline valid-iter?))
    (defun valid-iter? (iterator)
      (declare (type <iterator> iterator))
      (< (iterator-cursor (<vector>) iterator)
         (iterator-size   (<vector>) iterator)))


    (declaim (inline iter-key))
    (defun iter-key (iterator)
      "Return the KEY pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (declare (type <iterator> iterator))
      (check-if-safe (valid-iter? (<vector>) iterator))
      (aref (iterator-data   (<vector>) iterator)
            (iterator-cursor (<vector>) iterator)))

    
    (declaim (inline set-iter-key))
    (defun set-iter-key (iterator value)
      "Set the VALUE pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (declare (type <iterator> iterator)
               (type <t> value))
      (check-if-safe (valid-iter? (<vector>) iterator))
      (setf (aref (iterator-data   (<vector>) iterator)
                  (iterator-cursor (<vector>) iterator))
            value))

    
    (declaim (inline iter-value))
    (defun iter-value (iterator)
      "Return the VALUE pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (iter-key (<vector>) iterator))

    
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
           (incf (iterator-cursor (<vector>) ,viter) ,delta)
           ,viter)))

    
    (defmacro decf-iter (iterator &optional (delta 1))
      "Move ITERATOR backward by DELTA positions (default is 1)"
      (let ((viter (gensym)))
        `(let ((,viter ,iterator))
           (declare (type <iterator> ,viter))
           (decf (iterator-cursor (<vector>) ,viter) ,delta)
           ,viter)))


    ;; lookup

    
    (defun front (vector*)
      "If VECTOR* is not empty, return its first element.
Otherwise *may* signal an error"
      (declare (type <vector> vector*))
      (check-if-safe (not (empty? (<vector>) vector*)))
      (get-value (<vector>) vector* 0))

    
    (defun back (vector*)
      "If VECTOR* is not empty, return its last element.
Otherwise *may* signal an error"
      (declare (type <vector> vector*))
      (check-if-safe (not (empty? (<vector>) vector*)))
      (get-value (<vector>) vector* (1- <vsize>)))
       

    (declaim (inline get-value))
    (defun get-value (vector* index &optional default)
      (declare (type (vector* <t>) vector*)
               (type ufixnum index)
               (ignore default))
      (check-if-safe (< index (size (<vector>) vector*)))
      (aref <vdata> index))
    
    
    ;; modifiers

    (defun clear (vector*)
    "Remove all elements from VECTOR*. Return VECTOR*"
      (declare (type <vector> vector*))
      (setf <vsize> 0)
      vector*)

    
    (declaim (inline set-value))
    (defun set-value (vector* index value)
      (declare (type (vector* <t>) vector*)
               (type ufixnum index)
               (type <t> value))
      (check-if-safe (< index (size (<vector>) vector*)))
      (setf (aref <vdata> index) value))
    

    (declaim (inline pop-front))
    (defun pop-front (vector*)
      "If VECTOR* is not empty, erase its first element and return it.
Otherwise *may* signal an error.
Warning: this function is slow, i.e. takes O(N) time"
      (values (erase (<vector>) vector* 0)))


    (defun pop-back (vector*)
      "If VECTOR* is not empty, erase its last element and return it.
Otherwise *may* signal an error"
      (declare (type <vector> vector*))
      (check-if-safe (not (empty? (<vector>) vector*)))
      (aref <vdata> (decf <vsize>)))


    (declaim (inline push-front))
    (defun push-front (vector* element)
      "Insert ELEMENT before the beginning of COLLECTION. Return ELEMENT.
Warning: this function is slow, i.e. takes O(N) time"
      (values (insert (<vector>) vector* 0 element)))


    (defun push-back (vector* element)
      "Append ELEMENT at the end of VECTOR*. Return ELEMENT"
      (declare (type <vector> vector*)
               (type <t> element))
      (let* ((data     <vdata>)
             (old-size <vsize>)
             (new-size (ufixnum+ 1 old-size)))
        (cond
          ((<= new-size (array-dimension data 0))
           (setf <vsize> new-size
                 (aref data old-size) element))
          (t
           (resize (<vector>) vector* new-size)
           (set-value (<vector>) vector* old-size element)))))


    (declaim (inline erase^))
    (defun erase^ (vector* iterator)
      "Erase element at ITERATOR in VECTOR*.
Return iterator following the removed element."
      (erase (<vector>) vector* (iterator-cursor (<t>) iterator))
      iterator)


    (defun erase (vector* index &optional default)
      "Erase element at INDEX in VECTOR*.
Return (values VALUE T) where VALUE is the removed element."
      (declare (type <vector> vector*)
               (type ufixnum  index)
               (ignore default))
      (check-if-safe (< index <vsize>))
      (let* ((data <vdata>)
             (size <vsize>)
             (size-1 (1- size))
             (element (aref data index)))
        (loop :while (< index size-1) :do
           (setf (aref data index) (aref data (incf index))))
        (setf <vsize> size-1)
        (values element t)))


    (defun insert (vector* index value)
      "Insert VALUE at INDEX in VECTOR*. Return (values VALUE T)."
      (declare (type <vector> vector*)
               (type ufixnum  index))
      (check-if-safe (<= index <vsize>))
      (let ((cursor (ufixnum+1 <vsize>)))
        (declare (type ufixnum cursor))
        (if (<= cursor (capacity (<vector>) vector*))
            (setf <vsize> cursor)
            (resize (<vector>) vector* cursor))

        (let ((data <vdata>))
          (loop :while (> cursor index) :do
             (let* ((cursor-1 (ufixnum-1 cursor))
                    (e (aref data cursor-1)))
               (setf (aref data cursor) e
                     cursor cursor-1)))
          (setf (aref data cursor) value))
        (values value t)))
    ))
      



