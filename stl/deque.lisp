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

(alias ((<deque> (deque <t>))
        (<qdata>    (deque-data  (<t>) deque))
        (<qstart>   (deque-start (<t>) deque))
        (<qend>     (deque-end   (<t>) deque))

        (<iterator> (iterator (deque <t>)))
        (<idata>    (iterator-data   ((deque <t>)) iterator))
        (<istart>   (iterator-start  ((deque <t>)) iterator))
        (<iend>     (iterator-end    ((deque <t>)) iterator))
        (<icursor>  (iterator-cursor ((deque <t>)) iterator)))

  (template (&optional (<t> t))

    (defstruct deque
      "
A template-struct implementing resizeable, one-dimensional array with O(1)
random access to elements and amortized O(1) insertion/removal of elements
at both ends.

Resizing is implemented by allocating the twice size of array, then copying
the pointer to the elements."
      (data  (make-array 0 :element-type '<t>) :type (simple-array-1 <t>))
      (start 0 :type ufixnum)
      (end   0 :type ufixnum))


    ;; TEMPLATE DEFSTRUCT above defines MAKE-DEQUE, and does not yet support :CONSTRUCTOR,
    ;; so we must pick a different name for the public constructor :(
    (declaim (notinline new-deque))
    (defun new-deque (&key (initial-size 0) (initial-capacity 0)
                        (initial-element nil initial-element?)
                        (initial-contents nil initial-contents?))
      (let* ((size     (if initial-contents? (length initial-contents) initial-size))
             (capacity (max size initial-capacity))
             (start    (ash (- capacity size) -1))
             (end      (+ size start))
             (data     (if initial-element?
                           (make-array capacity
                                       :element-type '<t>
                                       :initial-element initial-element)
                           (make-array capacity
                                       :element-type '<t>))))
        (when initial-contents?
          (dotimes (i size)
            (setf (aref data (+ i start)) (aref initial-contents i))))
        
        (make-deque (<t>) :data data :start start :end end))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (template (&optional (<t> t))
    (:specialized-for ((deque <t>)))

    (defmacro new (&rest args)
      "NEW specialization for (DEQUE <T>)"
      `(new-deque (<t>) ,@args))

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
             (= (ufixnum- xend xstart)
                (ufixnum- yend ystart))
               
             (let ((xdata  (deque-data  (<t>) x))
                   (ydata  (deque-data  (<t>) y)))

               (loop :for xi :from xstart :below xend
                  :for yi :from ystart
                  :always
                  (equal-to (<t>) (aref xdata xi) (aref ydata yi)))))))))
      
    
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


    ;; size and capacity

    (declaim (inline empty?))
    (defun empty? (deque)
      (declare (type <deque> deque))
      (>= <qstart> <qend>))

    
    (declaim (inline size))
    (defun size (deque)
      (declare (type <deque> deque))
      (ufixnum- <qend> <qstart>))
    

    (declaim (inline capacity))
    (defun capacity (deque)
      (declare (type <deque> deque))
      (array-dimension <qdata> 0))

    
    (defun resize (deque new-size &key (at :back))
      "Set the DEQUE size. Return new size."
      (declare (type <deque> deque)
               (type ufixnum new-size)
               (type (member :front :back) at))

      (let* ((old-start    <qstart>)
             (old-end      <qend>)
             (old-size     (ufixnum- old-end old-start))
             (new-start    old-start)
             (required (the fixnum (- new-size old-size)))) ;; may be negative

        (when (plusp required)
          (let* ((old-capacity (capacity (<deque>) deque))
                 (available
                  (if (eq at :front)
                      old-start
                      (ufixnum- old-capacity old-end))))
                   
            (when (> required available)
              (let ((new-capacity
                     (if (<= old-capacity (truncate array-dimension-limit 2))
                         #+nil (* old-capacity 2)
                         (ash old-capacity 1) ;; twice the size
                         array-dimension-limit)))
                (reserve (<deque>) deque
                         (max 4 new-size new-capacity)
                         :at at))
              (setf new-start <qstart>))))

        (if (eq at :front)
            (setf <qstart> (the ufixnum (- new-start required)))
            (setf <qend>   (the ufixnum (+ new-start new-size)))))
      new-size)
    

    (defun reserve (deque new-capacity &key (at :both))
      "Set the DEQUE capacity. Return new capacity.
Does not alter deque size, and minimum capacity is the size."
      (declare (type <deque> deque)
               (type ufixnum new-capacity)
               (type (member :front :back :both) at))
      (let* ((old-data     <qdata>)
             (old-start    <qstart>)
             (old-capacity (array-dimension old-data 0))
             (size         (ufixnum- <qend> old-start))
             ;; allocate space for (- NEW-CAPACITY OLD-CAPACITY) elements
             ;; either at :front or :back. A little extra spare capacity
             ;; is reserved at the other end too.
             (asked-capacity (max new-capacity size))
             (extra-capacity (if (eq at :both)
                                 0
                                 (min #+nil (floor old-capacity 4)
                                      (ash asked-capacity -2)
                                      (ufixnum- array-dimension-limit asked-capacity))))
             (new-capacity (ufixnum+ asked-capacity extra-capacity)))
            
        (unless (= old-capacity asked-capacity)
          (let* ((new-data     (make-array new-capacity :element-type '<t>))
                 (new-start    (case at
                                 (:front (ufixnum- asked-capacity size))
                                 (:back  extra-capacity)
                                 (t      (ash (ufixnum- new-capacity size) -1)))))
            (dotimes (i size)
              ;; this one is ignoring the resizable array in default cl --- performance comparison required?
              (setf (aref new-data (ufixnum+ i new-start))
                    (aref old-data (ufixnum+ i old-start))))
            (setf <qdata>  new-data
                  <qstart> new-start
                  <qend>   (ufixnum+ new-start size))))
        asked-capacity))


    ;; iterators

    
    (defun at^ (deque index)
      (declare (type <deque> deque)
               (type ufixnum index))
      (let ((start <qstart>))
        (make-iterator (<deque>)
                       :data   <qdata>
                       :start  start
                       :end    <qend>
                       :cursor (ufixnum+ start index))))
    

    (declaim (inline begin^))
    (defun begin^ (deque)
      (declare (type <deque> deque))
      (at^ (<deque>) deque 0))
    

    (declaim (inline end^))
    (defun end^ (deque)
      (declare (type <deque> deque))
      (let ((end <qend>))
        (make-iterator (<deque>)
                       :data   <qdata>
                       :start  <qstart>
                       :end    end
                       :cursor end)))


    (declaim (inline valid-iter?))
    (defun valid-iter? (iterator)
      (declare (type <iterator> iterator))
      (let ((cursor <icursor>))
        (and
         (>= cursor <istart>)
         (<  cursor <iend>))))


    (declaim (inline iter-key))
    (defun iter-key (iterator)
      "Return the KEY pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (declare (type <iterator> iterator))
      (check-if-safe (valid-iter? (<deque>) iterator))
      (aref <idata> <icursor>))

    
    (declaim (inline set-iter-key))
    (defun set-iter-key (iterator value)
      "Set the VALUE pointed to by ITERATOR.
If ITERATOR is not valid, *may* signal an error"
      (declare (type <iterator> iterator)
               (type <t> value))
      (check-if-safe (valid-iter? (<deque>) iterator))
      (setf (aref <idata> <icursor>) value))

    
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


    ;; lookup

    
    (defun front (deque)
      "If DEQUE is not empty, return its first element.
Otherwise *may* signal an error"
      (declare (type <deque> deque))
      (check-if-safe (not (empty? (<deque>) deque)))
      (aref <qdata> <qstart>))

    
    (defun back (deque)
      "If DEQUE is not empty, return its last element.
Otherwise *may* signal an error"
      (declare (type <deque> deque))
      (check-if-safe (not (empty? (<deque>) deque)))
      (aref <qdata> (ufixnum-1 <qend>)))
       

    (declaim (inline get-value))
    (defun get-value (deque index)
      (declare (type (deque <t>) deque)
               (type ufixnum index))
      (let ((start <qstart>))
        (check-if-safe (>= index start))
        (check-if-safe (< index <qend>))
        (aref <qdata> (ufixnum+ index start))))
    
    
    ;; modifiers


    (defun clear (deque)
      "Remove all elements from DEQUE. Return DEQUE"
      (declare (type <deque> deque))
      (let ((capacity/2 (ash (capacity (<deque>) deque) -1)))
        (setf <qstart> capacity/2
              <qend>   capacity/2)
        deque))
      

    (declaim (inline set-value))
    (defun set-value (deque index value)
      (declare (type (deque <t>) deque)
               (type ufixnum index)
               (type <t> value))
      (check-if-safe (< index (size (<deque>) deque)))
      (setf (aref <qdata> (ufixnum+ index <qstart>)) value))
    

    (defun pop-front (deque)
      "If DEQUE is not empty, erase its first element and return it.
Otherwise *may* signal an error"
      (declare (type <deque> deque))
      (check-if-safe (not (empty? (<deque>) deque)))
      (let ((start <qstart>))
        (prog1
            (aref <qdata> start)
          (setf <qstart> (ufixnum+ start 1)))))


    (defun pop-back (deque)
      "If DEQUE is not empty, erase its last element and return it.
Otherwise *may* signal an error"
      (declare (type <deque> deque))
      (check-if-safe (not (empty? (<deque>) deque)))
      (aref <qdata> (decf <qend>)))


    (defun push-front (deque element)
      "Insert ELEMENT before the beginning of DEQUE. Return ELEMENT"
      (declare (type <deque> deque)
               (type <t> element))
      (let ((old-start <qstart>))
        (cond
          ((> old-start 0)
           (decf old-start)
           (setf <qstart> old-start
                 (aref <qdata> old-start) element))
          (t
           (let* ((old-end <qend>)
                  (old-size (ufixnum- old-end old-start))
                  (new-size (ufixnum+1 old-size)))
             (resize (<deque>) deque new-size :at :front)
             (set-value (<deque>) deque 0 element))))))


    (defun push-back (deque element)
      "Append ELEMENT at the end of DEQUE. Return ELEMENT"
      (declare (type <deque> deque)
               (type <t> element))
      (let* ((data     <qdata>)
             (old-end  <qend>)
             (new-end  (ufixnum+ 1 old-end)))
        (cond
          ((<= new-end (array-dimension data 0))
           (setf <qend> new-end
                 (aref data old-end) element))
          (t
           (let* ((old-start <qstart>)
                  (old-size (ufixnum- old-end old-start))
                  (new-size (ufixnum+1 old-size)))
             (resize (<deque>) deque new-size :at :back)
             (set-value (<deque>) deque old-size element))))))
    
    ))



