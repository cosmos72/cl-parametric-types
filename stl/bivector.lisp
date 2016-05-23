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

(template (&optional (<t> t))
  (defstruct bivector
    (data  (make-array 0 :element-type '<t>) :type (simple-array-1 <t>))
    (start 0 :type ufixnum)
    (end   0 :type ufixnum))

  ;; TEMPLATE DEFSTRUCT above defines MAKE-BIVECTOR and does not yet support :CONSTRUCTOR,
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
    
  (defun biref (bivector index)
    (declare (type (bivector <t>) bivector)
             (type ufixnum index))
    (aref (bivector-data (<t>) bivector)
          (the ufixnum (+ index (bivector-start (<t>) bivector)))))

  (defun set-biref (bivector index value)
    (declare (type (bivector <t>) bivector)
             (type ufixnum index)
             (type <t> value))
    (setf (aref (bivector-data (<t>) bivector)
                (the ufixnum (+ index (bivector-start (<t>) bivector))))
          value)))


(defsetf biref set-biref)


(template (&optional (<t> t))
  (:specialized-for ((bivector <t>)))

  (defstruct (iterator (:include (bivector <t>)))
    (cursor 0 :type ufixnum))

  (declaim (inline empty?))
  (defun empty? (bivector)
    (declare (type (bivector <t>) bivector))
    (>= (bivector-start (<t>) bivector)
        (bivector-end (<t>) bivector)))

  (declaim (inline size))
  (defun size (bivector)
    (declare (type (bivector <t>) bivector))
    (the ufixnum
         (- (bivector-end (<t>) bivector)
            (bivector-start (<t>) bivector)))))


(template (&optional (<t> t))
  
  (defun bivector-mkiter (bivector index)
    (declare (type (bivector <t>) bivector)
             (type ufixnum index))
    (let ((start (bivector-start (<t>) bivector)))
      (make-iterator ((bivector <t>))
                     :data   (bivector-data (<t>) bivector)
                     :start  start
                     :end    (bivector-end (<t>) bivector)
                     :cursor (the ufixnum (+ start index)))))

  (declaim (inline bivector-mkbegin))
  (defun bivector-mkbegin (bivector)
    (declare (type (bivector <t>) bivector))
    (bivector-mkiter (<t>) bivector 0))

  (defun bivector-mkend (bivector)
    (declare (type (bivector <t>) bivector))
    (let ((end (bivector-end (<t>) bivector)))
      (make-iterator ((bivector <t>))
                     :data   (bivector-data (<t>) bivector)
                     :start  (bivector-start (<t>) bivector)
                     :end    end
                     :cursor end))))


(alias ((<bivector> (bivector <t>))
        (<iterator> (iterator (bivector <t>))))
  (template (&optional (<t> t))
    (:specialized-for ((bivector <t>)))


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
      (>= (iterator-end    (<bivector>) iterator)
          (iterator-cursor (<bivector>) iterator)))
          

    (defun back^ (bivector)
      "If BIVECTOR is not empty, return iterator to last element.
Otherwise return END^"
      (declare (type <bivector> bivector))
      (let ((size (size (<bivector>) bivector)))
        (if (zerop size)
            (end^ (<bivector>) bivector)
            (bivector-mkiter (<t>) bivector (1- size)))))
          

    (defun front (bivector &optional default-key default-value)
      "If BIVECTOR is not empty, return (values KEY KEY T)
where KEY is the first entry in BIVECTOR.
Otherwise return (values DEFAULT-KEY DEFAULT-VALUE NIL)"
      (declare (type <bivector> bivector))
      (let ((start (bivector-start (<t>) bivector))
            (end   (bivector-end   (<t>) bivector)))
        (if (<= start end)
            (let* ((data (bivector-data (<t>) bivector))
                   (key  (aref (the (simple-array-1 <t>) data) start)))
              (values key key t))
            (values default-key default-value nil))))
            

    (defun back (bivector &optional default-key default-value)
      "If BIVECTOR is not empty, return (values KEY KEY T)
where KEY is the last entry in BIVECTOR.
Otherwise return (values DEFAULT-KEY DEFAULT-VALUE NIL)"
      (declare (type <bivector> bivector))
      (let ((start (bivector-start (<t>) bivector))
            (end   (bivector-end   (<t>) bivector)))
        (if (<= start end)
            (let* ((data (bivector-data (<t>) bivector))
                   (key  (aref (the (simple-array-1 <t>) data) (1- end))))
              (values key key t))
            (values default-key default-value nil))))))

       

        
