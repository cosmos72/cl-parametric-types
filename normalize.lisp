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


(in-package #:cl-parametric-types)

(defconstant +most-positive-fixnum-is-power-of-2-minus-1+
  (let ((n most-positive-fixnum))
    (zerop (logand n (1+ n)))))

(defconstant +fixnum-is-twos-complement+
  (eql (lognot most-negative-fixnum) most-positive-fixnum))

(declaim (ftype (function ((or symbol cons)) (values (or symbol cons) &optional)) normalize-type))
		
(declaim (inline is-power-of-2-minus-1?))
(defun is-power-of-2-minus-1? (n)
  (declare (type integer n))
  (if (typep n 'fixnum)
      (if (< n most-positive-fixnum)
          (zerop (logand n (1+ n)))
          +most-positive-fixnum-is-power-of-2-minus-1+)
      ;; slow bignum arithmetic
      (zerop (logand n (1+ n)))))

(defun normalize-unsigned-byte-type (type)
  (declare (type cons type))
  (let ((bits (or (second type) '*)))
    (case bits
      ((*) '(integer 0)) ;; (unsigned-byte *) -> (integer 0), because the latter is shorter
      (1   'bit)         ;; (unsigned-byte 1) -> bit
      (otherwise  type))))
       
(defun normalize-signed-byte-type (type)
  (declare (type cons type))
  (let ((bits (or (second type) '*)))
    (cond
      ((eq '* bits) 'integer)     ;; (signed-byte *) -> integer, because the latter is shorter
      ((and +most-positive-fixnum-is-power-of-2-minus-1+
            +fixnum-is-twos-complement+
            (eql bits #.(1+ (integer-length most-positive-fixnum))))
       #||#         'fixnum)      ;; (signed-byte #.(1+ (integer-length most-positive-fixnum))) -> fixnum
      (t            type))))

(defun normalize-integer-type (type)
  (declare (type cons type))
  (let ((lo (or (second type) '*))
        (hi (or (third  type) '*)))
    (block nil
      (when (eq '* hi)
        (return
          (if (eq '* lo)
              (first type)      ;; (integer * *) -> integer
              `(,(first type) ,lo)))) ;; (integer n *) -> (integer n)
      
      (when (is-power-of-2-minus-1? hi)
        (when (eql 0 lo)
          (return (normalize-unsigned-byte-type `(unsigned-byte ,(integer-length hi)))))
        (when (eql (lognot lo) hi)
          (return (normalize-signed-byte-type `(signed-byte ,(1+ (integer-length hi)))))))
         
      type)))

(defun normalize-array-type (type)
  (declare (type cons type))
  (let ((array-type   (first type))
	(element-type (or (normalize-type (second type)) '*))
        (dimensions   (or (third  type) '*)))
    (block nil
      (when (eq '* dimensions)
        (return
          (if (eq '* element-type)
              array-type      ;; (array * *) -> array
              (list array-type element-type)))) ;; (array el-type *) -> (array el-type)
      
      (list array-type element-type dimensions))))

(defun normalize-simple-vector-type (type)
  (declare (type cons type))
  (let ((dimensions (or (second type) '*)))
    (if (eq '* dimensions)
	(first type) ;; (simple-vector *) -> simple-vector
	type)))

(defun normalize-type (type)
  (declare (type (or symbol cons) type))
  (etypecase type
    (symbol
      (case type
        ;; signed-byte -> integer, for uniformity
        ;; with (signed-byte *) -> integer above
        (signed-byte   'integer)
        ;; unsigned-byte -> (integer 0), for uniformity
        ;; with (unsigned-byte *) -> (integer 0) above
        (unsigned-byte '(integer 0))
        (otherwise     type)))
    (cons
     (case (first type)
       (integer       (normalize-integer-type       type))
       (signed-byte   (normalize-signed-byte-type   type))
       (unsigned-byte (normalize-unsigned-byte-type type))
       ((array  simple-array  vector)        (normalize-array-type type))
       ((string simple-string simple-vector) (normalize-simple-vector-type type))
       (otherwise     (cons (first type)
			    (loop :for e :in (rest type)
			       :collect (normalize-type e))))))))


(defun typexpand (type &optional env)
  (declare (type (or symbol cons) type))
  #+(or ccl cmucl sbcl)
  (introspect-environment:typexpand type env)
  #+clisp
  (ext:type-expand type env))

  
#-(or ccl clisp cmucl sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "no known implementation of TYPEXPAND on this platform,
  cannot compile CL-PARAMETRIC-TYPES"))

(defun normalize-typexpand-types (types)
  (declare (type list types))
   (loop :for type :in types
      :collect (normalize-type (typexpand type))))
