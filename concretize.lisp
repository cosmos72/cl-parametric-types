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

(defun recurse-first-atom (thing)
  (loop :while (consp thing) :do
     (setf thing (first thing)))
  thing)

(defun first-atom (thing)
  (if (consp thing)
      (first thing)
      thing))

(defun lambda-list->args (lambda-list)
  (declare (type list lambda-list))
  (loop :for e :in lambda-list
     :for arg = (first-atom e)
     :unless (member arg lambda-list-keywords)
     :collect arg))

(defconstant +most-positive-fixnum-is-power-of-2-minus-1+
  (let ((n most-positive-fixnum))
    (zerop (logand n (1+ n)))))

(defconstant +fixnum-is-twos-complement+
  (eql (lognot most-negative-fixnum) most-positive-fixnum))

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
              `(integer ,lo)))) ;; (integer n *) -> (integer n)
      
      (when (is-power-of-2-minus-1? hi)
        (when (eql 0 lo)
          (return (normalize-unsigned-byte-type `(unsigned-byte ,(integer-length hi)))))
        (when (eql (lognot lo) hi)
          (return (normalize-signed-byte-type `(signed-byte ,(1+ (integer-length hi)))))))
         
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
       (otherwise     #+(and) type
                      ;; redundant recursion, (mangle) already calls (typexpand)
                      ;; on each template argument
                      #-(and)
                      (loop :for e :in type
                         :collect (normalize-type e)))))))

(defun typexpand-1 (type &optional env)
  (declare (type (or symbol cons) type))
  (normalize-type (introspect-environment:typexpand-1 type env)))

(defun typexpand (type &optional env)
  (declare (type (or symbol cons) type))
  (normalize-type (introspect-environment:typexpand type env)))



(defun mangled-simple-type-name? (name)
  (declare (type string name))
  (block fun
    (let* ((len (length name))
	   (len>0 (plusp len))
	   (len-1 (1- len))
	   (start< (and len>0 (char= #\< (char name 0))))
	   (end>   (and len>0 (char= #\> (char name len-1))))
	   (have<> (or start< end>))
	   (quotes nil)
	   (others nil)
	   (depth  0))
      (declare (type fixnum depth))
      (unless (and len>0 (eq start< end>))
	(return-from fun nil))

      (loop
	 :for i fixnum = 0 :then (1+ i)
	 :while (< i len) :do
	 (let ((ch (char name i)))
	   (cond
	     (quotes
	      ;; last char must be #\]
	      (when (= i len-1)
		(unless (char= ch #\])
		  (return-from fun nil)))
	      (case ch
		(#\] (setf quotes nil))
		(#\% (incf i)))) ;; treat next character literally

	     (t ;; no quotes
	      (case ch
		(#\[ (when (= i (1- len))
		       (return-from fun nil))
		     ;; start quoting
		     (setf quotes t))
		((#\% #\]) (return-from fun nil))
		(#\< (incf depth)
		     (setf have<> t))
		;; < and > must nest correctly
		(#\> (decf depth)
		     (setf have<> t)
		     (when (and (<= depth 0)
				(< i (1- len)))
		       (return-from fun nil)))
		;; dots are allowed only inside <>
		(#\. (when (zerop depth)
		       (return-from fun nil)))
		;; other characters are allowed only inside <>
		;; or if there are no <> at all
		(t (when (zerop depth)
		     (setf others t))))))
	   (when (and have<> others)
	     (return-from fun nil))))
      (zerop depth))))
	     		   

(defun mangle-simple-type-name (name)
  (declare (type string name))
  (if (mangled-simple-type-name? name)
      name
      (with-output-to-string (s)
	(princ #\[ s)
	(loop :for ch :across name :do
	   (case ch
	     ((#\[ #\] #\%) (princ #\% s)))
	   (princ ch s))
	(princ #\] s))))


(declaim (ftype (function (symbol) (values string &optional))
		mangle-simple-type)
	 (ftype (function (cons) (values string &optional))
		mangle-cons-type)
	 (ftype (function ((or cons symbol rational)) (values string &optional))
		mangle-any-type))
	 

(defun mangle-simple-type (type)
  (declare (type symbol type))
  (mangle-simple-type-name (symbol-name type)))


(defun mangle-cons-type (type)
  (declare (type cons type))
  (with-output-to-string (s)
    (princ #\< s)
    (loop :for list :on type :do
       (unless (eq list type)
	 (princ #\. s))
       (princ (mangle-any-type (first list)) s))
    (princ #\> s)))
    

(defun mangle-any-type (type)
  (declare (type (or cons symbol rational) type))
  (etypecase type
    (cons (mangle-cons-type type))
    (symbol (mangle-simple-type type))
    (rational (format nil "~A" type))))

(defmethod mangle ((kind symbol) (name symbol) actual-types)
  (declare (type list actual-types))
  (setf actual-types (mapcar #'typexpand actual-types))
  (ecase kind
    (template-function
     (concatenate 'string
		  (symbol-name name) "-"
		  (mangle-cons-type actual-types)))
    (template-type
     (mangle-cons-type (cons name actual-types)))))

(defmethod concretize (kind name actual-types)
  (declare (type list actual-types))
  (let ((mangled-name (mangle kind name actual-types)))
    (nth-value 0 (intern mangled-name (symbol-package name)))))

