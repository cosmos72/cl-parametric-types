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

(defmethod mangle ((kind symbol) (name symbol) actual-types
		   &key (normalize t))
  (declare (type list actual-types))
  (when normalize
    (setf actual-types (normalize-typexpand-types actual-types)))
  (values
   (ecase kind
     (template-type
      (mangle-cons-type (cons name actual-types)))
     (template-function
      ;; we want functions with a single template argument to be simple.
      ;; In particular, the following mangles (MAKE ((PAIR A B) ...))
      ;; to (MAKE-<PAIR.A.B> ...) which is exactly what DEFSTRUCT produces :)
      (let ((tokens (loop :for actual-type :in actual-types
                       :collect "-"
                       :collect (mangle-any-type actual-type))))
        (apply #'concatenate 'string (symbol-name name) tokens)))
     (template-accessor
      ;; we also want template-struct accessors to match what DEFSTRUCT produces
      ;; For example, 'FIRST '((PAIR A B)) must become <PAIR.A.B>-FIRST
      (let ((concrete-struct (mangle-any-type (first actual-types))))
        (concatenate 'string concrete-struct "-" (symbol-name name)))))
   actual-types))

(defmethod concretize (kind name actual-types &key (normalize t))
  (declare (type list actual-types))
  (multiple-value-bind (mangled-name actual-types*)
      (mangle kind name actual-types :normalize normalize)
    (let* ((symbol
            (case kind
              ;; for template-accessors, use the package where the struct is defined
              ;; because accessors could be named FIRST or similar, which is in package CL
              (template-accessor (recurse-first-atom actual-types))
              (otherwise name)))
           (package (symbol-package symbol)))
      (values
       (intern mangled-name package)
       actual-types*))))

