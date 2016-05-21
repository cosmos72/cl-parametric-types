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

#|

Convert abstract type names, as for example (PAIR BIT FIXNUM)
into concrete type names, as for example <PAIT.BIT.FIXNUM>

|#


(in-package #:cl-parametric-types)

(defvar *concretize* t)

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
		   &key (simplify t))
  (declare (type list actual-types))
  (when simplify
    (setf actual-types (simplify-typexpand-list actual-types)))
  (values
   (ecase kind
     (template-type
      (mangle-cons-type (cons name actual-types)))
     ((template-function template-constructor)
      ;; we want functions with a single template argument have simple names,
      ;; and we want to mangle template-struct constructors as
      ;; (MAKE ((PAIR A B)) ...)) to (MAKE-<PAIR.A.B> ...)
      ;; because it is exactly what DEFSTRUCT produces :)
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


(defmethod concretize (kind name actual-types &key (simplify t))
  (declare (type list actual-types))

  (unless *concretize*
    (when simplify
      (setf actual-types (simplify-typexpand-list actual-types)))
    (return-from concretize (cons name actual-types)))
  
  (multiple-value-bind (mangled-name actual-types*)
      (mangle kind name actual-types :simplify simplify)
    (let* ((symbol
            (case kind
              ;; for TEMPLATE-ACCESSOR and TEMPLATE-CONSTRUCTOR,
              ;; we must use the package where the struct is defined
              ;; because that's the package where all concrete accessors
              ;; and constructors must be instantiated.
              ;;
              ;; Instead the function itself could be named FIRST or similar,
              ;; which is in package CL
              ((template-accessor template-constructor)
               (recurse-first-atom actual-types))
              (otherwise name)))
           (package (symbol-package symbol)))
      (unless package
        (error "CL-PARAMETRIC-TYPES: symbol ~S is uninterned, i.e. has no home package.
 I have no idea in which package I should instantiate ~S into!"
               symbol (cons name actual-types)))
      (let ((mangled-symbol (intern mangled-name package)))
	(log.trace "~&; concretized ~A name ~S to ~S~&"
		   (kind-name kind) (cons name actual-types) mangled-symbol)
	(values
	 mangled-symbol
	 actual-types*)))))

