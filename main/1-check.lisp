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

CHECK-IF-SAFE

|#


(in-package :cl-parametric-types)

(declaim (type (or symbol (function (string &rest t) t)) *check-failed-function*))
(defvar *check-failed-function* 'error)

(defun check-failed (caller form &rest args)
  (funcall (or *check-failed-function* 'error)
           "~S: check failed: ~A~%with:~A" caller form
           (with-output-to-string (s)
             (loop :while args :do
                (format s "~%  ~A = ~S" (pop args) (pop args))))))


(defun is-template-function? (func-name)
  (when (symbolp func-name)
    (let ((definition
           (get-definition 'template-function func-name nil)))
      (eq 'defun (car (fourth definition))))))
    
(defun is-function? (func-name)
  (eq :function (introspect-environment:function-information func-name)))
    

(defmacro check-if-safe (expr &environment env)
  "If SPEED < SAFETY, signal an error if EXPR evaluates to NIL.
If SPEED >= SAFETY, do nothing"
  (when (< (introspect-environment:policy-quality 'speed env)
           (introspect-environment:policy-quality 'safety env))
    (let ((args nil))
      (when (consp expr)
        (let ((func-name (first expr)))
          (cond
            ((is-function? func-name)
             (setf args (cdr expr)))
            ((is-template-function? func-name)
             ;; first argument of template functions is (<t> ...), skip it
             (setf args (cddr expr)))
            (t 
             ;; do not evaluate the arguments of a macro
             nil))))
      
      `(unless ,expr
         (check-failed (current-function) ',expr
                       ,@(loop :for arg :in args
                            :collect `',arg
                            :collect arg))))))
    
