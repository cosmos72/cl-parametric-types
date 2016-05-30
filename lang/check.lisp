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


(in-package :cl-parametric-types.lang)


(defun check-failed (form &rest args)
  (error "check failed: ~S~%with:~A" form
         (with-output-to-string (s)
           (loop :while args :do
              (format s "~%  ~A = ~S" (pop args) (pop args))))))


(defmacro check-if-safe (expr &environment env)
  "If SPEED < SAFETY, signal an error if EXPR evaluates to NIL.
If SPEED >= SAFETY, do nothing"
  (when (< (introspect-environment:policy-quality 'speed env)
           (introspect-environment:policy-quality 'safety env))
    `(unless ,expr
       (check-failed ',expr ,@(when (consp expr)
                                    (loop :for e :in (rest expr)
                                       :collect `',e
                                       :collect e))))))
    
