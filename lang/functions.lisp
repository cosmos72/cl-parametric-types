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

CURRENT-FUNCTION

|#


(in-package :cl-parametric-types.lang)

(deftype package-designator () '(or symbol string package))
(deftype symbol-designator () '(or symbol string))


(defun find-package* (package)
  (declare (type package-designator package))
  (if (packagep package)
      package
      (find-package package)))


(defun find-symbol* (package name)
  (declare (type package-designator package)
           (type symbol-designator name))
  (let ((package (find-package* package)))
    (when package
      (let ((name (if (symbolp name)
                      (symbol-name name)
                      name)))
        (values (find-symbol name package))))))


(defun find-function-or-macro (package name)
  (declare (type package-designator package)
           (type symbol-designator name))
  (let ((name (find-symbol* package name)))
    (handler-case
        (progn
          (symbol-function name)
          name)
      (condition () nil))))


(defun is-function? (func-name)
  (declare (type symbol func-name))
  (when (not (macro-function func-name))
    (handler-case
        (let ((function (symbol-function func-name)))
          (typep function 'function))
      (condition () nil))))


(defmacro current-function (&environment env)
  (let* ((callers (log4cl:enclosing-scope-block-name *package* env))
         (caller (cond
                   ((atom callers) callers)
                   ((rest callers) callers)
                   (t       (first callers)))))
    `',caller))

