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

TREE-FIND

|#


(in-package :cl-parametric-types.lang)

(defun recurse-first-atom (thing)
  "Returns the first element of a cons tree. e.g. (((a) b) c) -> a"
  (loop :while (consp thing) :do
     (setf thing (first thing)))
  thing)

(defun first-atom (thing)
  "Returns the first element of a cons, or itself when THING is an atom.
   e.g. (a b) -> a
          a   -> a"
  (if (consp thing)
      (first thing)
      thing))

(defun lambda-list->args (lambda-list)
  "Remove the lamda-list keywords (&key &optional &rest etc.) and the default values
from a lambda list. Example: (a b &optional (c 1)) -> (a b c)
"
  (declare (type list lambda-list))
  (loop :for e :in lambda-list
     :for arg = (first-atom e)
     :unless (member arg lambda-list-keywords)
     :collect arg))


