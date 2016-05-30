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


(declaim (inline first-atom))
(defun first-atom (thing)
  "Returns the first element of a cons, or itself when THING is an atom.
   e.g. (a b) -> a
          a   -> a"
  (if (consp thing)
      (first thing)
      thing))

(defun lambda-list->params (lambda-list)
  "Remove the lamda-list keywords (&key &optional &rest etc.) and the default values
from a lambda list. Example: (a b &optional (c 1)) -> (a b c)"
  (declare (type list lambda-list))
  (loop :for e :in lambda-list
     :for arg = (first-atom e)
     :unless (member arg lambda-list-keywords)
     :collect arg))

(defun lambda-list->args (lambda-list)
  "Convert the lamda-list into a list of arguments for a function call.
Example: (a b &optional (c 1) &key foo) -> (a b c :foo foo).
WARNING: Removes any argument following &rest. Use LAMBDA-LIST->REST to capture them"
  (declare (type list lambda-list))
  (let ((args nil)
        (key? nil))
    (loop :for es :on lambda-list :do
       (let* ((e (first es))
              (arg (first-atom e)))
         (case arg
           (&key (setf key? t))
           (&rest (loop-finish))
           (otherwise
            (unless (member arg lambda-list-keywords)
              (when key?
                (push (intern (symbol-name arg) :keyword) args))
              (push arg args))))))
    (nreverse args)))

(defun lambda-list->rest (lambda-list)
  "Extract the arguments following &rest in lambda-list"
  (rest (member '&rest lambda-list)))
