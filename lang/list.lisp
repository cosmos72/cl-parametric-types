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

(defun proper-list? (list)
  (declare (type list list))
  (loop :while list :do
     (let ((rest (rest list)))
       (unless (listp rest)
         (return-from proper-list? nil))
       (setf list rest)))
  t)

(defun proper-tree? (tree)
  (declare (type list tree))
  (loop :while tree :do
     (let ((first (first tree))
           (rest  (rest  tree)))
       (unless (listp rest)
         (return-from proper-tree? nil))
       (when (consp first)
         (unless (proper-tree? first)
           (return-from proper-tree? nil)))
       (setf tree rest)))
  t)

(defun valid-type-specifier? (type)
  (declare (type (or symbol cons) type))
  (or (symbolp type)
      (proper-tree? type)))

(defun valid-type-specifiers? (type-list)
  (declare (type list type-list))
  (proper-tree? type-list))

(defun lambda-list->params (lambda-list)
  "Remove the lambda-list keywords (&key &optional &rest etc.) and the default values
from a lambda list. Example: (a b &optional (c 1)) -> (a b c)"
  (declare (type list lambda-list))
  (loop :for e :in lambda-list
     :unless (member e lambda-list-keywords)
     :collect (first-atom e)))

(defun lambda-list->params-flags (lambda-list)
  "Return the third element of &optional and &key lambda-list parameters.
Example: (a b &optional (c 1) (d 2 d?)) -> (d?)"
  (declare (type list lambda-list))
  (loop :for e :in lambda-list
     :when (and (consp e) (cddr e))
     :collect (third e)))

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
