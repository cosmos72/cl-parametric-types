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

TREE-FIND and MULTI-SUBST

|#


(in-package :cl-parametric-types.lang)


(defun tree-find (tree item &key key test test-not)
  (declare (type (or function symbol) key test test-not))
  (flet ((to-function (name)
           (etypecase name
             (symbol (fdefinition name))
             (function name))))
    
    (let ((key  (if key (to-function key)))
          (test (cond
                  (test     (to-function test))
                  (test-not
                   (setf test-not (to-function test-not))
                   (lambda (arg1 arg2)
                     (not (funcall (the function test-not) arg1 arg2))))
                  (t        #'eql))))
      (labels ((%tree-find (node)
                 (when (funcall test (if key (funcall key node) node) item)
                   (return-from %tree-find node))
                 (when (consp node)
                   (or (%tree-find (car node))
                       (%tree-find (cdr node))))))
        (%tree-find tree)))))


(defun multi-subst (values args tree &optional quote-symbol)
  (declare (type list values args)
           (type atom quote-symbol))
  (let ((item tree))
    (cond
      ((atom item)
       (let ((pos (position item args)))
         (if pos
             (let ((replace-values (when pos (nthcdr pos values))))
               (if replace-values
                   (first replace-values)
                   t))
             item)))
      ;; item is a CONS
      ((and quote-symbol (eql quote-symbol (car item)))
       ;; unquote
       (check-type (cddr item) null)
       (second item))
      (t
       (cons (multi-subst values args (car item) quote-symbol)
             (multi-subst values args (cdr item) quote-symbol))))))


#|
(defun multi-subst (values args tree)
  (declare (type list values args tree)
           (type symbol quote-symbol))
  (setf tree (nsubst (pop values) (pop args) (copy-tree tree)))
  (loop :for arg :in args
     :for value = (if values (pop values) t)
     :do (nsubst value arg tree))
  tree)
|#
