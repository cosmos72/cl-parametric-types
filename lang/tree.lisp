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


(defun multi-subst (new-list old-list tree &optional quote-symbol eval-symbol env)
  (declare (type list new-list old-list)
           (type atom quote-symbol eval-symbol))
  (labels ((%multi-subst (items)
             (let* ((pos (position items old-list)))
               (when pos
                 (let ((new-items (nthcdr pos new-list)))
                   (return-from %multi-subst (if new-items (first new-items) t))))
               (cond
                 ((atom items) items)
                 ;; items is a CONS
                 ((and quote-symbol (eql quote-symbol (car items)))
                  ;; unquote
                  (check-type (cddr items) null)
                  (second items))
                 ((and eval-symbol (eql eval-symbol (car items)))
                  ;; eval
                  (check-type (cddr items) null)
                  (let ((items (%multi-subst (second items))))
                    (eval items env)))
                 (t
                  (let* ((head (cons nil nil))
                         (tail head))
                    (loop :for subitems :on items :do
                       (let ((new  (%multi-subst (first subitems)))
                             (rest (rest subitems)))
                         (setf (car tail) new)
                         (cond
                           ((consp rest)
                            (let ((cons (cons nil nil)))
                              (setf (cdr tail) cons
                                    tail       cons)))
                           (rest ;; NIL marks end-of-proper-list, ignore it
                            (setf (cdr tail) (%multi-subst rest))))))
                    head))))))
    (%multi-subst tree)))
                  



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
