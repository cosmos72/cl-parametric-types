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

(in-package #:cl-parametric-types.stl)

;;;; ** Generic sorted binary tree
;;;; For a red-black tree implementation, see red-black-tree.lisp
;;;; For a sorted-map implementation, see sorted-map.lisp
;;;; For a sorted-set implementation, see sorted-set.lisp

(template (<k> <v>)
  (defstruct (binary-node (:include (pair <k> <v>)))
    (left   nil :type (or null name!)) ;; NAME! is an ugly wart, but (binary-node <k> <v>)
    (right  nil :type (or null name!)) ;; currently causes infinite recursion...
    (parent nil :type (or null name!))))


(template (<k> <v> &optional (<less> (instantiate-function 'less `(,<k>))))
  (defstruct binary-tree
    (root   nil :type (or null (binary-node <k> <v>)))
    (count  0   :type fixnum)))


(template (<k> <v> &optional (<less> (instantiate-function 'less `(,<k>))))
  (:specialized-for ((binary-tree <k> <v> <less>)))
  (deftype iterator () '(binary-node <k> <v>)))


(template (<k> <v> &optional (<less> (instantiate-function 'less `(,<k>))))
  (:specialized-for ((binary-tree <k> <v> <less>)))
  (declaim (inline clear))
  (defun clear (tree)
    "Remove all elements from TREE. Return TREE."
    (setf (binary-tree-root  (<k> <v> <less>) tree) nil
	  (binary-tree-count (<k> <v> <less>) tree) 0)
    tree))


(template (<k> <v> &optional (<less> (instantiate-function 'less `(,<k>))))
  (:specialized-for ((binary-tree <k> <v> <less>)))
  (declaim (inline empty?))
  (defun empty? (tree)
    "Return t if TREE is empty, otherwise return nil."
    (declare (type (binary-tree <k> <v> <less>) tree))
    (null (binary-tree-root (<k> <v> <less>) tree))))


(template (<k> <v> &optional (<less> (instantiate-function 'less `(,<k>))))
  (:specialized-for ((binary-tree <k> <v> <less>)))
  (declaim (inline size))
  (defun size (tree)
    "Return number of elements in TREE."
    (declare (type (binary-tree <k> <v> <less>) tree))
    (binary-tree-count (<k> <v> <less>) tree)))


(template (<k> <v> &optional (<less> (instantiate-function 'less `(,<k>))))
  (:specialized-for ((binary-tree <k> <v> <less>)))
  (declaim (notinline lookup))
  (defun lookup (tree key)
    "Return iterator-to-key if TREE contains KEY, otherwise return NIL."
    (declare (type (binary-tree <k> <v> <less>) tree)
	     (type <k> key))
    (alias ((<kv>  (<k> <v>))
	    (<kvl> (<k> <v> <less>)))
      (let ((node (binary-tree-root <kvl> tree)))
	(loop :named loop :while node :do
	   (let ((xkey (binary-node-first <kv> node)))
	     (cond
	       ((<less> key xkey) (setf node (binary-node-left  <kv> node)))
	       ((<less> xkey key) (setf node (binary-node-right <kv> node)))
	       (t
		(return-from loop node)))))))))


(template (<k> <v> &optional (<less> (instantiate-function 'less `(,<k>))))
  (:specialized-for ((binary-tree <k> <v> <less>)))
  (defun set-value (tree key value)
  "Add KEY to TREE if not present, then associate KEY to VALUE in TREE.
Return VALUE."
  (declare (type (binary-tree <k> <v> <less>) tree)
	   (type <k> key)
	   (type <v> value))
  (alias ((<kv>  (<k> <v>))
	  (<kvl> (<k> <v> <less>)))
    (block set-value
      (let ((parent nil)
	    (node   (binary-tree-root <kvl> tree))
	    (left   t))
	(loop :while node :do
	   (let ((xkey (binary-node-first <kv> node)))
	     (cond
	       ((<less> key xkey)
		(setf parent node
		      node   (binary-node-left  <kv> node)
		      left   t))
	       ((<less> xkey key)
		(setf parent node
		      node   (binary-node-right <kv> node)
		      left   nil))
	       (t
		(return-from set-value
		  (setf (second <kv> node) value))))))))
    
    ;; no such key, create node for it
    (setf node (make-binary-node <kv> :first key :second value :parent parent))
    (incf (binary-tree-count <kvl> tree))
    (if parent
	(if left
	    (setf (binary-node-left  <kl> parent) node)
	    (setf (binary-node-right <kl> parent) node))
	;; binary-tree is empty
	(setf (binary-tree-root <kvl> root) node))
    
    ;; (rebalance-after-insert <kvl> tree node)
    value)))



(template (<t>)
  (:specialized-for ((binary-tree <k> <v> <less>)))
  (defun rem-value (map key &optional default)
    (declare (type (binary-tree <k> <v> <less>) tree))
    ;; TODO
  ))


(template (<t>)
  (:specialized-for ((binary-tree <k> <v> <less>)))
  (defun begin (tree)
    "Return iterator to the first KEY in TREE,
or NIL if TREE is empty"
    (declare (type (binary-tree <k> <v> <less>) tree))
    (alias ((<kv>  (<k> <v>))
	    (<kvl> (<k> <v> <less>)))
      (let ((node (binary-tree-root <kvl> root)))
	(when node
	  (loop :for child = (binary-node-left <kv> node) :do
	     (if child
		 (setf node child)
		 (loop-finish))))
	node))))


(template (<t>)
  (:specialized-for ((binary-tree <k> <v> <less>)))
  (defun first (tree &optional default-key default-value)
    "If tree is not empty, return (values KEY VALUE T) where KEY is the smallest key in TREE.
Otherwise return (values DEFAULT-KEY DEFAULT-VALUE NIL)"
    (declare (type (binary-tree <k> <v> <less>) tree))
    (let ((iter (begin (<k> <v> <less>) tree)))
      (if iter
	  (values (binary-node-first  (<k> <v>) node)
		  (binary-node-second (<k> <v>) node)
		  t)
	  (values default-key default-value nil)))))




;;;; ** Helper functions used by subclasses
         
#|
(declaim (inline is-left-binary-node-child?))
(defun is-left-binary-node-child? (node parent)
  (declare (type binary-node node parent))
  (eq node (_ parent left)))


(defun rotate-binary-node-left (node)
  "Rotate left the subtree around node. Return new subtree root."
  (declare (type binary-node node))
  (log.debug "before:~%~A" (print-object-contents nil node))
  (let1 x (_ node right)
    (setf (_ node right) (_ x left))
    (setf (_ x left)     node)
    (log.debug "after:~%~A" (print-object-contents nil x))
    x))


(defun rotate-binary-node-right (node)
  "Rotate right the subtree around node. Return new subtree root."
  (declare (type binary-node node))
  (log.debug "before:~%~A" (print-object-contents nil node))
  (let1 x (_ node left)
    (setf (_ node left)  (_ x right))
    (setf (_ x right)    node)
    (log.debug "after:~%~A" (print-object-contents nil x))
    x))



(defun rotate-binary-node-around (node parent &key left)
  "Rotate left or right the subtree around node. Return new subtree root
and also update parent's link to subtree root."
  (declare (type binary-node node)
           (type (or null binary-node) parent)
           (type boolean left))
  
  (let1 new-node
      (if left
          (rotate-binary-node-left node)
          (rotate-binary-node-right node))

    ;; connect parent to rotated node
    (when parent
      (if (is-left-binary-node-child? node parent)
          (setf (_ parent left) new-node)
          (setf (_ parent right) new-node)))
    new-node))


(defun replace-binary-node (old-node new-node parent)
  "Unlink old-node from it parent and replace it with new-node.
Return t if left child was replaced, nil if right child was replaced"
  (declare (type (or null binary-node) old-node new-node parent))
  (when parent
    (let1 left-child? (is-left-binary-node-child? old-node parent)
      (if left-child?
          (setf (_ parent left) new-node)
          (setf (_ parent right) new-node))
      left-child?)))
|#
