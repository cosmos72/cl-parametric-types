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


(template (<k> <v> &optional (<less> (instantiate-function 'less `(,<k>))))
  (defstruct binary-tree
    (root   nil :type (or null (binary-node <k> <v>)))
    (count  0   :type fixnum)))


(alias ((<tree>    (binary-tree <k> <v> <less>))
        (<node>    (binary-node <k> <v>))
        (<kvl>     (<k> <v> <less>))
        (<kv>      (<k> <v>))
        (root-of   binary-tree-root)
        (count-of  binary-tree-count)
        (left-of   binary-node-left)
        (right-of  binary-node-right) 
        (parent-of binary-node-parent))
             
    (template (<k> <v> &optional (<less> (instantiate-function 'less `(,<k>))))
      (:specialized-for (<tree>))

      (deftype iterator () '<node>)

      (declaim (inline clear))
      (defun clear (tree)
        "Remove all elements from TREE. Return TREE."
        (setf (root-of  <kvl> tree) nil
              (count-of <kvl> tree) 0)
        tree)


      (declaim (inline empty?))
      (defun empty? (tree)
        "Return t if TREE is empty, otherwise return nil."
        (declare (type <tree> tree))
        (null (root-of <kvl> tree)))


      (declaim (inline size))
      (defun size (tree)
        "Return number of elements in TREE."
        (declare (type <tree> tree))
        (count-of <kvl> tree))


      (declaim (notinline find-iter))
      (defun find-iter (tree key)
        "If TREE contains KEY then return iterator pointing to KEY, otherwise return NIL."
        (declare (type <tree> tree)
                 (type <k> key))
        (let ((node (root-of <kvl> tree)))
          (loop :named loop :while node :do
             (let ((xkey (binary-node-first <kv> node)))
               (cond
                 ((<less> key xkey) (setf node (left-of  <kv> node)))
                 ((<less> xkey key) (setf node (right-of <kv> node)))
                 (t
                  (return-from loop node)))))))


      (declaim (inline set-value))
      (defun set-value (map key value)
        (declare (type <tree> tree)
                 (type <k> key)
                 (type <v> value))
        (binary-tree-put <kvl> tree key value)
        value)


      (defun rem-value (map key &optional default)
        (declare (type <tree> tree))
        ;; TODO
        )


      (defun first-iter (tree)
        "Return iterator to the first KEY in TREE,
or NIL if TREE is empty"
        (declare (type <tree> tree))
        (let ((node (root-of <kvl> tree)))
          (when node
            (loop :for child = (left-of <kv> node)
               :while child :do
               (setf node child)))
          node))


      (defun last-iter (tree)
        "Return iterator to the last KEY in TREE,
or NIL if TREE is empty"
        (declare (type <tree> tree))
        (let ((node (root-of <kvl> tree)))
          (when node
            (loop :for child = (right-of <kv> node)
               :while child :do
               (setf node child)))
          node)))




    (template (<k> <v> &optional (<less> (instantiate-function 'less `(,<k>))))
      (defun binary-tree-put (tree key value)
        "Add KEY to TREE if not present, then associate KEY to VALUE in TREE.
Return inserted NODE if KEY was not present, otherwise return NIL.
Does *not* rebalance TREE."
        (declare (type <tree> tree)
                 (type <k> key)
                 (type <v> value))
        (block binary-tree-put
          (let ((parent nil)
                (node   (root-of tree))
                (left   t))
            (loop :while node :do
               (let ((xkey (binary-node-first <kv> node)))
                 (cond
                   ((<less> key xkey)
                    (setf parent node
                          node   (left-of <kv> node)
                          left   t))
                   ((<less> xkey key)
                    (setf parent node
                          node   (right-of <kv> node)
                          left   nil))
                   (t
                    (setf (binary-node-second <kv> node) value)
                    (return-from binary-tree-put nil)))))))

      ;; no such key, create node for it
      (setf node (make-binary-node <kv> :first key :second value :parent parent))
      (incf (binary-tree-count <kvl> tree))
      (if parent
          (if left
              (setf (binary-node-left  <kl> parent) node)
              (setf (binary-node-right <kl> parent) node))
          ;; binary-tree is empty
          (setf (root-of tree) node))
      
      ;; (rebalance-after-insert <kvl> tree node)
      node)))

