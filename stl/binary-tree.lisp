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

;;; Generic sorted binary tree

#|
For a red-black tree implementation, see red-black-tree.lisp
For a sorted-map implementation, see sorted-map.lisp
For a sorted-set implementation, see sorted-set.lisp
|#

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
    (defstruct binary-tree
      (root   nil :type (or null <node>))
      (first  nil :type (or null <node>))
      (last   nil :type (or null <node>))
      (count  0   :type fixnum)))
      

  (template (<k> <v> &optional (<less> (instantiate-function 'less `(,<k>))))
      (:specialized-for (<tree>))

      (deftype iterator () '<node>)



      ;; capacity
      
      (declaim (inline empty?))
      (defun empty? (tree)
        "Return T if TREE is empty, otherwise return NIL."
        (declare (type <tree> tree))
        (null (root-of <kvl> tree)))

      (declaim (inline size))
      (defun size (tree)
        "Return number of elements in TREE."
        (declare (type <tree> tree))
        (count-of <kvl> tree))



      ;; iterators

      (declaim (inline begin^))
      (defun begin^ (tree)
        "If TREE is not empty, return iterator to first element.
Otherwise return END^"
        (declare (type <tree> tree))
        (binary-tree-first <kvl> tree))

      (declaim (inline end^))
      (defun end^ (tree)
        "Return iterator \"one past last element\" in TREE."
        (declare (type <tree> tree))
        nil)
      
      (declaim (inline back^))
      (defun back^ (tree)
        "If TREE is not empty, return iterator to last element.
Otherwise return END^"
        (declare (type <tree> tree))
        (binary-tree-last <kvl> tree))



      ;; lookup

      (declaim (notinline find^))
      (defun find^ (tree key)
        "If TREE contains KEY, return iterator pointing to it.
Otherwise return END^."
        (declare (type <tree> tree)
                 (type <k> key))
        (let ((node (root-of <kvl> tree))
              (best nil))
          (loop :named loop :while node :do
             (let ((xkey (binary-node-first <kv> node)))
               (if (<less> key xkey)
                   (setf node (left-of  <kv> node))
                   (setf best node ;; remember "best" node
                         node (right-of <kv> node)))))
          (when best
            (let ((xkey (binary-node-first <kv> best)))
              (unless (<less> xkey key)
                best)))))



      ;; modifiers                  

      (declaim (inline clear))
      (defun clear (tree)
        "Remove all elements from TREE. Return TREE."
        (setf (root-of  <kvl> tree) nil
              (count-of <kvl> tree) 0)
        tree)

      (defun insert^ (map key &optional value)
        (declare (type <tree> tree))
        ;; TODO
        )

      (defun put^ (map key &optional value)
        (declare (type <tree> tree))
        ;; TODO
        )

      (defun erase^ (map key &optional default)
        (declare (type <tree> tree))
        ;; TODO
        ))


  ;; helper functions
  
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

