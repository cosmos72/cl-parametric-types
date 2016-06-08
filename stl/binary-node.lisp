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

;;; binary node

(template (<k> <v>)
  (defstruct (binary-node (:include (pair <k> <v>)))
    (left   nil :type (or null (binary-node <k> <v>)))
    (right  nil :type (or null (binary-node <k> <v>)))
    (parent nil :type (or null (binary-node <k> <v>)))))


;;; Helper functions used by binary-tree subclasses

(alias ((<kv> (<k> <v>)))
  (template (<k> <v>)
    (declaim (inline binary-node-is-left-child?))
    (defun binary-node-is-left-child? (node parent)
      (declare (type (binary-node <k> <v>) node parent))
      (eq node (binary-node-left (<k> <v>) parent)))


    (defun binary-node-rotate-left (node)
      "Rotate left the subtree around node. Return new subtree root."
      (declare (type (binary-node <k> <v>) node))
      (log.debug "before:~%~A" (print-object-contents nil node))
      (let ((x (binary-node-right <kv> node)))
        (setf (binary-node-right <kv> node) (binary-node-left <kv> x))
        (setf (binary-node-left  <kv> x)    node)
        (log.debug "after:~%~A" (print-object-contents nil x))
        x))


    (defun rotate-binary-node-right (node)
      "Rotate right the subtree around node. Return new subtree root."
      (declare (type (binary-node <k> <v>) node))
      (log.debug "before:~%~A" (print-object-contents nil node))
      (let ((x (binary-node-left <kv> node)))
        (setf (binary-node-left  <kv> node) (binary-node-right <kv> x))
        (setf (binary-node-right <kv> x)    node)
        (log.debug "after:~%~A" (print-object-contents nil x))
        x))


    (defun rotate-binary-node-around (node parent &key left)
      "Rotate left or right the subtree around node. Return new subtree root
and also update parent's link to subtree root."
      (declare (type (binary-node <k> <v>) node)
               (type (or null (binary-node <k> <v>)) parent))
      (let ((new-node
             (if left
                 (binary-node-rotate-left  <kv> node)
                 (binary-node-rotate-right <kv> node))))
        ;; connect parent to rotated node
        (when parent
          (if (binary-node-is-left-child? node parent)
              (setf (binary-node-left  <kv> parent) new-node)
              (setf (binary-node-right <kv> parent) new-node)))
        new-node))


    (defun binary-node-replace (old-node new-node parent)
      "Unlink old-node from it parent and replace it with new-node.
Return t if left child was replaced, nil if right child was replaced"
      (declare (type (or null (binary-node <k> <v>)) old-node new-node parent))
      (when parent
        (let ((left-child? (binary-node-is-left-child? old-node parent)))
          (if left-child?
              (setf (binary-node-left  <kv> parent) new-node)
              (setf (binary-node-right <kv> parent) new-node))
          left-child?)))))

