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

DEFSTRUCT

|#


(in-package :cl-parametric-types.lang)

(declaim (inline struct-name-and-options->name))
(defun struct-name-and-options->name (name-and-options)
  (first-atom name-and-options))


(defun struct-name-and-options->option (name-and-options keyword)
  (declare (type (or symbol cons) name-and-options)
	   (type keyword keyword))
  (when (consp name-and-options)
    (block nil
      (dolist (option (rest name-and-options))
	(when (eq keyword (first-atom option))
	  (return option))))))


(declaim (inline struct-name-and-options->include))
(defun struct-name-and-options->include (name-and-options)
  (declare (type (or symbol cons) name-and-options))
  (struct-name-and-options->option name-and-options :include))

  
(defun struct-name-and-options->superclass-name (name-and-options)
  (declare (type (or symbol cons) name-and-options))
  (let ((include (struct-name-and-options->include name-and-options)))
    (when (consp include)
      (second include))))


(defmacro defstruct (name-and-options &rest slot-descriptions)
  "Extended version of CL:DEFSTRUCT. It supports all standard options,
plus it also supports derived types as superclasses,
i.e. in (:INCLUDE INCLUDED-STRUCTURE-NAME ...)"
  (let ((include (struct-name-and-options->include name-and-options)))
    (when (consp include)
      (let* ((superclass     (second include))
             (superclass-exp (typexpand superclass)))
        (unless (equal superclass superclass-exp)
          (setf name-and-options (copy-tree name-and-options)
                include          (struct-name-and-options->include name-and-options)
                (second include) superclass-exp))))
    `(cl:defstruct ,name-and-options ,@slot-descriptions)))
