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

This file does XXX.


|#

(in-package #:cl-parametric-types)

(defmacro log.info (&rest args)
  (when args
    `(format t ,@args)))

(defmacro log.debug (&rest args)
  (when args
    `(when *compile-verbose*
       (format t ,@args))))

(defvar *log.trace* nil)

(defmacro log.trace (&rest args)
  (when args
    `(when (and *log.trace* *compile-verbose*)
       (format t ,@args))))
