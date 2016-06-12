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

TYPEXPAND

|#

(in-package :cl-parametric-types.lang)


(defun typexpand (type &optional env)
  (declare (type (or symbol cons) type))
  #+(or ccl cmucl sbcl)
  (introspect-environment:typexpand type env)
  #+abcl
  (system::expand-deftype type)
  #+clisp
  (ext:type-expand type env))

(defun typexpand-1 (type &optional env)
  (declare (type (or symbol cons) type))
  #+(or ccl cmucl sbcl)
  (introspect-environment:typexpand-1 type env)
  #+abcl
  (system::expand-deftype type)
  #+clisp
  (ext:type-expand-1 type))


#-(or abcl ccl clisp cmucl sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "no known implementation of TYPEXPAND on this platform,
  cannot compile CL-PARAMETRIC-TYPES"))
