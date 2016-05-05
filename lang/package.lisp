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

(in-package :cl-user)

(defpackage #:cl-parametric-types.lang
  (:nicknames #:cpt.lang #:c+lang)

  (:use #:common-lisp)

  (:shadow #:defstruct #:eval)

  (:export #:defstruct #:eval #:eval! #:multi-subst #:tree-find
           #:typexpand #:typexpand-1

           #:first-atom #:recurse-first-atom #:lambda-list->args

           #:sarray #:sarray-t #:svector #:svector-t #:sstring #:sbstring))
