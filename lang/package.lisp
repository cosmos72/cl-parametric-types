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

  (:shadow #:defstruct)

  (:export #:defstruct #:alias #:eval-in-env #:eval! #:error!
           #:find-package* #:find-symbol* #:find-function-or-macro
           #:is-function? #:current-function
	   #:multi-subst #:tree-find
           #:typexpand #:typexpand-1

           #:first-atom #:recurse-first-atom
           #:proper-list? #:proper-tree? #:valid-type-specifier? #:valid-type-specifiers?
           #:lambda-list->params #:lambda-list->params-flags
           #:lambda-list->args #:lambda-list->rest

	   #:ufixnum #:ufixnum+1 #:ufixnum-1 #:ufixnum+ #:ufixnum- #:ufixnum* #:ufixnum/
           #:simple-t-array #:simple-array-1 #:simple-t-array-1
           #:char-string #:simple-char-string))

