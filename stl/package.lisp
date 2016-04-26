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

(defpackage #:cl-parametric-types.stl
  (:nicknames #:cpt.stl #:c+t.stl #:c+stl)

  (:use #:cl #:cl-parametric-types)

  (:shadowing-import-from #:cl-parametric-types
                          #:defstruct)

  (:export #:less #:less-equal #:greater #:greater-equal
           
           #:pair #:make-pair #:copy-pair #:pair-p
           #:pair-first #:pair-second

           #:triple #:make-triple #:copy-triple #:triple-p
           #:triple-first #:triple-second #:triple-third))
