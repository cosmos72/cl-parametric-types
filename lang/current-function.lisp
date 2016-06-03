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

CURRENT-FUNCTION

|#


(in-package :cl-parametric-types.lang)

(defmacro current-function (&environment env)
  (let* ((callers (log4cl:enclosing-scope-block-name *package* env))
         (caller (cond
                   ((atom callers) callers)
                   ((rest callers) callers)
                   (t       (first callers)))))
    `',caller))
