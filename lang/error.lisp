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

ERROR!

|#


(in-package :cl-parametric-types.lang)

(defmacro error! (datum &rest arguments &environment env)
  "invoke ERROR at macroexpansion time"
  (push datum arguments)
  (let ((args (loop :for arg :in arguments
		 :collect (eval arg env))))
    (apply 'error args)))
