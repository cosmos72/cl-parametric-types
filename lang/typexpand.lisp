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
  (declare (type (or symbol cons) type)
           #+(or ecl lispworks) (ignore env))
  #+(or ccl cmucl sbcl)
  (introspect-environment:typexpand type env)  
  #+abcl
  (system::expand-deftype type)
  #+allegro
  (excl::deftype-expand type env)
  #+clisp
  (ext:type-expand type env)
  #+ecl
  (si::expand-deftype type)
  #+lispworks
  (type::expand-user-type type))

#+ecl
(defun expand-deftype-1 (type)
  "This function largely copies from ECL's expand-deftype"
  (let (base args)
    (if (atom type)
        (setf base type
              args nil)
        (setf base (car type)
              args (cdr type)))
    (let ((expander (system::get-sysprop base 'system::DEFTYPE-DEFINITION)))
      (funcall expander type))))

(defun typexpand-1 (type &optional env)
  (declare (type (or symbol cons) type)
           #+(or ecl lispworks) (ignore env))
  #+(or ccl cmucl sbcl)
  (introspect-environment:typexpand-1 type env)
  #+abcl
  (system::expand-deftype type)
  #+allegro
  (excl::deftype-expand-1 type env)
  #+clisp
  (ext:type-expand-1 type)
  #+ecl
  (expand-deftype-1 type)
  #+lispworks
  (type::expand-user-type-1 type))


#-(or abcl allegro ccl clisp cmucl ecl lispworks sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "no known implementation of TYPEXPAND on this platform,
  cannot compile CL-PARAMETRIC-TYPES"))
