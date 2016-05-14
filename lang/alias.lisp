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

ALIAS

|#


(in-package :cl-parametric-types.lang)


(defmacro alias (aliases &body body)
  "Perform textual replacement on BODY. Has the same syntax as LET, i.e.

\(ALIAS ((ALIAS1 EXPR1)
        (ALIAS2 EXPR2)
         ...)
  FORM1
  FORM2
  ...)

and replaces *all* occurrences of the aliases in BODY,
wherever they appear. For example:

\(ALIAS ((FOO (SOME-REALLY-COMPLEX-TYPE ARG1 ARG2 ARG3))
        (BAR (ANOTHER-COMPLEX-TYPE ARG4 ARG5 ARG6)))
  (DEFUN BAZ (A B)
    (DECLARE (TYPE FOO A)
             (TYPE BAR B))
    (FROBNICATE A B)))

macroexpands to:

\(DEFUN BAZ (A B)
  (DECLARE (TYPE (SOME-REALLY-COMPLEX-TYPE ARG1 ARG2 ARG3) A)
           (TYPE (ANOTHER-COMPLEX-TYPE ARG4 ARG5 ARG6) B))
  (FROBNICATE A B)))

With some care, it can be used as poor man's implementation of local types,
since Common Lisp has DEFTYPE to define types in the global environment,
but nothing to define local types..."
  (declare (type list aliases))
  (multiple-value-bind (new-list old-list)
      (loop :for (key val) :in aliases
	 :collect key :into old-list
	 :collect val :into new-list
	 :finally (return (values new-list old-list)))
    (let ((body (multi-subst new-list old-list body)))
      (if (rest body)
	  `(progn ,@body)
	  (first body)))))
