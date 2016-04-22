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


(defgeneric mangle (kind name actual-types &key simplify)
  (:documentation
   "Given the symbol name of a parametric function, class or struct
 and the actual types to instantiate it on,
return a string containing a mangled concatenation of the name and actual types.
Return as additional value
\(IF NORMALIZE (NORMALIZE-TYPEXPAND-TYPES ACTUAL-TYPES) ACTUAL-TYPES)

For example, (MANGLE 'TEMPLATE-TYPE 'PAIR '(CONS HASH-TABLE)) returns
\"<PAIR.CONS.HASH-TABLE>\"
while (MANGLE 'TEMPLATE-FUNCTION 'LESS '(FIXNUM)) returns
\"LESS-<FIXNUM>\"

The actual mangling algorithm is subject to change,
the only guarantee is that it must be reversible"))

(defgeneric concretize (kind name actual-types &key simplify)
  (:documentation
   "Given the symbol name of a parametric function, class or struct
and the actual types to instantiate it on,

return the symbol name of the instantiated (or to be instantiated) function,
class or struct. Such symbol must contain a mangled concatenation of the name
and actual types.

Default implementation is
\(MULTIPLE-VALUE-BIND (MANGLED-NAME ACTUAL-TYPES*)
      (MANGLE KIND NAME ACTUAL-TYPES :NORMALIZE NORMALIZE)
    (VALUES
     (INTERN MANGLED-NAME (SYMBOL-PACKAGE NAME))
     ACTUAL-TYPES*))"))

(defgeneric get-definition (kind name)
  (:documentation
   "Return the definition of a parametric function, class or struct
if available, otherwise return nil.

Default implementation is
\(GET NAME KIND)"))

(defgeneric (setf get-definition) (definition kind name)
  (:documentation
   "Set the definition of a parametric function, class or struct.

Default implementation is
\(SETF (GET NAME KIND) DEFINITION)"))

(defgeneric instantiate-definition (kind name actual-types definition
				    &key simplify)
  (:documentation
   "Given the definition of a parametric function, class or struct,
actually instantiate it using the specified actual types"))

(defgeneric instantiate (kind name actual-types &key simplify)
  (:documentation
   "Find the definition of a parametric function, class or struct,
and actually instantiate it using the specified actual types"))

(defgeneric instantiate* (kind name actual-types &key simplify)
  (:documentation
   "If a parametric function, class or struct is not yet instantiated
on the specified actual types, then instantiate it.
Returns the concretized name of the instantiated function, class or struct."))

