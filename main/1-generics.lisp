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

generic functions for internal API.

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

(defgeneric get-all-definitions (kind name)
  (:documentation
   "Return all the template definitions of a parametric function, class or struct
if available, otherwise return nil.

Default implementation is
\(GET NAME KIND)"))

(defgeneric (setf get-all-definitions) (definition kind name)
  (:documentation
   "Set all the definitions of a parametric function, class or struct.

Default implementation is
\(SETF (GET NAME KIND) DEFINITION)"))

(defgeneric get-specialized-definition (kind name specialized-for)
  (:documentation
   "Return the most specific template definition of a parametric function, class or struct
available for specialization SPECIALIZED-FOR, otherwise return nil.
This function implements partial specialization,
i.e. finds the most specific among template partial specializations"))

(defgeneric (setf get-specialized-definition) (definition kind name specialized-for)
  (:documentation
   "Set the definition of a parametric function, class or struct
for specialization SPECIALIZED-FOR."))

(defgeneric get-definition (kind name specialized-for)
  (:documentation
   "Call GET-SPECIALIZED-DEFINITION"))

(defgeneric (setf get-definition) (definition kind name specialized-for)
  (:documentation
   "If SPECIALIZED-FOR is NIL, clear template definitions of NAME by calling
\(SETF (GET-ALL-DEFINITIONS KIND NAME) NIL)
Then in any case always call (SETF GET-SPECIALIZED-DEFINITION)"))

(defgeneric instantiate-definition (kind name actual-types)
  (:documentation
   "Given the definition of a parametric function, class or struct,
actually instantiate it using the specified actual types"))

(defgeneric instantiate* (kind name actual-types)
  (:documentation
   "Find the definition of a parametric function, class or struct,
and actually instantiate it using the specified actual types"))

(defgeneric instantiate (kind name actual-types)
  (:documentation
   "If a parametric function, class or struct already instantiated
on the specified actual types, then return (VALUES <CONCRETIZED-NAME> NIL).
Otherwise instantiate it on the specified actual types,
and return (VALUES <CONCRETIZED-NAME> T)"))

(defun get-definition-template-args (kind name specialized-for)
  (caddr (assoc specialized-for (get-all-definitions kind name) :test 'equal)))
