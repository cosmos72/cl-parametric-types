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


(defun multi-subst (values args tree)
  (declare (type list values args tree))
  (setf tree (nsubst (pop values) (pop args) (copy-tree tree)))
  (loop :for arg :in args
     :for value = (if values (pop values) t)
     :do (nsubst value arg tree))
  tree)

(defun kind-name (kind)
  (string-downcase (symbol-name kind)))

(defmethod get-definition ((kind symbol) (name symbol))
  (get name kind))

(defmethod (setf get-definition) (definition (kind symbol) (name symbol))
  (declare (type (or list (function (symbol list) (values symbol &optional)))
		 definition))
  (setf (get name kind) definition))

(defmethod instantiate-definition (kind name actual-types definition &key (simplify t))
  (declare (type list actual-types definition))
  (let* ((formal-types     (second definition))
	 (definition-form  (third  definition)))
    (multiple-value-bind (concrete actual-types*) (concretize kind name actual-types)
      (unless definition
	(error "~A ~S has no definition,
cannot instantiate ~S"
	       (kind-name kind) name (cons name actual-types)))
      (values
       (multi-subst (cons concrete (if simplify actual-types* actual-types))
                    (cons name formal-types)
                    definition-form)
       concrete))))
      

(defmethod instantiate (kind name actual-types &key (simplify t))
  (declare (type list actual-types))
  (let ((definition (get-definition kind name))
	(actual-types (if simplify (simplify-typexpand-list actual-types) actual-types)))
    (etypecase definition
      ((or symbol function) (funcall definition name actual-types))
      (list
       (multiple-value-bind (to-eval concrete)
           (instantiate-definition kind name actual-types definition :simplify nil)
         (let ((orig-package *package*))
           (unwind-protect
                (progn
                  ;; first set package, then eval definition. reason:
                  ;; on some implementations, when in package A, (defstruct B::FOO ...) 
                  ;; may define some functions in A and some in B!
                  (eval `(in-package ,(package-name (symbol-package concrete))))
                  (eval to-eval))
             (eval `(in-package ,(package-name orig-package))))))))))


(defmethod instantiate* (kind name actual-types &key (simplify t))
  (declare (type list actual-types))
  (multiple-value-bind (concrete actual-types*) (concretize kind name actual-types)
    (case kind
      ((template-accessor template-constructor)
       ;; struct accessors and constructors cannot be instantiated manually...
       ;; they should be already instantiated by typexpanding the struct name.
       (fdefinition concrete)) ;; signals error if not found
      (otherwise
       (handler-case
           (ecase kind
             (template-type     (find-class concrete))
             (template-function (fdefinition concrete)))
         (condition ()
           (log.debug "~&; instantiating ~A ~S as ~S~&"
                      (kind-name kind) (cons name actual-types) concrete)
           (setf concrete (instantiate kind name (if simplify actual-types* actual-types)
                                       :simplify nil))))))
    concrete))


