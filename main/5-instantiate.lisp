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

(defconstant quote! 'quote!)

(defun kind-name (kind)
  (string-downcase (symbol-name kind)))

(defmethod get-definition ((kind symbol) (name symbol))
  (get name kind))

(defmethod (setf get-definition) (definition (kind symbol) (name symbol) &key specialized-for)
  (declare (type (or list (function (symbol list) (values symbol &optional))) definition)
           (type list specialized-for))
  (if specialized-for
      nil ;; TODO
      (setf (get name kind) definition)))

(defmethod instantiate-definition (kind name actual-types
				   &key definition (simplify t))
  (declare (type list actual-types definition))
  (multiple-value-bind (concrete actual-types*) (concretize kind name actual-types)
    (unless definition
      (setf definition (get-definition kind name))
      (unless definition
	(error "~A ~S has no definition,
cannot instantiate ~S"
	       (kind-name kind) name (cons name actual-types))))
    (destructuring-bind (_ formal-lambda &rest forms) definition
      (declare (ignore _))
      (let ((formal-types (lambda-list->args formal-lambda)))
        (values
         (multi-subst (cons concrete (if simplify actual-types* actual-types))
                      (cons name formal-types)
                      (if (rest forms)
                          `(progn ,@forms)
                          (first forms))
                      :quote-symbol 'quote! :eval-symbol 'eval!
                      :eval-splice-symbol 'eval-splice!)
         concrete)))))
      
(defmethod instantiate* (kind name actual-types &key (simplify t))
  (declare (type list actual-types))
  (let ((definition (get-definition kind name))
	(actual-types (if simplify
			  (simplify-typexpand-list actual-types)
			  actual-types)))
    (etypecase definition
      ((or symbol function) (funcall definition name actual-types))
      (list
       (multiple-value-bind (to-eval concrete)
           (instantiate-definition kind name actual-types
				   :definition definition :simplify nil)
         (let ((orig-package *package*))
           (unwind-protect
                (progn
                  ;; first set package, then eval definition. reason:
                  ;; on some implementations, when in package A, (defstruct B::FOO ...) 
                  ;; may define some functions in A and some in B!
                  (eval `(in-package ,(package-name (symbol-package concrete))))
                  (eval to-eval))
             (eval `(in-package ,(package-name orig-package))))))))))


(defmethod instantiate (kind name actual-types &key (simplify t))
  (declare (type list actual-types))
  (multiple-value-bind (concrete actual-types*) (concretize kind name actual-types)
    (let ((just-instantiated nil))
      (case kind
        ((template-accessor template-constructor)
         ;; struct accessors and constructors cannot be instantiated manually...
         ;; they should be already instantiated by typexpanding the struct name
         ;; in the call to CONCRETIZE above.
         (fdefinition concrete)) ;; signals error if not found
        (otherwise
         (handler-case
             (ecase kind
               (template-type     (find-class concrete))
               (template-function (fdefinition concrete)))
           (condition ()
             (log.debug "~&; instantiating ~A ~S as ~S~&"
                        (kind-name kind) (cons name actual-types) concrete)
             (setf concrete (instantiate* kind name (if simplify actual-types* actual-types)
                                          :simplify nil)
                   just-instantiated t)))))
      (values concrete just-instantiated))))
       

(defun instantiate-type (name-and-actual-types)
  (declare (type cons name-and-actual-types))
  (instantiate 'template-type (first name-and-actual-types)
               (rest name-and-actual-types)))

(defun instantiate-function (name actual-types)
  (declare (type symbol name)
           (type list actual-types))
  (instantiate 'template-function name actual-types))

(defun instantiate-accessor (name struct-name-and-actual-types)
  (declare (type symbol name)
           (type cons struct-name-and-actual-types))
  (instantiate 'template-accessor name struct-name-and-actual-types))

(defun instantiate-constructor (name struct-name-and-actual-types)
  (declare (type symbol name)
           (type cons struct-name-and-actual-types))
  (instantiate 'template-constructor name struct-name-and-actual-types))
