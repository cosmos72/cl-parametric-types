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

Machinery to instantiate templates: INSTANTIATE function and friends

|#

(in-package #:cl-parametric-types)

(defconstant name!  'name!)
(defconstant quote! 'quote!)

(defun kind-name (kind)
  (string-downcase (symbol-name kind)))

(declaim (type list *instantiating-types* *instantiating-functions*))

(defvar *instantiating-types* nil
  "ALIST of types currently being instantiated. Used to break infinite recursion.")

(defvar *instantiating-functions* nil
  "ALIST of functions currently being instantiated. Used to break infinite recursion.")


(defun instantiating (kind name actual-types)
  "Return concrete function/type name in case it is currently being
instantiated."
  (let ((alist
         (case kind
           (template-type     *instantiating-types*)
           (template-function *instantiating-functions*))))
    (when alist
      (rest (assoc (cons name actual-types) alist :test 'equal)))))


(defun acons-instantiating (name actual-types concrete alist)
  "Return new value of ALIST that also includes the concrete function/type name
currently being instantiated."
  (acons (cons name actual-types) concrete alist))


(defun concretize-cached (kind name actual-types)
  "Cached version of CONCRETIZE. The cache is in alists
*INSTANTIATING-TYPES* and *INSTANTIATING-FUNCTIONS*"
  (or
   (instantiating kind name actual-types)
   (concretize kind name actual-types)))
    

(defmethod instantiate-definition (kind name actual-types)
  (declare (type list actual-types))
  
  (check-valid-type-specifiers actual-types)

  (let ((concrete (concretize-cached kind name actual-types)))
    (multiple-value-bind (definition pattern template-types template-constraints)
	(get-definition kind name actual-types)

      (unless definition
	(error "~A ~S has no definition,
cannot instantiate ~S"
	       (kind-name kind) name (cons name actual-types)))

      (let ((template-params (mapcar #'car template-constraints))
	    (template-values (mapcar #'cdr template-constraints)))
	;; paranoia: in case template-constraints is incomplete...
	(dolist (template-type template-types)
	  (unless (member template-type template-params)
	    (push template-type template-params)
	    (push t             template-values)))
	    
	(log.debug "~&; instantiating ~a ~s~&;   as ~s~a~&"
		   (kind-name kind) (cons name actual-types) concrete
                   (if (equal pattern template-types)
                       ""
                       (format nil "~%;   using ~s specialization for ~s"
                               name pattern)))

	(let ((forms (cddr definition)))
	  (values
	   (multi-subst (cons concrete template-values)
			(cons 'name!   template-params)
			(if (rest forms)
			    `(progn ,@forms)
			    (first forms))
			:quote-symbol 'quote! :eval-symbol 'eval!
			:eval-splice-symbol 'eval-splice!)
	   concrete))))))
      

(defmethod instantiate* (kind name actual-types)
  (declare (type list actual-types))
  (multiple-value-bind (to-eval concrete) (instantiate-definition kind name actual-types)
    ;; restore *package* after EVAL.
    ;; reason: template-struct definitions need to set *package*
    (let ((*package* *package*))
      (eval to-eval))
    concrete))


(defmethod instantiate :around (kind name actual-types)
  "Use *INSTANTIATING-TYPES* and *INSTANTIATING-FUNCTIONS*
as caches for concrete name of types/functions currently being instantiated,
in order to break infinite recursions."
  (declare (type list actual-types))

  (log.trace "~&; attempt to use ~a ~s~&" (kind-name kind) (cons name actual-types))
  (let ((concrete (instantiating kind name actual-types)))
    (when concrete
      (log.trace "~&; ~a ~s is currently being instantiated
;   as ~s
;   breaking infinite recursion~&"
                 (kind-name kind) (cons name actual-types) concrete)
      (return-from instantiate (values concrete nil)))
    (setf concrete (concretize kind name actual-types))
    (case kind
      (template-type
       (let ((*instantiating-types* (acons-instantiating name actual-types concrete
                                                         *instantiating-types*)))
         ;;(break "*instantiating-types* ~S" *instantiating-types*)
         (call-next-method kind name actual-types)))
      (template-function
       (let ((*instantiating-functions* (acons-instantiating name actual-types concrete
                                                             *instantiating-functions*)))
         (call-next-method kind name actual-types)))
      (otherwise
       (call-next-method kind name actual-types)))))
          

(defmethod instantiate (kind name actual-types)
  (let* ((concrete (concretize-cached kind name actual-types))
         (just-instantiated nil))
    (case kind
      ((template-accessor template-constructor)
       ;; struct accessors and constructors cannot be instantiated manually...
       ;; they should be already instantiated by typexpanding the struct name
       ;; in the call to CONCRETIZE-CACHED above.
       (fdefinition concrete)) ;; signals error if not found
      (otherwise
       (handler-case
           (progn
             (ecase kind
               (template-type     (find-class concrete))
               (template-function (fdefinition concrete)))
             (log.trace "~&; ~a ~s is already instantiated as ~s~&"
                        (kind-name kind) (cons name actual-types) concrete))
	 (condition ()
	   (setf concrete (instantiate* kind name actual-types)
		 just-instantiated t)))))
    (values concrete just-instantiated)))
       

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
