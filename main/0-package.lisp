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

(defpackage #:cl-parametric-types
  (:nicknames #:cl-ptypes #:cpt #:c+t)

  (:use #:common-lisp #:cl-parametric-types.lang)

  (:shadowing-import-from #:cl-parametric-types.lang
     #:defstruct)
   
  (:import-from #:cl-parametric-types.lang
     #:struct-name-and-options->name 
     #:struct-name-and-options->option 
     #:struct-name-and-options->include
     #:struct-name-and-options->superclass-name)

  (:Export #:defstruct #:alias #:template #:define-template #:typexpand

           ;; types from cl-parametric-types.lang
           #:simple-t-array #:simple-array-1 #:simple-t-array-1
           #:char-string #:simple-char-string

	   #:check-if-safe #:error! #:eval! #:eval-splice! #:quote! #:name!
           #:make #:copy 

           #:instantiate-type     #:instantiate-function
           #:instantiate-accessor #:instantiate-constructor
           
           #:mangle   #:concretize  #:get-definition
           #:instantiate-definition #:instantiate  #:instantiate*

           #:template-type     #:template-struct   #:template-class
	   #:template-function #:template-accessor #:template-constructor))

