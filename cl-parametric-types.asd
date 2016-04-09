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


(asdf:defsystem #:cl-parametric-types
  :name "CL-PARAMETRIC-TYPES"
  :version "0.0.0"
  :author "Massimiliano Ghilardi"
  :license "LLGPL"
  :description "C++-style templates for Common Lisp"
  
  :depends-on (:introspect-environment)
  
  :components
  ((:static-file "cl-parametric-types.asd")
   (:file "package")
   (:file "log"          :depends-on ("package"))
   (:file "generics"     :depends-on ("package"))
   (:file "concretize"   :depends-on ("generics"))
   (:file "instantiate"  :depends-on ("concretize" "log"))
   (:file "struct"       :depends-on ("package"))
   (:file "template"     :depends-on ("instantiate" "struct"))
   (:file "pair"         :depends-on ("template"))))

