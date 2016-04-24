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
  :version "0.0.1"
  :author "Massimiliano Ghilardi"
  :license "LLGPL"
  :description "C++-style templates for Common Lisp"
  :depends-on (:introspect-environment :closer-mop)
  :pathname "main/"
  :components
  ((:file "0-package")
   (:file "1-log"          :depends-on ("0-package"))
   (:file "1-generics"     :depends-on ("0-package"))
   (:file "1-options"      :depends-on ("0-package"))
   (:file "1-struct"       :depends-on ("0-package"))
   (:file "2-simplify"     :depends-on ("1-generics"))
   (:file "2-normalize"    :depends-on ("2-simplify"))
   (:file "3-concretize"   :depends-on ("2-simplify"))
   (:file "4-instantiate"  :depends-on ("1-log"    "3-concretize"))
   (:file "5-template"     :depends-on ("1-struct" "1-options" "4-instantiate"))))

