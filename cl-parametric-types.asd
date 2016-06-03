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
  :depends-on (:closer-mop :introspect-environment :log4cl)
  :components
  ((:static-file "cl-parametric-types.asd")

   (:module :lang
    :components
    ((:file "package")
     (:file "list"         :depends-on ("package"))
     (:file "tree"         :depends-on ("package"))
     (:file "alias"        :depends-on ("tree"))
     (:file "current-function" :depends-on ("package"))
     (:file "eval"         :depends-on ("package"))
     (:file "error"        :depends-on ("eval"))
     (:file "types"        :depends-on ("package"))
     (:file "typexpand"    :depends-on ("package"))
     (:file "defstruct"    :depends-on ("typexpand"))))

   (:module :main :depends-on (:lang)
    :components
    ((:file "0-package")
     (:file "1-check"        :depends-on ("0-package"))
     (:file "1-generics"     :depends-on ("0-package"))
     (:file "1-log"          :depends-on ("0-package"))
     (:file "1-options"      :depends-on ("0-package"))
     (:file "1-struct"       :depends-on ("1-generics"))
     (:file "2-simplify"     :depends-on ("1-generics"))
     (:file "2-normalize"    :depends-on ("2-simplify"))
     (:file "3-concretize"   :depends-on ("2-simplify"))
     (:file "4-specialize"   :depends-on ("1-check" "2-normalize"))
     (:file "5-instantiate"  :depends-on ("1-log"    "3-concretize"))
     (:file "6-template"     :depends-on ("1-struct" "1-options" "5-instantiate"))))))

