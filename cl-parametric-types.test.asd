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


(asdf:defsystem :cl-parametric-types.test
  :version "0.0.1"
  :author "Massimiliano Ghilardi"
  :license "LLGPL"
  :description "Test system for cl-parametric-types"
  :depends-on (:fiveam :cl-parametric-types :cl-parametric-types.stl)
  :pathname "test/"
  :components
  ((:file "package")
   (:file "types"     :depends-on ("package"))
   (:file "compare"   :depends-on ("package"))
   (:file "pair"      :depends-on ("package"))
   (:file "triple"    :depends-on ("pair"))
   (:file "deque"     :depends-on ("package"))))
   


(defmethod asdf:perform :after ((operation asdf:load-op)
                                (component (eql (asdf:find-system :cl-parametric-types.test))))
  (asdf:test-system :cl-parametric-types.test))

(defmethod asdf:perform ((operation asdf:test-op)
                         (component (eql (asdf:find-system :cl-parametric-types.test))))
  (uiop:symbol-call :5am :run! (uiop:find-symbol* :suite :cl-parametric-types.test)))
