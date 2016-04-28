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

Simple pattern matching.

Used to match template partial specializations against each other,
and against actual types.

|#

(in-package #:cl-parametric-types)

(defun match-pattern (vars pattern form)
  (declare (type list vars)
           (type (or symbol cons) pattern form))
  (let ((constraints nil))
    (tagbody
       (labels ((%add-match (var match)
                  (let ((pair (assoc var constraints)))
                    (if pair
                        (unless (equal match (rest pair))
                          ;; (break "0")
                          (go fail))
                        (push (cons var match) constraints))))

                (%match (pattern form)
                  (when (atom pattern)
                    (let ((matches (member pattern vars)))
                      (if matches
                          (%add-match (first matches) form)
                          (unless (eql pattern form)
                            ;; (break "2")
                            (go fail)))
                      (return-from %match)))

                  (when (atom form)
                    ;; (break "3")
                    (go fail))
                  (let ((ps pattern)
                        (fs form))
                    (loop :while (and fs ps) :do
                       (%match (pop ps) (pop fs)))
                    (unless (eq (null fs) (null ps))
                      ;; (break "4")
                      (go fail)))))

         (%match pattern form)
         (return-from match-pattern (values constraints t)))
     fail
       (values nil nil))))

                    

                 
        
  
  
