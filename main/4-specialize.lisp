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

Methods GET-ALL-DEFINITIONS and (SETF GET-ALL-DEFINITIONS),
and simple pattern matching function MATCH-PATTERN.

Used to find the most specialized template definition
matching some given actual types.

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
	 ;; (break "match-pattern SUCCESSFUL")
         (return-from match-pattern (values t constraints)))
     fail
       ;; (break "match-pattern FAILED")
       (values nil nil))))

                    


(defmethod get-specialized-definition (kind name specialized-for)
  (declare (type list specialized-for))

  (log.trace "~&; searching best specialization for ~A ~S"
	     (kind-name kind) (cons name specialized-for))
	
  (let ((all-defs (get-all-definitions kind name))
	(best-defs nil))
    (flet ((%compare-specialized-definition (vars pattern)
	     "Return 1 if vars+pattern is MORE specialized than all current candidates.
Return -1 if vars+pattern is LESS specialized than all current candidates.
Otherwise return 0"
	     (let ((less t)
		   (more t))
	       (dolist (best-def best-defs)
		 (destructuring-bind (def def-pattern def-vars constraints) best-def
		   (declare (ignore def constraints))
		   (let* ((less-eq-i (match-pattern vars pattern def-pattern))
			  (more-eq-i (match-pattern def-vars def-pattern pattern)))
		     (when less-eq-i
		       (setf more nil))
		     (when more-eq-i
		       (setf less nil)))))
	       (cond
		 ((and more (not less)) 1)
		 ((and less (not more)) -1)
		 (t 0)))))
      ;; loop on all specializations and find the most specific one
      (dolist (pair all-defs)
	(let* ((def         (cdr pair))
	       (def-vars    (lambda-list->args (second def)))
	       ;; non-specialized template definition has NULL def-pattern:
	       ;; it matches everything, as long as the number of template arguments is the same
	       (def-pattern (or (car pair) def-vars)))
	  (multiple-value-bind (match constraints)
	      (match-pattern def-vars def-pattern specialized-for)
	    (when match
	      (let ((cmp (%compare-specialized-definition def-vars def-pattern)))
		(when (> cmp 0)
		  (setf best-defs nil))
		(unless (< cmp 0)
		  (push (list def def-pattern def-vars constraints) best-defs))))))))
    (when (rest best-defs)
      (error "CL-PARAMETRIC-TYPES: cannot find most specific specialization
for ~A ~S:
all the following patterns are \"equally\" specific:~S"
	     (kind-name kind) name
	     (with-output-to-string (s)
	       (dolist (def best-defs)
		 (format s "~%~S" (second def))))))
    (if best-defs
	(values-list (first best-defs))
	nil)))
		   
    


(defmethod (setf get-specialized-definition) (definition kind name specialized-for)
  (declare (type list definition specialized-for))
  (let* ((all-defs (get-all-definitions kind name))
	 (prev-def (assoc specialized-for all-defs :test 'equal)))
    ;; adding a specialization? then ensure the length of specialized-for is always the same
    (when (and specialized-for all-defs)
      (let ((n1 (length specialized-for)))
	(dolist (def all-defs)
	  ;; non-specialized template definition has NULL def-pattern:
	  ;; we must compute it from def-lambda-list
	  (let* ((def-specialized-for (or (car def) (lambda-list->args (caddr def))))
		 (n2 (length def-specialized-for)))
	    (unless (= n1 n2)
	      (error "CL-PARAMETRIC-TYPES: the number of template arguments does not match!
Trying to add ~A ~S partial specialization for
~S i.e. with ~S template arguments,
but there is a pre-existing ~A ~S specialized for
~S i.e. with ~S template arguments."
		     (kind-name kind) name specialized-for n1
		     (kind-name kind) name def-specialized-for n2))))))
    (if prev-def
	(setf (cdr prev-def) definition)
	(progn
	  (push (cons specialized-for definition) all-defs)
	  (setf (get-all-definitions kind name) all-defs)
	  definition))))


(defmethod get-all-definitions ((kind symbol) (name symbol))
  (get name kind))

(defmethod (setf get-all-definitions) (definition (kind symbol) (name symbol))
  (setf (get name kind) definition))


(defmethod get-definition (kind name specialized-for)
  (if specialized-for
      (get-specialized-definition kind name specialized-for)
      (cdr (assoc nil (get-all-definitions kind name)))))

(defmethod (setf get-definition) (definition kind name specialized-for)
  (unless specialized-for
    (setf (get-all-definitions kind name) nil))
  (setf (get-specialized-definition kind name specialized-for) definition))
  
