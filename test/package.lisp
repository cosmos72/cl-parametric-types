

(defpackage :cl-parametric-types.test
  (:use :cl :cl-parametric-types :fiveam))

(in-package :cl-parametric-types.test)

(def-suite :cl-parametric-types)
(in-suite :cl-parametric-types)

(test cl-parametric-types
      )

(test less
  (template (<t>)
    (defun less (a b)
      (declare (type <t> a b))
      (< a b)))
  (is-true
   (progn
     (less (float) 0.0 1.0)))
  (is-true
   (progn
     (less (fixnum) 0 1)))
  (is-false
   (progn
     (less (float) 1.0 0.0)))
  (is-false
   (progn
     (less (fixnum) 1 0))))

(test pair
  (template (&optional (<t1> t) (<t2> t))
    (defstruct pair
      (first  nil :type <t1>)
      (second nil :type <t2>))
    (defun pair-less (a b)
      (and (< (pair-first (<t1>) a) (pair-first (<t2>) b))
           (< (pair-second (<t1>) a) (pair-second (<t2>) b)))))
  (is-true
   (progn
     (pair-less
      (fixnum fixnum)
      (make-pair (fixnum fixnum) :first 1 :second 2)
      (make-pair (fixnum fixnum) :first 3 :second 4)))))


