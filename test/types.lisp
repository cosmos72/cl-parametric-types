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

(in-package :cl-parametric-types.test)

(def-suite types :in suite)
(in-suite types)

(def-test simplify-type-1 (:compile-at :definition-time)
  (is (equal '(array fixnum (8 7))    (cpt::simplify-type '(array fixnum (8 7)))))
  (is (equal '(array fixnum)          (cpt::simplify-type '(array fixnum *))))
  (is (equal '(array character (4 3)) (cpt::simplify-type '(array character (4 3)))))
  (is (equal '(array character)       (cpt::simplify-type '(array character *))))
  (is (equal '(array base-char (2 1)) (cpt::simplify-type '(array base-char (2 1)))))
  (is (equal '(array base-char)       (cpt::simplify-type '(array base-char *))))
  (is (equal '(array bit (1 0))       (cpt::simplify-type '(array bit (1 0)))))
  (is (equal '(array bit)             (cpt::simplify-type '(array bit *))))
  (is (equal '(array t)               (cpt::simplify-type '(array t *))))
  (is (equal 'array                   (cpt::simplify-type '(array * *)))))

(def-test simplify-type-2 (:compile-at :definition-time)
  (is (equal '(simple-array fixnum (8 7))    (cpt::simplify-type '(simple-array fixnum (8 7)))))
  (is (equal '(simple-array fixnum)          (cpt::simplify-type '(simple-array fixnum *))))
  (is (equal '(simple-array character (4 3)) (cpt::simplify-type '(simple-array character (4 3)))))
  (is (equal '(simple-array character)       (cpt::simplify-type '(simple-array character *))))
  (is (equal '(simple-array base-char (2 1)) (cpt::simplify-type '(simple-array base-char (2 1)))))
  (is (equal '(simple-array base-char)       (cpt::simplify-type '(simple-array base-char *))))
  (is (equal '(simple-array bit (1 0))       (cpt::simplify-type '(simple-array bit (1 0)))))
  (is (equal '(simple-array bit)             (cpt::simplify-type '(simple-array bit *))))
  (is (equal 'simple-t-array                 (cpt::simplify-type '(simple-array t *))))
  (is (equal 'simple-array                   (cpt::simplify-type '(simple-array * *)))))

(def-test simplify-type-3 (:compile-at :definition-time)
  (is (equal '(vector fixnum 7)       (cpt::simplify-type '(array fixnum (7)))))
  (is (equal '(vector fixnum)         (cpt::simplify-type '(array fixnum (*)))))
  (is (equal '(char-string 3)         (cpt::simplify-type '(array character (3)))))
  (is (equal 'char-string             (cpt::simplify-type '(array character (*)))))
  (is (equal '(base-string 2)         (cpt::simplify-type '(array base-char (2)))))
  (is (equal 'base-string             (cpt::simplify-type '(array base-char (*)))))
  (is (equal '(bit-vector 1)          (cpt::simplify-type '(array bit (1)))))
  (is (equal 'bit-vector              (cpt::simplify-type '(array bit (*)))))
  (is (equal '(vector t 0)            (cpt::simplify-type '(array t (0)))))
  (is (equal '(vector t)              (cpt::simplify-type '(array t (*))))))

(def-test simplify-type-4 (:compile-at :definition-time)
  (is (equal '(simple-array-1 fixnum 7)      (cpt::simplify-type '(simple-array fixnum (7)))))
  (is (equal '(simple-array-1 fixnum)        (cpt::simplify-type '(simple-array fixnum (*)))))
  (is (equal '(simple-char-string 3)         (cpt::simplify-type '(simple-array character (3)))))
  (is (equal 'simple-char-string             (cpt::simplify-type '(simple-array character (*)))))
  (is (equal '(simple-base-string 2)         (cpt::simplify-type '(simple-array base-char (2)))))
  (is (equal 'simple-base-string             (cpt::simplify-type '(simple-array base-char (*)))))
  (is (equal '(simple-bit-vector 1)          (cpt::simplify-type '(simple-array bit (1)))))
  (is (equal 'simple-bit-vector              (cpt::simplify-type '(simple-array bit (*)))))
  (is (equal '(simple-vector 0)              (cpt::simplify-type '(simple-array t (0)))))
  (is (equal 'simple-vector                  (cpt::simplify-type '(simple-array t (*))))))
