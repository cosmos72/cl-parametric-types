Template-function HASH
======================

Syntax:
-------
`(hash (<t>) object) => hash-code`

Arguments and values:
--------------------
object:    an object
hash-code: a non-negative fixnum

Description:
------------
HASH returns the [hash code](http://en.wikipedia.com/wiki/Hash_function) of an object.
Commonly used together with [EQUAL-TO](equal-to.md)
to implement [hash associative containers](hash-set-or-map.md)

Unlike CL:SXHASH, values returned by HASH on different Lisp images
for the same objects *can* be different - HASH is only guaranteed
to produce consistent values within a *single* Lisp image;
this allows salted hashes that prevent collision DoS attacks.

The algorithm underlying HASH is implementation-dependent,
and provides the following guarantees:

1. consistency between HASH and EQUAL-TO: equal objects have equal hash code.

   Formally: given a type `<T>` and two objects A, B of type <T> or some subtype,
   `(EQUAL-TO <T> A B)` implies `(= (HASH <T> A) (HASH <T> B))`

2. invariability: The hash-code for an object is always the same within a single
   Lisp image, provided that the object is not visibly modified with regard to
   the equivalence test EQUAL-TO. This is a logical consequence of item 1 above.

3. discrimination: with very high probability, different objects
   have different hash codes.
  
   Formally: given a type `<T>` and two objects A, B of type <T> or some subtype,
   `(NOT (EQUAL-TO <T> A B))` implies that, with a probability approaching
   1/(1+ MOST-POSITIVE-FIXNUM), `(/= (HASH <T> A) (HASH <T> B))`
   
4. uniformity: the implementation should make a good-faith effort to produce
   hash codes that are well distributed between 0 and MOST-POSITIVE-FIXNUM

5. not intended for cryptography: the algorithms used by HASH do *not* attempt
   to be resistant to cryptographic attacks.

   Formally:
   Constructing HASH collisions, i.e. different objects that have the same hash code,
   is probably an easy task.
   Similarly, given a hash code, constructing an object that has such hash code,
   is probably an easy task too.

Notes:
------
In order to maintain the consistency between HASH and EQUAL-TO, if you define
a partial template specialization for the template-function [EQUAL-TO](equal-to.md),
you *should* also define the corresponding specializazion for HASH.

Side effects:
-------------
none

Exceptional situations:
-----------------------
none

Specializations:
----------------
CL-PARAMETRIC-TYPES.STL predefines the following specializations:

        (HASH (<T>))
        (HASH ((PAIR <T1> <T2>)))
        (HASH ((SIMPLE-ARRAY <T>)))
        (HASH (SIMPLE-BIT-VECTOR))
        (HASH (SIMPLE-BASE-STRING))
        (HASH (SIMPLE-CHAR-STRING)) ;; i.e. (HASH ((SIMPLE-ARRAY CHARACTER (*))))
        (HASH ((TRIPLE <T1> <T2> <T3>)))

Examples:
---------
To be written...