Package CL-PARAMETRIC-TYPES.STL
===============================

CL-PARAMETRIC-TYPES.STL aims at reimplementing most of C++ STL functions and types,
translating them to idiomatic Common Lisp.

It currently implements:
* template-type `(BIVECTOR <T>)` - see [bivector.md](bivector.md)

* template-type `(PAIR <T1> <T2>)` - see [pair.md](pair.md)

* template-type `(TRIPLE <T1> <T2> <T3>)` - see [triple.md](triple.md)

* template-functions `(EQUAL-TO (<T>))` and `(NOT-EQUAL-TO (<T>))`
  - see [equal-to.md](equal-to.md)
  
* template-function `(HASH (<T>))` - see [hash.md](hash.md)

* template-functions `(LESS (<T>))` `(LESS-EQUAL (<T>))` `(GREATER (<T>))`
  and `(GREATER-EQUAL (<T>))` - see [less.md](less.md)

