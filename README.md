CL-PARAMETRIC-TYPES
===================

Summary
-------
CL-PARAMETRIC-TYPES adds C++-style template classes, structs and functions to Common Lisp.

This project is currently ALPHA, not yet ready for public usage.


Appendix: why bringing C++-style templates to Common Lisp?
-------

Short answer: because we can.

Long answer:

Several comparisons exists between C++ templates and Common Lisp macros, including at least:
* [C++ Templates Common Lisp Macros Comparison](http://c2.com/cgi/wiki?CeePlusPlusTemplatesCommonLispMacrosComparison)
* [C++ and Lisp](http://www.lurklurk.org/cpp_clos.html)
* [What's wrong with C++ templates?](http://www.kuro5hin.org/story/2003/5/26/22429/7674)

Any serious comparison between C++ templates and CL macros will agree on some basic facts:
* Both are Turing complete at compile-time, i.e. they can instruct the compiler
  to perform arbitrary computations while compiling - opposed to normal programs code,
  which perform arbitrary computations at *runtime*
* The syntax of Common Lisp macros is the same as regular Common Lisp,
  while the syntax of C++ templates is different from regular C++ - some could say
  it is "verbose", "ugly" or even worse, but that's not the point:
  the point is that C++ templates and regular C++ are two different languages,
  with different syntax and rules.

So, are C++-style templates really a missing feature in Common Lisp?
An intuitive answer could be
"no, because Common Lisp macros are equivalent to (and more elegant than) C++ templates"

Yet there are other points of view:
* C++ templates are very useful: somebody can implement general algorithms and classes,
  which can be later specialized by somebody else on concrete cases (instantiated),
  producing very efficient machine code, without duplicating the source code.
  
  Common Lisp does not *natively* provide any equivalent mechanism, because macros
  operate on a different level: they can produce arbitrary code, but actually programming
  them is up to the programmer. They are general tools to build a programming language,
  rather than a specialized tool to repeatedly compile the same source code with different types.
* If Common Lisp macros are considered "better" (for some definition of better) than C++ templates
  by someone, then it should be possible to implement C++ templates using them.

  An even stronger argument could be: show that Common Lisp macros can implement C++ templates,
  and *then* the statement "CL macros are better than C++ templates" would have some objective justification.

CL-PARAMETRIC-TYPES exists for all these reasons, and for an additional one:
scratch the author's personal itch about combining two of his favorite tools - Common Lisp and C++ templates.

