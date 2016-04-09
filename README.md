CL-PARAMETRIC-TYPES
===================

Summary
-------
CL-PARAMETRIC-TYPES adds C++-style template classes, structs and functions to Common Lisp.

This project is currently BETA, i.e. usable but not complete.


Supported systems
-----------------
CL-PARAMETRIC-TYPES is currently tested only on SBCL.

It is quite portable, only relying on [introspect-environment](https://github.com/Bike/introspect-environment)
so porting it to other Common Lisp implementations should be quite straightforward.


Installation and loading
------------------------
Download CL-PARAMETRIC-TYPES into your Quicklisp local-projects folder.
Open a shell and run the commands:

    $ cd ~/quicklisp/local-projects
    $ git clone git://github.com/cosmos72/cl-parametric-types.git

then load a REPL and run:

    CL-USER> (ql:quickload "cl-parametric-types")
    CL-USER> (use-package :cl-parametric-types)
     
If all goes well, it will load CL-PARAMETRIC-TYPES and its dependencies.


Basic usage
-----------
CL-PARAMETRIC-TYPES exports the following macros:

- `TEMPLATE-FUNCTION` declares that a function is parametric, i.e. that the "abstract" source code you provide
  will be later instantiated on "concrete" types. For example,

        (template-function (<t>)
          (defun less (a b)
            (declare (type <t> a b))
            (< a b)))

  is conceptually equivalent to the following C++ code

        template<class T>
        bool less(T a, T b)
        {
            return a < b;
        }

  i.e. both instruct the compiler that a function `LESS` exists, and it in order to actually compile
  and use it, it must be instantiated first, i.e. specialized, on a single type (&lt;t&gt; in CL, T in C++)

  Note: `TEMPLATE-FUNCTION` accepts arbitrary lambda-lists as its arguments, including optional arguments,
  keyword arguments, and &rest as for example:

        (template-function (&optional (<t1> real) (<t2> real))
          (defun less (a b)
            (declare (type <t1> a)
                     (type <t2> b))
            (< a b)))
  

- `TEMPLATE-STRUCT` declares that a structure-object is parametric,
  i.e. that the "abstract" source code you provide  will be later instantiated
  on "concrete" types. For example,

        (template-struct (&optional (<t1> t) (<t2> t))
          (defstruct pair
            (first  nil :type <t1>)
            (second nil :type <t2>)))

  is conceptually equivalent to the following C++ code

        template<class T1, class T2>
        struct pair
        {
            T1 first;
            T2 second;
        }

  Note: also `TEMPLATE-STRUCT` accepts arbitrary lambda-lists as its arguments, including optional arguments,
  keyword arguments, and &rest.
  
- `TEMPLATE-CLASS` declares that a standard-object is parametric,
  i.e. that the "abstract" source code you provide  will be later instantiated
  on "concrete" types. For example,

        (template-class (&optional (<t1> t) (<t2> t))
          (defclass pair ()
            ((first  :type <t1>)
             (second :type <t2>))))

  is conceptually equivalent to the following C++ code

        template<class T, class T2>
        class pair
        {
        private:
            T1 first;
            T2 second;
        }

  Note: also `TEMPLATE-CLASS` accepts arbitrary lambda-lists as its arguments, including optional arguments,
  keyword arguments, and &rest.

Unlike C++ templates, where you must declare if the arguments of `template<...>` are types or values
(and if they are values, you must declare their type), the arguments of `TEMPLATE-FUNCTION`,
`TEMPLATE-STRUCT` and `TEMPLATE-CLASS` can be anything, not only types.

Appendix: why bringing C++-style templates to Common Lisp?
----------------------------------------------------------

Short answer: because we can.

Long answer:

Several comparisons exists between C++ templates and Common Lisp macros, including at least:
* [C++ Templates Common Lisp Macros Comparison](http://c2.com/cgi/wiki?CeePlusPlusTemplatesCommonLispMacrosComparison)
* [C++ and Lisp](http://www.lurklurk.org/cpp_clos.html)
* [What's wrong with C++ templates?](http://www.kuro5hin.org/story/2003/5/26/22429/7674)

Any serious comparison between C++ templates and CL macros will agree on some basic facts:
* Both are Turing complete at *compile-time*, i.e. they can instruct the compiler
  to perform arbitrary computations while compiling - opposed to normal programs code,
  which perform arbitrary computations at *runtime*.
* The syntax of Common Lisp macros is the same as regular Common Lisp,
  while the syntax of C++ templates is different from regular C++ - some could say
  it is "verbose", "ugly" or even worse, but that's not the point:
  the point is that C++ templates and regular C++ are two different languages,
  with different syntax and rules.

So, are C++-style templates really a missing feature in Common Lisp?

A Lisp advocate answer could be
"no, because Common Lisp macros are equivalent to (and more elegant than) C++ templates"

Yet there are other points of view:
* C++ templates are very useful: somebody can implement general algorithms and classes,
  which can be later specialized by somebody else on concrete cases (instantiated),
  producing very efficient machine code, without duplicating the source code.
  
  Common Lisp does not *directly* provide any equivalent mechanism, because macros
  operate on a different level: they can produce arbitrary code, but actually programming
  them is up to the programmer. Macros are general tools to build a programming language,
  rather than a specialized tool to repeatedly compile the same source code with different types.
* If Common Lisp macros are to be considered "better" (for some definition of better)
  than C++ templates, then it should be possible to implement C++ templates using them.

  An even stronger argument could be: show that CL macros can implement C++ templates,
  and *then* the statement "CL macros are better than C++ templates" could have some objective justification.

CL-PARAMETRIC-TYPES exists for all these reasons, and for an additional one:

scratch the author's personal itch about combining two of his favorite tools - Common Lisp and C++ templates.

