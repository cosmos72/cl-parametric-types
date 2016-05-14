CL-PARAMETRIC-TYPES
===================

[![Build Status](https://travis-ci.org/guicho271828/cl-parametric-types.svg?branch=master)](https://travis-ci.org/guicho271828/cl-parametric-types)

Summary
-------
CL-PARAMETRIC-TYPES adds C++-style template classes, structs and functions to Common Lisp.

This project is currently BETA, i.e. usable but not complete.


Supported systems
-----------------
CL-PARAMETRIC-TYPES is currently tested on SBCL, CCL, CLISP and CMUCL.

It is quite portable, as it only needs the function `CLASS-SLOTS`,
for example from [CLOSER-MOP](https://github.com/pcostanza/closer-mop)
and the macro `TYPEXPAND`, for example from [INTROSPECT-ENVIRONMENT](https://github.com/Bike/introspect-environment)
so porting it to other Common Lisp implementations should be relatively straightforward.


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

A somewhat annoying issue is that CL-PARAMETRIC-TYPES must redefine the standard macro
DEFSTRUCT to work around a technical limitation imposed by the standard
\(for the curious, :INCLUDE disallows derived types, so template superclasses would be forbidden).
The effect is that `(use-package :cl-parametric-types)` signals a symbol conflict warning:
you will need to either take the new symbol or resolve the conflict manually.

In real code, one would write instead:

    (defpackage #:my-package
      (:use #:cl #:cl-parametric-types)
      (:shadowing-import-from #:cl-parametric-types #:defstruct)
      (:export #:my-exported-symbols...))


Basic usage
-----------
CL-PARAMETRIC-TYPES exports the following macros:

- `ALIAS` is an utility macro performing textual replacement on forms.
  It is used to define local, short names for long or complicated expressions,
  and it is especially useful to shorten the names of complicated types.
  (Common Lisp has no local version of DEFTYPE).

  It has the same syntax as LET:

        (alias ((alias1 expr1)
                (alias2 expr2)
                 ...)
          form1
          form2
          ...)

  and replaces *all* occurrences of the aliases in the contained forms,
  wherever they appear. For example:

          (alias ((foo (some-really-complex-type arg1 arg2 arg3))
                  (bar (another-complex-type arg4 arg5 arg6)))
            (defun baz (a b)
              (declare (type foo a)
                       (type bar b))
              (frobnicate a b)))

  macroexpands to:

          (defun baz (a b)
            (declare (type (some-really-complex-type arg1 arg2 arg3) a)
                     (type (another-complex-type arg4 arg5 arg6) b))
            (frobnicate a b)))

  With some care, it can be used as poor man's implementation of local types.


- `TEMPLATE` declares that one or more functions, structs or objects are parametric,
  i.e. that the "abstract" source code you provide will be later instantiated
  on "concrete" types. For example,

        (template (<t>)
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

  Note: `TEMPLATE` accepts arbitrary lambda-lists as its arguments, including optional arguments,
  keyword arguments, and &rest as for example:

        (template (&optional (<t1> 'real) (<t2> 'real))
          (defun multiply (a b)
            (declare (type <t1> a)
                     (type <t2> b))
            (* a b)))
  
  `TEMPLATE` can also be used with `DEFSTRUCT` to declare that a structure-object is parametric.
  For example,

        (template (&optional (<t1> t) (<t2> t))
          (defstruct pair
            (first  nil :type <t1>)
            (second nil :type <t2>)))

  is conceptually equivalent to the following C++ code

        template<class T1, class T2>
        struct pair
        {
            T1 first;
            T2 second;
        };

  Note: `DEFSTRUCT` has many options - some say *too* many.
        At the moment, `(TEMPLATE (DEFSTRUCT ...))` only supports
        the option :INCLUDE to set the struct superclass.
        Template superclasses *are* supported, as for example:
        
        (template (&optional (<t1> t) (<t2> t) (<t3> t))
          (defstruct (triple (:include (pair <t1> <t2>)))
            (third  nil :type <t3>)))
        
  is conceptually equivalent to the following C++ code

        template<class T1, class T2, class T3>
        struct triple : public pair<T1, T2>
        {
            T3 third;
        };

  Finally, `TEMPLATE` can also be used with `DEFCLASS` to declare that a standard-object
  is parametric, i.e. that the "abstract" source code you provide  will be later instantiated
  on "concrete" types. For example,

        (template (&optional (<t1> t) (<t2> t))
          (defclass pair2 ()
            ((first  :type <t1>)
             (second :type <t2>))))

  is conceptually equivalent to the following C++ code

        template<class T, class T2>
        class pair2
        {
        private:
            T1 first;
            T2 second;
        };
        
  It is also possible to combine multiple functions, structures and classes definitions
  in a single `TEMPLATE`, as long as all functions, structures and classes share
  the same template arguments:
  
        (template (&optional (<t1> 'real) (<t2> 'real))
          (defun multiply (a b)
            (declare (type <t1> a)
                     (type <t2> b))
            (* a b))
          (defstruct pair
            (first  nil :type <t1>)
            (second nil :type <t2>))
          (defclass pair2 ()
            ((first  :type <t1>)
             (second :type <t2>))))
             
  Unlike C++ templates, where you must declare if the arguments of `template<...>` are types or values
  (and if they are values, you must declare their type), the arguments of `TEMPLATE` can be anything,
  not only types.


Invoking template functions
---------------------------

When a function is declared TEMPLATE, a macro is actually created with its name,
to instantiate the appropriate function and to dispatch at compile time.

For example,

        (template (&optional (<t1> t) (<t2> t))
          (defstruct pair ()
            ((first  :type <t1>)
             (second :type <t2>))))

defines the parametric type `PAIR`, and also defines the symbols `MAKE-PAIR`, `COPY-PAIR`,
`PAIR-P`, `PAIR-FIRST` and `PAIR-SECOND` as macros that, before all the usual parameters,
expect an additional one - the list actual template arguments you want to use:

        ;; i.e. instead of (MAKE-PAIR :FIRST 1 :SECOND 2) you must also specify
        ;; the concrete types to instantiate PAIR and MAKE-PAIR:
        ;;
        (make-pair (bit fixnum) :first 1 :second 2)
        ; instantiating template-type (PAIR BIT FIXNUM) as <PAIR.BIT.FIXNUM>
        #S(<PAIR.BIT.FIXNUM> :FIRST 1 :SECOND 2)
        
        (defvar *pair* *) ;; store last result into *pair*
        *PAIR*

        (pair-first (bit fixnum) *pair*)
        1

Also, `(SETF PAIR-FIRST)` and `(SETF PAIR-SECOND)` work as expected:

        (setf (pair-first (bit fixnum) *pair*) 0)
        0

        *pair*
        #S(<PAIR.BIT.FIXNUM> :FIRST 0 :SECOND 2)
        

Partial template specialization
-------------------------------

`TEMPLATE` also supports partial template specialization. For example, to specialize the function
`LESS` defined above, adding a specialization that compares instances of `PAIR`, one can write:
  
        (template (<t1> <t2>)
          (:specialized-for ((pair <t1> <t2>))
          (defun less (a b)
            (declare (type (pair <t1> <t2>) a b))
            (let ((a1 (pair-first (<t1> <t2>)) a)
                  (b1 (pair-first (<t1> <t2>)) b))
              (cond
                (((quote! less) (<t1>) a1 b1) t)
                (((quote! less) (<t1>) b1 a1) nil)
                (t
                 ((quote! less) (<t2>) (pair-second (<t1> <t2>) a)
                                       (pair-second (<t1> <t2>) b)))))))

The ugly `(QUOTE! LESS)` stuff is a (hopefully temporary) workaround,
it prevents `LESS` to be interpreted as the function currently being defined
by the template, i.e. as `LESS ((PAIR <T1> <T2>))`

The equivalent C++ code would be:

        template<class T1, class T2>
        bool less<pair<T1, T2> >(pair<T1,T2> a, pair<T1,T2> b) 
        {
            T1 a1 = a.first, b1 = b.second;
	    if (less<T1>(a1, b1))
	        return true;
	    if (less<T1>(b1, a1))
	        return false;
	    return less<T2>(a.second, b.second);
        };

Note that, just like in C++, partially specialized functions and types
are used when their specialization *pattern* matches the actual types,
ignoring any superclass<->subclass relationship.

In other words, the function `LESS` specialized for `(PAIR <T1> <T2>)`
will *not* be instantiated nor called when trying to invoke
`(LESS (TRIPLE <T1> <T2> <T3>) ...)` even if `TRIPLE` is a subclass of `PAIR`.


Appendix: design philosophy
---------------------------

Short version: maximum performance, zero runtime overhead, compile-time instantiation,
  compile-time overload resolution (i.e. dispatching).

Long version: to be written...


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

Legal
-----

CL-PARAMETRIC-TYPES is released under the terms of the [Lisp Lesser General Public License]
(http://opensource.franz.com/preamble.html), known as the LLGPL.
