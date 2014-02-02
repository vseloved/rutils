# RUTILS is a general purpose utilities package for COMMON-LISP
  that aims to radically simplify the programmer's life


## Short history

Originally, `RUTILS` stood for `REASONABLE-UTILITIES`, but now I
switched to calling them `RADICAL-UTILITIES`, because they make on
some uncommon and not generally acceptable choices in terms of style
of CL development. Namely:

- Introducing several literal notations for anonymous functions,
  hash-tables, heredoc-strings etc.
- Adding a lot of abbreviations of long symbols from the `CL` package
- Having several versions of the same utilities with different names

But, what's more important, they aim to radically simplify Lisp programming
in the small (with a special accent on simplifying REPL-driven development)

The original **Rationale** for `REASONABLE-UTILITIES` was the following:

1. Virtually everyone talks about utilities in CL (probably, due to
Paul Graham's "On Lisp", that starts with a chapter on the importance
of utilities).  But the existing utility packages are not enough
visible and widespread either due to: not enough utility, poor names,
or lack of community involvement.  The best and, probably,
quite usable package is, in my opinion, `ALEXANDRIA`, which has a great
team of developers, but suffers from a shortcomming of the idea to be
a "good citizen" in the CL world and not include code from other
well-established, but specific utilities packages (`SPLIT-SEQUENCE`,
`ITERATE`, `ANAPHORA`, ...) But the utility package should be as much
all-around and all-encompasing as possible.  Because it should be used in
most of the libraries, and library authors, for obvious reasons, don't
like to add dependencies.  This is one of the causes of not enough
spread of such useful packages as `ITERATE`: one or two usages of
the `ITER` macro often don't justify the dependency on an additional
package.  Yet, if all various utilities are collected under one roof,
it should be a much more reasonable choice to depend on them.

2. Yet the reverse of the coin of all-encompassing utilities' package is
bloat.  It is a common complaint about the CL standard, that it lacks
modularity, and the utilities' package can as well suffer from the
same problem.  But the solution to it is found and implementated using
the CL package mechanism: every part of functionality (like list or
hash-table handling) is segmented into it's own package.
Every package name is formed according to the following template:
`RUTILS.<functionality>` (like: `RUTILS.LIST`, `RUTILS.ITER`).
So it's a single dependency (1 ASDF system) - multiple packages mechanism
that can be used on-demand, depending on the project's needs.
Besides, there are umbrella packages, that include all useful stuff.
A style distinction of different naming conventions is also taken into
consideration, so both long and short names are allowed to co-exist
and be selected according to one's preferences.
So there are such umbrella packages as: `RUTILS`, which
includes all the core functions, and RUTIL, which also includes shorter names.

3. Support for growth of CL. Our aim is to include in this package as
much of the work done in the previous periods and scattered over the
Internet, as possible.  Those utilities are unified in this package
and a lot of effort is put into better documenting them.  Besides we
want to support some (or several) community process for developing the
CL environment and incorporating into it new ideas, that are proved
important and useful. One of such processes is
[CDR](http://cdr.eurolisp.org), and we aim to provide an
implementation of every CDR proposal, that targets "userspace"
(i.e. does not require efforts on the implementation side).
All that will be first put into `RUTILS-CONTRIB` and moved to core after its
presence is justified by user demand.


## Comparison to other utility packages for CL

The other utility packages in CL-land include: `ALEXANDRIA`,
`QUICKUTIL`, `CL-UTILITIES`, `METATILITIES`, and `ARNESI`.



## Current status

What is included and excluded:

### Included

- "Modern" readtable with supoort for literal syntax for hash-tables,
  lambdas and heredoc-strings Basic common contol utilities, like
- Common macros like WITH-GENSYMS, ONCE-ONLY, EVAL-ALWAYS etc.
- Anaphoric utilities
- ITER macro with keywords support
- Symbol manipulation utilities
- List manipulation utilities
- Sequence manipulation utilities (including SPLIT-SEQUENCE)
- Hash-table manipulation utilities
- String manipulation utilities
- Tree manipulation utilities

### Excluded

* Support for concurrency.  The reason is not, that we consider this not useful or general-purpose enough, but rather, that it's a whole new paradigm, and the scope of RUTILS is too small to comfortably accomodate it.
Look at:

 - PORTABLEAGER-FUTURES2
 - STMX

* Functional features.  As in the above, it's as well a whole other paradigm.  It has a limited, but reasonable support in CL.  Other features should be unified in it's own package, and maybe RUTILS can serve as a model for such package or even accomodate it in the future.
Look at:

 - SERIES
 - CLAZY
 - CL-UNIFICATION

* Collections are as well a separate area in CS, so it requires a lot of effort to maintain and develop a comprehensive package in it.
Look at:
 - FSET
 - CL-CONTAINERS

* MOP.  MOP abstraction layer is a CDR and it is as well an essential part of CL.  It is implemented in CLOSER-MOP, and there are plans to integrate it in the future.


## Additional notes

See LICENSE for usage permissions.
See AUTHORS for credits.

The following symbols are added to *FEATURES*:
 - :iter (by RUTILS.ITER, RUTILS, RUTIL)
 - :split-sequence (by RUTILS.SEQUENCE, RUTILS, RUTIL)
