# RUTILS is a syntactic utilities package for Common Lisp

(that aims to radically simplify the Lisp programmer's life
both in the REPL and in large system development)

RUTILS stands for RADICAL-UTILITIES :)

See the [announcement](docs/ann-rutils.md).

Read the [tutorial](docs/tutorial.md) for usage details or the [quickdocs](http://quickdocs.org/rutils).


## Short history

Originally, `RUTILS` stood for `REASONABLE-UTILITIES`, but now I
switched to calling them `RADICAL-UTILITIES`, because they make
some uncommon and not generally acceptable choices in terms of style
of CL development. Namely:

- Introducing several literal notations for anonymous functions,
  hash-tables, heredoc-strings etc.
- Adding a lot of abbreviations of long symbols from the `CL` package
- Having several versions of the same utilities with different names

But, what's more important, they aim to radically simplify Lisp programming
in the small (with a special accent on simplifying REPL-driven development).

See the [original rationale](docs/reasonable-utilities.md) for `REASONABLE-UTILITIES`.

The current version is 5.0.0.


## Major Features

- "Modern" readtable with support for literal syntax for hash-tables,
  lambdas and heredoc-strings
- Common macros like `WITH-GENSYMS`, `ONCE-ONLY`, `EVAL-ALWAYS` etc. and other symbol manipulation utilities
- Basic anaphoric macros
- A simple pair data structure and accompanying utilities
- List manipulation utilities
- Hash-table manipulation utilities
- Hash-set manipulation utilities
- Sequence manipulation utilities (including `SPLIT-SEQUENCE`, or rather, simply `SPLIT`)
- Array manipulation utilities
- String manipulation utilities
- Tree manipulation utilities
- generic access to elements of any data structure: `GENERIC-ELT`/`?`
- `ITER` macro with keywords support
- generic `BIND`
- Clojure-style threading macros `->` and `->>`


## Explicitly not supported

The following broad topics are not supported by design:

* Concurrency.

  The reason is not, that I consider this not useful or general-purpose enough,
  but rather, that it's a whole new paradigm, and the scope of `RUTILS`
  is too small to comfortably accommodate it.

* Functional paradigm.

  As in the above, it's as well a whole other paradigm.
  It has a limited, but reasonable support in CL.
  Other features should be unified in it's own package,
  and maybe `RUTILS` can serve as a model for such package
  or even accommodate it in the future.

* Advanced collections are as well a separate area in CS,
  so it requires a lot of effort to maintain and develop a comprehensive package in it.

  Look at `FSET` and `CL-CONTAINERS` for such things.

* MOP.  MOP abstraction layer is a CDR and it is as well an essential part of CL.
  It is implemented and well supported by `CLOSER-MOP`.


## Technical notes

### Dependencies

- [NAMED-READTABLES](http://common-lisp.net/project/named-readtables/)
- [CLOSER-MOP](https://github.com/pcostanza/closer-mop)

### Exported features

The following symbols are added to `*FEATURES*`:

- `:split-sequence` (by `RUTILS.SEQUENCE`, `RUTILS`, and `RUTIL`)
- `:iter` (by `RUTILSX.ITER` and `RUTILSX`)


## Organizational notes

(c) 2009-2019, Vsevolod Dyomkin <vseloved@gmail.com>

See LICENSE for usage permissions.
See AUTHORS for credits.
