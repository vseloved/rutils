# RUTILS is a syntactic utilities package for Common Lisp

(that aims to radically simplify the Lisp programmer's life
both in the REPL and in large system development)

RUTILS stands for RADICAL-UTILITIES :)

See the [announcement](docs/ann-ruitls.md).


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

See the [original rationale](docs/reasonable-utilities.md) for [REASONABLE-UTILITIES].


## Brief description

### Included in core

- "Modern" readtable with supoort for literal syntax for hash-tables,
  lambdas and heredoc-strings

Basic common contol utilities, like

- Common macros like `WITH-GENSYMS`, `ONCE-ONLY`, `EVAL-ALWAYS` etc.
- Basic anaphoric macros
- Symbol manipulation utilities
- List manipulation utilities
- Sequence manipulation utilities (including `SPLIT-SEQUENCE`)
- Hash-table manipulation utilities
- String manipulation utilities
- Tree manipulation utilities
- A simple pair data structure and accompanying utilities

### Included in contrib (RUTILSX)

- generic access to elements of any data structure: `generic-elt`/`~`,
  generic sequence and key-value iteration protocols
- `ITER` macro with keywords support
- generic `BIND`

### Explicitly not supported

The following broad topics are not supported by design:

* Concurrency.

  The reason is not, that we consider this not useful or general-purpose enough,
  but rather, that it's a whole new paradigm, and the scope of `RUTILS`
  is too small to comfortably accomodate it.

* Functional paradigm.

  As in the above, it's as well a whole other paradigm.
  It has a limited, but reasonable support in CL.
  Other features should be unified in it's own package,
  and maybe RUTILS can serve as a model for such package or even accomodate it in the future.

* Advanced collections are as well a separate area in CS,
  so it requires a lot of effort to maintain and develop a comprehensive package in it.

  Look at `FSET` and `CL-CONTAINERS` for such things.

* MOP.  MOP abstraction layer is a CDR and it is as well an essential part of CL.
  It is implemented and well supported by `CLOSER-MOP`.

### Features

The following symbols are added to `*FEATURES*`:

- `:split-sequence` (by `RUTILS.SEQUENCE`, `RUTILS`, and `RUTIL`)
- `:iter` (by `RUTILSX.ITER` and `RUTILSX`)


## Organizational notes

(c) 2009-2014, Vsevolod Dyomkin <vseloved@gmail.com>

See LICENSE for usage permissions.
See AUTHORS for credits.

