# RUTILS Tutorial

## Overview 

RUTILS is split into two parts: core (package `rutils`) and contrib (package `rutilsx`). These are aggregate packages that just re-export the symbols that are provided by the specific packages like `rutils.anaphora` or `rutils.list`. Overall, there are 17 parts of the core, which are described, in more detail, in this tutorial. They include (with some changes and additions) 3 facilities, which are also available from separate libraries: SPLIT-SEQUENCE, ITERATE, and ANAPHORA. Besides, it defines 2 lightweight wrapper data structures: `pair` and `hash-set`.

There's also the package `rtl` that includes the core plus short names for a lot of basic Lisp operations.

Contrib holds "experimental" stuff (in the sense that it's not totally conventional even for me) that, gradually, migrates to core. I won't talk more about it in the tutorial: those who are interested can check on their own or ask questions.

RUTILS is mainly intended to be `use`d: either the package `rutils`/`rtl` or a particular package like `rutils.hash-table`. However, for easy REPL-based workflows, there are also packages `rutils-user`/`rtl-user`/`rutilsx-user` which are `cl-user` plus the appropriate RUTILS package.


## Installation

The library is available from Quicklisp. The simplest sequence to get started, from the REPL, is:

```
CL-USER> (ql:quickload :rutils)
CL-USER> (in-package :rtl-user)
CL-USER> (named-readtables:in-readtable rutils-readtable)
```

If you want to work with the most current version (for instance, the latest 5.0 release isn't in Quicklisp yet) you can grab it from github. The recommended way is to put it in your `~/common-lisp/` folder. Afterwards, it will be automatically found by `ql:quickload`. Yet, if you want to be sure (or if you put it into a different folder) you may precede the `quickload` call with the following:

```
CL-USER> (push "~/common-lisp/rutils/" asdf:*central-registry*)  ; or use the path where you've put rutils
```


## Readtables

The major external dependency of RUTILS is [named-readtables](https://common-lisp.net/project/named-readtables/), which facilitates managing of readtables (that are defined by the Lisp standard but don't have adequate high-level API there). Essentially, readtables are similar to packages, but, instead of providing new names, they provide new reader-macros i.e. additional syntax. `named-readtables` allows using readtables in the same manner as packages: there's `in-readtable` that acts similar to `in-package` and `defreadtable` that is, to some extent, an analog of `defpackage`. You can learn more on these from the library docs.

RUTILS defined the following syntactic extensions available from `rutils-readtable`.

1. Literal lambdas

Clojure has introduced the idea of shorthand syntax for anonymous functions with predefined argument names. RUTILS provides 2 interchangeable versions of such syntax:

```
RTL-USER> ^(+ % %%)
#<FUNCTION (LAMBDA (&OPTIONAL % %%)) {1022395A6B}>
```

and

```
RTL-USER> #`(+ % %%)
#<FUNCTION (LAMBDA (&OPTIONAL % %%)) {1022395BAB}>
```

Unlike Clojure, only 2 implicit arguments are supported: `%` and `%%`. This covers more than 80% of use cases. And, think of it, when you have a lambda with more than 2 arguments, most probably, it's not so simple to be written in shorthand form. After all, it's supposed that these small functions should span a single expression and at most 3 lines of code...

Although, multiple-expression lambdas are also supported: `^((print %) (1+ %%)) => (lambda (&optional % %%) (print %) (1+ %%))`

2. Literal hash-tables

The ability to initialize the hash-table with values is essential to maintaining code declarativeness. Once again, similar to Clojure, Python, Ruby, and other languages provide it. So, we also do, and, also in 2 variants.

Originally, I came up with the `#{}` notation that should be very familiar to users of other languages:

```
RTL-USER> #{:a 1 :b 2}
#<HASH-TABLE :TEST EQL :COUNT 2>
;; holding 2 key/value pairs: ((:a . 1) (:b . 2))
RTL-USER> #{equal "a" 1 "b" 2}
#<HASH-TABLE :TEST EQUAL :COUNT 2>
;; holding 2 key/value pairs: (("a" . 1) ...)
```

However, it doesn't play very well with Lisp code formatting, in particular, in Emacs, so an alternative paren-based syntax was also added: `#h().

```
RTL-USER> #h(:a 1 :b 2)
#<HASH-TABLE :TEST EQL :COUNT 2>
RTL-USER> #h(equal "a" 1 "b" 2)
#<HASH-TABLE :TEST EQUALP :COUNT 2>
```

3. Literal dynamic vectors

Lisp provides literal syntax for vectors: `#()`. Unfortunately, it creates only constant vectors, but vectors with variable contents are much more widely used. For this purpose, we introduce `#v()` — similar to `#h()` for hash-tables. It creates a non-adjustable normal vector. If you want the vector to also be adjustable use `vec` (from `rutils.array`). So, now there are 3 ways to easily create and initialize a vector in a declarative manner:

```
RTL-USER> #(1 2 3)
#(1 2 3)
RTL-USER> #v(1 2 3)
#(1 2 3)
RTL-USER> (vec 1 2 3)
#(1 2 3)
```

Don't be fooled by the same literal representation, though :D

4. Dot notation for accessing struct/object slots

The absence of shorthand access to object fields was always one of the common complaints of outsiders. Now, you can show them RUTILS :)

We suggest the following syntax: `@object.slot#index`. It requires prepending of the object with `@` (like it's done for variables in such languages like Perl or PHP). The dot is expanded into `smart-slot-value` (see below) and `#` into `elt`. So, this syntax covers both dot access and `[]`-style array element access.

Compare `(elt (nth 1 (foo-slot2 (bar-slot1 obj)) 0)` (sometimes you have to write similar things) with `@obj.slot1.slot2#1#0`. May be considered cryptic and unlispy. But may be much easier to read also...

5. Heredoc strings

Heredoc is a concept from the Shell and Perl that allows representing strings without the need for escaping. The variant RUTILS provides uses the open-close syntax: `#/ ... /#`.

```
RTL-USER> #/This is a string
which allows using the quote (") without escaping/#
"This is a string
which allows using the quote (\") without escaping"
```

It might be very useful for docstrings, to represent JSON and other documents, etc.

Note, that it may not always play well with some REPL (e.g. SLIME), which count open/close quotes.


## Main Syntactic Additions

### rutils.symbol

The package `rutils.symbol` provides the basic utilities for manipulating symbols that are also used in most of the other packages.

First of all, it has the classic:

- `with-gensyms`/`with-unique-names`/`w/uniqs`
- and `once-only`

If you don't know how these are used you can read more in PCL or On Lisp.

- `eval-always` is `eval-when` with all 3 modes on
- `ensure-symbol`/`mksym` and `ensure-keyword`/`mkeyw` turn a string into a symbol or keyword (first upcasing it). It has a keyword `:format` argument that allows specifying a format-string for the symbol transformation

Finally, it provides `abbr` that allows to easily define aliases for any functions and macros.

### rutils.misc

This package holds a number of random utilities that didn't fit into any other group.

It includes some logic shorthands:

- `true` is the complement of `null`
- `xor`
- `and2`, `or2`, and `xor2` are 2-element functions that may be used as `:test` arguments (usual `and` and `or` can't)
- `more`/`not-more` and `less`/`not-less` are like `>`/`>=`/`<`/`<=` but allow some arguments to be null

Clojure-style threading macros (`->` and `->>`):

```
RTL-USER> (-> 2
              1+
              (expt % 2))
9
``` 

And a function composition operator (`=>`): `(=> fn1 fn2)` is the same as `(fn1 (fn2 x))`.

There are also 2 case-like control-flow constructs:

- `pcase`, `dcase`
- `switch`, `cswitch`, `eswitch`

And a couple of shorthands I use quite often:

- `void` is `(setf x nil)`. But it seems much more semantic :)
- `2nd` is `(nth-value 1 x)`

Finally,

- `named-lambda`/`fn` allows to created named anonymous functions, for what it's worth :) Their main use-case is improved debugging
- `coercef` coerces and re-assigns the variable (there's also a similar `appendf` for lists)


### rutils.anaphora

There's a separate Lisp library called [ANAPHORA](https://common-lisp.net/project/anaphora/) that provides such control-flow constructs that allow performing simultaneous assignment and logical tests. RUTILS copies the basic and most widely-used ones. `when-it` and `if-it` are among the most used extensions for me. Here's an example:

```
(when-it (foo)
  (bar it))
```

is the same as:

```
(let ((it (foo)))
  (when it
    (bar it)))
```

This is the implicit anaphora, in which `it` variable is created behind-the-scenes. Such an approach is not unseen in Lisp, as there's `call-next-method` that follows the same principle. Yet, RUTILS supports an explicit style, as well:

```
(if-let (x (foo))
        (bar x)
        (baz))
```

is the same as:

```
(let ((x (foo)))
  (if x
      (bar x)
      (baz)))
```

Other supplied anaphoric macros are:

- `cond-it`/`cond-let`
- `dowhile-it`/`dowhile-let`

The names should speak for themselves.

### rutils.bind

This library provides an extensible unification of all Lisp binding operations (`let`, `let*`, `multiple-value-bind`, `destructuring-bind`, `with-slots`,...) — `bind`/`with` (I prefer the second variant). Unlike `iter` or `anaphora`, I didn't take some of the existing binding libraries (e.g. METABANG-BIND) and decided to implement a novel version that provides more intuitive syntax for the common operations: primarily, multiple values, key-based and slot access.

Here's an example:

```
RTL-USER> (with ((a 1)  ; => plain let
                 (b c (values 2 3))  ; more than 2 variables => multiple-value-bind
                 ((d _ e &rest f) '(4 0 5 12 13 14 15))  ; a list => destructuring-bind
                 (((g :g) (h :h)) ? #h(:g 6 :h 7))   ; ? in 2nd position => generic-elt
                 ((i j) @ (make-foo :i 8 :j 9))  ; @ in 2nd position => smart-slot-value   
                 (((k k) (l l)) @ (make 'foo :k 10 :l 11)))
            (list* a b c d e g h i j k l f)))
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
```

It also supports ignoring some of the bindings by putting `_` instead of the variable name — like in the third clause (`destructuring-bind`).

It is extensible as it allows to add more variants that support custom clauses. This is defined in `bind-dispatch` that uses the first 2 arguments to decide, which operation to emit. Let's define a binding clause for `with-open-file` that triggers when the second argument is a pathname:

```
(defmethod bind-dispatch ((arg1 list) (arg2 pathname) &rest args)
  `(with-open-file (,arg1 ,arg2 ,@args)))

RTL-USER> (with ((str "hello world")
                 (out #p"/tmp/foo.txt" :direction :output))
            (write-line str out))
error opening #P"/tmp/foo.txt": File exists
```

### rutils.generic

This package provides the generic accessor `generic-elt` with a shorter alias `?` and setter `generic-setf` (also `(setf (? ...`). Also, generic `copy` function is defined and generic items count (or length, or size) — `tally`.

```
RTL-USER> (defstruct foo bar)
(? (make-foo :bar #h(:baz '(0 1 42)))
   'bar :baz 2)
42
```

Access to struct/object slots is performed using `smart-slot-value`, also defined here. It is an enhanced version of the standard `slot-value`, which searches by the symbol name regardless of the package (using CLOSER-MOP machinery and caching the found accessor). So, `(? (make-foo :bar 42) 'bar)` and `(? (make-foo :bar 42) :bar)`, and even `(? (make-foo :bar 42) 'foo::bar)` will yield the same result 42.

### rutils.iter

Alongside SPLIT-SEQUENCE, this is another external library originally called [ITERATE](https://common-lisp.net/project/iterate/) that is incorporated by RUTILS. With an important modification of defining the clause keys in the keyword package. You can refer to the official documentation for the extensive examples of its usage. Just mind that instead of:

```
(iterate (for item in list)
         (finding elt maximizing (length elt)))
```

you'll have to write:

```
(iter (:for item :in list)
      (:finding elt :maximizing (length elt)))
```

Using keywords not only makes the iteration keys more visible, in the code, but also prevents name conflicts with the `:common-lisp` package.

### rutils.abbr

Here, you will find some short aliases for common long/antiquetely-named Lisp operations.

These are the most impactful (and so controversial) ones:

- `:=` for `setf` (yes, it's using the syymbol from the keyword package, and the sky doesn't fall because of that)
- `:+`/`:-`/`:*`/`:/` are the emoji aliases of `incf`/`decf` and similar operations if they had been defined for multiplication and division
- `make` for `make-instance` (now, there's nice symmetry between `make 'object` and `make-struct`)
- `call` for `funcall` (it was fun to call functions by their symbol-name in the 60's but it has become the norm now)

These two are, primarily, REPL-oriented:

- `defpar` for `defparameter`
- `m1` for `macroexpand-1`

And some more basic Lisp operations:

- `just` for `identity`
- `ds-bind` for `destructuring-bind`
- `mv-bind` for `multiple-value-bind` (although, simple `bind` and its recommended alias `with` is even shorter :)
- `flet*` for `labels`
- `sub` for `subseq`
- `pushx` for `vector-push-extend`

I have to say, I don't use all of these versions myself, but it's, at least, worth considering that not all standard Lisp names are optimal and the search for better ones should continue...


## Data Structure Utilities 

### rutils.pair

This package provides the `pair` struct that is intended to serve as a replacement for cons-cell. There are several reasons for its introduction:

- using `.` for literal conses is unlispy and also prevents from using it in DSLs where it's often needed (unfortunately, `pair` can only slightly help here, but, still, if we know that we won't have to use conses, we can resort to ugly hacks to put the dot character to other use)
- more semantic naming that is more understandable to newcomers (`car` and `cdr`, unfortunately, are used to scare some people off Lisp)

The accessor functions are `pair-left`/`lt` and `pair-right`/`rt`. And the pair may be created by both calling `(make-pair :left :foo :right 42)` and a shorter `(pair :foo 42)`.

`pair` is a list-based struct, which allows it to be used in destructuring similar to cons. At the drawback of somewhat worse performance (another dereference is needed to access the right part). So, if you need to squeeze the last bit of performance, in someplace, you might still need to use cons-pairs. There's also `with-pair` to destructure a single pair:

```
RTL-USER> (with-pair (l r) (pair :foo 42)
            (cons l r))
(:FOO . 42)
```

The utility functions to transform a list of pairs to/from a hash-table are provided:

- `pairs->ht`
- `ht->pairs`

### rutils.list

This package accumulates many list-manipulation utilities from On Lisp, other books, and beyond. After all, the name Lisp originally meant "List Processing", and a lot of stuff was invented to make such manipulation more efficient. But not all of it got into the standard.

- `last1` provides read-write access to the last element. It should be noted that it isn't efficient (`O(n)`) hence, possibly, its absence from the standard
- `assoc1` is the classic missing shortcut to not have to write `(cdr (assoc ...))`; it also works for pairs
- `single`/`dyadic`/`triadic` check if list has exactly 1,2 or 3 elements
- `ensure-list`/`mklist` turns atom arguments into lists while preserving list arguments as is; `atomize` is the opposite operation
- `flatten` turns the list into absolutely flat (removing all nesting) or removes just the number of levels supplied as the optional second argument

```
RTL-USER> (flatten '((1 (2 3)) 4) 1)
(1 (2 3) 4)
RTL-USER> (flatten '((1 (2 3)) 4))  ; here, same as 2 levels
(1 2 3 4)
``` 

- `interleave` turns a number of lists into 1 by interleaving their elements
- `interpose` adds a separator argument after each list's element

```
RTL-USER> (interleave '(1 2 3) '(4 5 6) '(:foo :bar :baz))
(1 4 :FOO 2 5 :BAR 3 6 :BAZ)
RTL-USER> (interpose :foo '(1 2 3))
(1 :FOO 2 :FOO 3)
```

`take` efficiently and without null-pointer exceptions returns a list of the first n (or less, if the original is shorter) elements of the list

Plist- and alist-related stuff:

- `plistp` and `alistp` check if the appropriate conditions for a list being a plist or alist are met; there's also `alist-to-plist` and `plist-to-alist`
- there's also `remove-from-plist` and `delete-from-plist`
- finally, there's `doplist` which iterates the plist by one key-value pair, at once

`appendf`/`nconcf`/`unionf`/`nunionf`/`reversef`/`nreversef` assign the result of the appropriate operation to the variable passed as their argument

`set-equal` compares 2 sets represented as lists for equality

`zip`/`zip-with` and `zip*`/`zip*-with` are like `mapcar` by `list` or an arbitrary function. Where `zip` finishes when at least one of the input lists ends, while `zip*` waits until the last one ends substituting the missing elements with nils.

```
RTL-USER> (zip '(1 2 3) '(4 5 6))
((1 4) (2 5) (3 6))
RTL-USER> (zip-with '+ '(1 2 3) '(4 5 6))
(5 7 9)
RTL-USER> (zip-with '+ '(1 2 3) '(4 5))
(5 7)
RTL-USER> (zip*-with '+ '(1 2 3) '(4 5))
 ; Evaluation aborted on #<TYPE-ERROR expected-type: NUMBER datum: NIL>.
```

`maptimes` is the mapping version of `dotimes`:

```
RTL-USER> (maptimes 10 '1+)
(1 2 3 4 5 6 7 8 9 10)
```

`mapindex` iterates the list and keeps track of the current position in it (there's also `mapcanindex`):

```
RTL-USER> (mapindex (lambda (i x)
                      (print (pair i x)))
                    '(:foo :bar :baz))

(0 :FOO) 
(1 :BAR) 
(2 :BAZ) 
((0 :FOO) (1 :BAR) (2 :BAZ))
```

`mappend`/`mapconcat` or, better, `flat-map` is the non-destructive alternative to `mapcan`.

`range` produces a range of numbers from start to limit with an optional step.

`concat` is the shorthand for `(concatenate 'list ...)`.

`dcons` and `dlistp` are for working with Ron Garret's [D-Lists](https://groups.google.com/forum/#!topic/comp.lang.lisp/pE-4JL9lnAA).

### rutils.array

The two main utilities here are:

- `slice` performs efficient vector subsequencing (also works for strings) by using array-displacement
- `vec` creates a dynamic adjustable vector from its arguments

There's also `dovec` wich iterates the elements of the vector — a more direct way than using `dotimes` + `aref`.

### rutils.string

This package provides a number of very frequently used string utilities and also some things to make basic file-to-string/string-to-file operations more accessible.

String utilities:

- `blankp` tests whether the string is empty (i.e. `(zerop (length string))`
- `strcat` concatenates strings, as well as characters and, actually, anything else that prints to string, including nil (yep, the name is taken from C)
- `strjoin` is a shortcut for `(format nil "~{~A~^<delimiter>~}" ...)`
- `substr` is an efficient substring implementation (based on `slice`, i.e. using displaced arrays) that also allows using negative indices, like in Python, which signify index from the end of the string
- `white-char-p` tests whether the character is one of the 5 basic whitespace-characters (`#\Space`, `#\Tab`, `#\Newline`, `#\Return`, `#\Linefeed`)
- `starts-with` and `ends-with` are more intuitive wrappers around `mismatch`
- `last-char` returns the last character of the given string
- `fmt` is the shortcut for `format nil`

```
RTL-USER> (strcat "foo" nil "bar" #\4 #\2)
"foobar42"
RTL-USER> (strjoin #\Space '("foo" nil :bar 42))
"foo BAR 42"
```

File-related stuff (very commonly used by me):

- `read-file`/`slurp` reads file contents into a string
- `with-out-file` opens a file for writing with the most default parameters: `:if-exists :supersede :if-does-not-exist :create`

### rutils.sequence

This package contains the very popular `split-sequence` family of utilities (it's recommended to utilize it using the shorter `split`/`split-if`/`split-if-not` names), with a minor, but impactful change of the default value of `remove-empty-subseqs` argument to `t` (for me, it's the value I have to provide in 99% of the cases). More docs on `split-sequence` can be found on the internet, in particular, on [Cliki](https://www.cliki.net/SPLIT-SEQUENCE).

Also provided is the `partition-with` function that splits the sequence into groups by a list of delimiters:

```
RTL-USER> (partition-with '(1 5 10) (range 0 20) :test '<=)
((0 1) (2 3 4 5) (6 7 8 9 10))
(1 5 10)
```

While `group` just splits it into groups of the supplied length.

`removef`/`deletef` are, basically, aliases for `(setf x (remove x ...` and the same with `delete`.

`doindex` iterates the sequence keeping track of the index of the current element (it coerces the argument to a vector). Compare it with `mapindex` from `rutils.list`.

```
RTL-USER> (doindex (i item '(:foo :bar :baz))
            (print (pair i item)))
(0 :FOO)
(1 :BAR)
(2 :BAZ)
```

`shuffle` creates an algorithmically properly shuffled copy of the sequence and `nshuffle` does that in-place.

`rotate` returns a copy with elements shifted by n:

```
RTL-USER> (rotate '(:foo :bar :baz) 2)
(:BAZ :FOO :BAR)
```

Efficient length comparison is performed by the following utilities:

- `emptyp` tests if the sequence has length zero.
- `length=` checks if the sequence length is exactly the second argument
- and `equal-lengths` tests if the sequences have the same lengths

`last-elt` retrieves the last element (also setfable).

`safe-sort` is normal `sort` that copies the input sequence prior to its application (the normal `sort` in-place behavior was a source of many hard-to-find bugs, for me)

`keep`/`keep-if`/`keep-if-not` are the complements to `remove`/`remove-if`/`remove-if-not`.

`sum`/`product` reduce a sequence of numbers to a number by adding/multiplying them.

And `map*` is a DWIM version of `map`. It doesn't ask for the result type and just uses the type of its first argument.

### rutils.hash-table

Hash-tables have, sadly, very rudimentary API, in the standard, despite their current ubiquity. I guess, at the time they were introduced, there the understanding of their universal utility hadn't been formed yet. So, RUTILS bridges that gap.

Alongside the reader support, provided by the readtable, literal syntax is also supported by the printer with `print-hash-table`/`print-ht`. Besides, there's also a way to plug into normal printing, with `toggle-print-hash-table`:

```
RTL-USER> #h(:foo 42)
#<HASH-TABLE :TEST EQL :COUNT 1 {10251850A3}>
RTL-USER> (toggle-print-hash-table)
WARNING:
   redefining PRINT-OBJECT (#<STRUCTURE-CLASS COMMON-LISP:HASH-TABLE>
                            #<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>) in DEFMETHOD
T
RTL-USER> #h(:foo 42)
#{
  :FOO 42
 } 
RTL-USER> (toggle-print-hash-table)
NIL
RTL-USER> #h(:foo 42)
#<HASH-TABLE :TEST EQL :COUNT 1 {10251F5F43}>
```

Some additional operations were added:

- `sethash` is a shorthand for `(setf (gethash ...))`
- `takehash` is like `remhash`, but it also returns the removed element
- `getsethash` either gets the value from the table when it's present there or sets it and returns the newly set value; the setting is performed on-demand and no calculation is performed if the value is already there

Iterating a hash-table with `maphash` or `loop :for keys :beign :the ...` is not the most natural way. A do-style macro is missing, so it was introduced as `dotable`:

```
RTL-USER> (let ((rez ()))
            (dotable (k v #h(1 :foo 2 :bar) rez)
              (when (oddp k)
                (push v rez))))
(:FOO)
```

It supports automatic ignoring of `_` argument and, also, iterating over alists.

Another idea I had was to distinguish hash-table-related operations with a hash-sign at the end. It brought to the definition of a group of alternative hash-table operations:

- `get#` - `gethash`
- `set#` - `sethash`
- `getset#` - `getsethash`
- `take#` - `takehash`
- `in#` - a new operation, which is a shorthand for `(2nd (gethash ...`
- `p#` - `print-hash-table`

Another important missing piece of hash-table UX was easy access to all the keys/vals. It is covered by `hash-table-keys`/`ht-keys` and `hash-table-vals`/`ht-vals`.

Besides, alongside `pairs->ht`/`ht->pairs` there's also:

- `hash-table-to-alist`/`ht->alist` and `hash-table-from-alist`/`alist->ht`
- `hash-table-to-plist``ht->plist` and `hash-table-from-plist`/`plist->ht`

Finally, is also sometimes needed `merge-hash-tables`/`merge-hts`, `merge-hash-tables-with`/`merge-hts-with`.

### rutils.hash-set

From the efficiency standpoint, hash-sets should be the goto set implementations, but, in Lisp, it seems like sets were introduced prior to hash-table so the simpler list-based variants are supported in the standard. Here, we provide a hash-based alternative based on the hash-table with all `t` values:

`hash-set` will create a set from a list of items (hash-table-test may be specified).

All the operations end in `#` similar to the hash-table aliases:

- `add#` adds an item to the set (and the hash-table `rem#` can be used to remove it)
- `inter#`/`union#`/`diff#`/`xor#` are the basic set-theoretic operations

Example usage of a hash-set:

```
RTL-USER> (let ((h1 (hash-set 'eql :foo))
                (h2 (hash-set 'eql :bar)))
            (format t "~&Original hash-set:~%")
            (p# h1)
            (add# :foo h1)
            (format t "~&After item addition:~%")
            (p# h1)
            (format t "~&Union with another:~%")
            (p# (union# h1 h2))
            (format t "~&Intersect with another:~%")
            (p# (inter# h1 h2)))
Original hash-set:
#{
  :FOO T
 } 
After item addition:
#{
  :FOO T
 } 
Union with another:
#{
  :FOO T
  :BAR T
 } 
Intersect with another:
#{
 } 
```

### rutils.kv

Key-values are more than just hash-tables and alists (although, these are the most wide-spread ones, in the Lisp world). A generic kv-access protocol is defined here: `keys` and `vals` allows to define methods to access exactly those, `kvs` returns key-value pairs as a list (possible flavors are: plist, alist, and dlist).

Besides, there's a generic `mapkv`, which iterates a single key-value structure and returns a similar structure with the calculated values assigned to the same keys, and `dokv`, which uses `mapkv` for do-style iteration. However, using `dotable` will be more efficient for hash-tables and alists (although not extensible).

```
RTL-USER> (mapkv ^(1+ %%) #h(:foo 0 :bar 1))
#{
  :FOO 1
  :BAR 2
 } 
```

As you see, `mapkv` expects a function of 2 arguments: the key and the value.

### rutils.tree

The tree utilities, mostly, automate list-based trees iteration.

`dotree`/`maptree` and `doleaves`/`mapleaves` allow iterating the tree by subtrees or just the leaves in do- and map-styles: i.e. map will also produce an altered copy of the iterated list.

```
RTL-USER> (dotree (subtree '(1 (2 3) (4 (5 6))))
            (print subtree))
(1 (2 3) (4 (5 6))) 
(2 3) 
3 
(4 (5 6)) 
(5 6) 
6 
NIL

RTL-USER> (doleaves (node '(1 (2 3) (4 (5 6))))
            (print node))
3
6
NIL

RTL-USER> (maptree ^(if (oddp %) :foo :bar)
                  '(1 (2 3) (4 (5 6))))
(:FOO (:BAR :FOO) (:BAR (:FOO :BAR)))
```

## Afterword

In this tutorial, I've described, maybe, 80-90% of what RUTILS has to offer. If you want to learn more, browse the [docs](http://quickdocs.org/rutils/) (but wait for the update), the tests or the codebase itself (I tried to make docstrings comprehensible and clear).

Also, I wanted to clearly state that the major part of RUTILS is not my invention: it was taken from other libraries, extracted from books, internet forum discussions. Many ideas were borrowed from other languages. My additions are not that big and are, mostly, variations on the existing topics. The principal idea was not to invent something new but, rather, to carefully gather the existing stuff scattered over many places.

I have used RUTILS in almost all of my Lisp projects, for more than 7 years already. I never had any problems or conflicts in connection to that. However, you should note that my main platform is SBCL and it's where I do most of the testing. Specific support for other Lisp compilers was occasionally contributed by other users.

This is my personal Top-10 of the most frequently used/useful features of RUTILS:

1. `?`
2. `with`
3. Shorthand lambdas: `^()`
4. Anaphoric `if-it` and `when-it`
5. `toggle-print-hash-table` and the literal hash-table syntax `#h()`
6. The major abbreviations: `:=`, `make`, `call`
7. `read-file` and `with-out-file`
8. `split`
9. `dotable`, `doindex`
10. `get#`/`in#`/`set#`/...

But that's just the tip of an iceberg: I could easily extend it to 30 items that I use regularly, in my code.

So, you may not like some of the choices made by RUTILS (that's one of the reasons it's called radical, after all), but, in fact, it nicely coexists with any other Lisp code and provides a plethora of small improvements that can make your day-to-day Lisp workflow more efficient and pleasant. You can use them as a whole if you happen to have a similar mindset to mine, or partially (for instance, by importing one of its packages like `rutils.bind`), or even cherry-pick a couple of items you need and copy-paste it to your own codebase. Anyway, I'd be glad if it comes handy...
