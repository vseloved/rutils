# Announcing RADICAL-UTILS (a.k.a RUTILS 3.0)


## TL;DR

`RUTILS` is to my knowledge the most comprehensive and all-encompassing
suite of syntactic utilities to support modern day-to-day `Common Lisp` development.
It aims to simplify the experience of both serious system-level development
and rapid experimentation in the REPL.

The library has stabilized over more than 3 years of evolution,
it introduces some substantial improvements not available elsewhere,
has a thorough documentation and a good test suite.
The only thing it lacks is, probably, a manual, yet example usage can be found
in my other libraries (notably, [CL-REDIS](https://github.com/vseloved/cl-redis)
and [SHOULD-TEST](https://github.com/vseloved/should-test)).


## A short history

According to git history I've started the project `RUTILS`
(originally, `REASONABLE-UTILITIES`) on Saturday, Sep 12 2009, almost 4,5 years ago.
Yet, I've never done any serious announcement of it.
One of the reasons was that it's kind of controversial
to release "yet another"™ utilities library for Common Lisp,
so I wanted to see if it would stick without any promotion.
And it hasn't - i.e. it hasn't gotten any serious adoption in the CL crowd
(e.g. only one of the authors of libraries in Quicklisp has used it,
and only once there was a serious external interest to contribute).
Yet, it did stick very well with me and has grown quite well over the years.

And now it has reached version 3 by finally getting a proper test suite,
and I've also come to the conclusion that it makes sense to rename the project
to `RADICAL-UTILITIES` - just to add a bit of self-irony
that should be an essential component of any grown-up open source project. ;)


## So, why `RUTILS`?

Initially, the concept of the library was expressed in its
[manifesto](https://github.com/vseloved/rutils/blob/master/docs/reasonable-utilities.md).
Here are the three main points:

- Become an extension to Common Lisp standard by adding the missing pieces
  that were proposed by notable members of the community in the past
  or have been proven useful in other languages.
  This aim included support of as many [CDR](http://cdr.eurolisp.org/)s, as possible.

- Accumulate all the useful syntactic utilities in one suite.
  Contrary to [ALEXANDRIA][alexandria]), the most widely-used Lisp utilities library,
  that proclaims the concept of a "good citizen" (i.e. it doesn't include such libraries
  as [SPLIT-SEQUENCE][split-sequence], [ANAPHORA](http://common-lisp.net/project/anaphora/),
  or [ITERATE](http://common-lisp.net/project/iterate/)),
  `RUTILS` aims to include all the needed pieces, regardless of their origin
  and presence in other libraries.

- Introduce fine-grained modularity - one of the common complaints about the
  standard is the lack of this property. So, as `RUTILS` is intended
  as an extension of the standard, it makes sense to address the complaint
  This point, actually, arouse interest from the folks at
  [Mathematical Systems Institute](https://github.com/mathematical-systems),
  who proposed to co-develop the library, and that prompted the appearance of version 2.0.
  Yet, their plans had changed, and this collaboration hadn't worked out.
  However, it pushed me to the understanding of what should be done
  to make the library production-ready and generally useful.

`REASONABLE-UTILITIES` has failed in several aspects.
First of all, there was not enough effort put into documentation and testing.
Also, `CDR` support didn't prove very useful in practice.
Finally, the modularity aspect, regardless of its theoretical appeal,
doesn't seem to make a difference as well:
`:shadow` is just enough to deal with modularity at this level :)

However, all that was not critical, because the benefits for me personally
were always substantial.


## Version 3.0. Radical?

So, `RUTILS` has gradually solidified. In 2011, I had found the time to document everything,
and last year I had finally added a good test suite
(after, also finally finding a comfortable way to write tests with [SHOULD-TEST][should-test]).

So, here are the main benefits of `RUTILS`:

- Adding a big set of list, hash-table, string, sequence, tree, array
  and other general-purpose utilities.

- Providing a readtable with support for short lambdas (similar to Clojure's),
  literal hash-tables and vectors, and heredoc strings.
  This is the first "radical" step, as it radically cracks on boilerplate,
  and is a good use of the standard reader facilities (as well, as the excellent
  [NAMED-READTABLES][readtables] library).
  In general, I would say that support making hash-tables a first-class citizen
  is the main achievement of `RUTILS`.

- And the second "radical" step - adding shortcuts for many standard Common Lisp operators
  and some of `RUTILS` utilities, as well. This serves three purposes: simplifies
  introduction of many operations to beginners, reduces typing in the
  REPL, and saves on horizontal line space (I adhere to 80-characters rule).
  This is a debatable decision, yet the stance of `RUTILS` is to provide the choice,
  but not enforce it.

- Adding a `pair` structure instead of cons-pair. Frankly, there's nothing very
  wrong with cons-pair, except for the ugly middle-dot syntax for it,
  but it's, I believe, a general agreement in the CL community, that cons-pair
  is legacy and should be deprecated. Getting rid of it also allows to retire
  `car` and `cdr` - which is a necessary step despite a strong affection to them
  with many seasoned lispers (myself included, although I'm not very seasoned :)

Some more radicality may be found in the system `RUTILSX`, which is treated as `contrib`.
Here I plan to place the stuff which value may not really be well-proven.
Currently, it includes `iterate` with keywords from `keyword` package
(this solves naming conflicts and allows to use arbitrary clause keywords),
my own version of a generic `bind` (I had a couple of attempts at it),
and, perhaps, the most radical of all - the addition of `~` operator
for generic access to elements of arbitrary structures. Think:

    (~ #{:foo '(1 2 3}} :foo 1) => 2  ;; #{} is a literal hash-table syntax

Finally, as mentioned earlier, `RUTILS` also includes a comprehensive test suite
that covers all, but the most basic and straightforward functions and macros.
Most other utility libraries don't.


## FAQ (imaginary)

[ALEXANDRIA][alexandria] is the most widely used CL utilities library,
and, actually, CL library in general. Why not use it?

> I don't have anything against it, as it's just useful.
> Yet, if I was using it, I'd still need to depend on several other libraries
> (which is not a problem now with [quicklisp][ql]).
> Still, I'd also have to include my own stuff in each project,
> as no external library provides reader macros for hash-tables and lambdas,
> as well as many other small things provided by `RUTILS`.

What about [quickutil](http://quickutil.org/)?

> I think, it solves the wrong problem. The main problem is utilities' raw utility :)
> Lack of modularity is often mentioned as a drawback of the Lisp standard,
> but practice shows that it's just a perception issue, not a real-world one.
> I believe that the level of modularity provided by `RUTILS` is just good enough,
> and even it isn't utilized so far (but, maybe, in the future it will be to some extent).
> The `QUICKUTIL` approach just adds unnecessary bookkeeping
> and introduces another import mechanism, while sticking with the standard package
> import also would work and doesn't create additional mental tax and confusion.

> Also, I've come to the conclusion that Lisp's flat namespaces are, actually,
> a win over most implementations of hierarchical namespaces,
> but that's a subject of another rant.

What about other individuals' utility suits (`metatilities`, `arnesi`,
`fare-utils`, ...) - I remember, that [Xach](http://twitter.com/xach)
has counted around 13?

> Most of them are not supported. They also are usually a random
> collection of stuff without a vision. Yet, sometimes it makes sense to use them as well:
> for instance, I've used [ARNESI](http://common-lisp.net/project/bese/arnesi.html)
> for its delimited continuations facility and had even put it a fork on my
> [github](https://github.com/vseloved/arnesi) to fix some bugs.
> Although, as discussed below, a finer-grained approach is better -
> see [CL-CONT][cont].

Have you heard of [cl21][cl21]?
How does it compare with `RUTILS`?

> Surely. It is a similar effort to `RUTILS`, but with a much bolder aim.
> I, personally, doubt that it will succeed in it, because Lisp's culture is quite conservative,
> and you need to get a buy-in from the community in order to make such radical moves.
> The problem with this is that no one has enough motivation,
> given that Lisp is already good enough,
> and there's no central authority to steward the evolution process.

> So, the way to go, as for me, is to make small incremental improvements and get them adopted.
> This has always worked in the past, and there are many good examples:
> [CLOSER-MOP](http://common-lisp.net/projects/closer/closer-mop.html),
> [CL-PPCRE](http://weitz.de/cl-ppcre/), [NAMED-READTABLES][readtables],
> [Marco Antoniotti's libraries](http://within-parens.blogspot.com/),
> [CL-CONT][cont], [OPTIMA][optima], etc.

> The main improvements, proposed by `cl21` can or are already addressed by such libraries,
> including `RUTILS`. For instance, `cl21` proposes the same literal syntax for hash-tables.
> Another utility already present in `RUTILS` is a generic sequence iteration loop:
> `doeach` in `cl21` and `doseq` in `RUTILSX`.

> The only thing that, IMHO, may make sense to explicitly modernize in Lisp is the reader,
> as it has 2 hard-coded cases for handling `.` and `:`. I'm thinking
> in the lines of providing access to the previously read item and thus allowing
> for definition of arbitrary infix syntaxes. But this needs much more research...

What about speed and performance?

> Lisp is a dynamic language with good performance characteristics.
> One of the reasons for that is the standard library engineered with performance in mind.
> It is manifested in the presence of various specific functions
> for working with different data structures (like `nth`, `svref`, `aref`, `char` accessors).
> This prompts a valid criticism as being unfriendly to newcomers.
> The solution, in my opinion, is to build on top of that generic versions
> using capabilities provided by CLOS. The sequence-manipulation part of the standard
> sets such an example with the likes of `elt`.
> `RUTILS` continues along this track. But there's an obvious drawback of loosing
> the performance benefit. I think that the Lisp approach here is balanced,
> as it's always possible to fall back to the "low-level" specific functions
> (that should always be provided), and at the same time to use the easy approach
> in 95% of the case when performance isn't critical.

In `RUTILS`, there's very little support for functional programming. Why is it missing?

> There are several aspects of functional programming that are not present in CL standard.
> One that is usually addressed by utilities is function composition and currying.
> It is also addressed by `RUTILS` but in an uncommon way - with sharp-backquote reader macro.
> In my opinion it's a more concise, yet easier to understand approach.
> Here are a couple of examples taken from [cl21][cl21] docs and elsewhere:

>      #`(member % '("foo" "bar") :test 'string=) is a generalized approach to currying
>      #`(sin (1+ %)) is the equivalent of (compose #'sin #'1+) in cl21
>      #`(and (integerp %) (evenp %)) is the equivalent of (conjoin #'integerp #'evenp) in cl21
>      #`(or (oddp %) (zerop %)) is the equivalent of (disjoin #'oddp #'zerop) in cl21

> The benefit of this approach is that it is the same for any kinds of composition,
> while you need to define your own functions to accommodate them with the "functional" style:
> for example, how do you join on `xor`? (btw, `xor` is also provided by `RUTILS`)

> Other functional features, like lazy evaluation, pattern matching,
> or functional data-structures, are much more specific in use
> (for example, I rarely use them if at all), and they have dedicated support elsewhere.
> See [CLAZY](http://common-lisp.net/projects/clazy/), [OPTIMA][optima],
> and [FSet](http://common-lisp.net/project/fset/Site/index.html) for examples.

Where's the documentation you're mentioning?

> It's in the docstrings and subsequently in the excellent
> [quickdocs](http://quickdocs.org/rutils/api). With its appearance,
> I can only recommend everyone not to waste time on creating a function
> documentation for your library, and focus on manual and use cases
> instead. Though, we need to wait for the update of the project to the
> most recent quicklisp.


  [alexandria]: http://common-lisp.net/project/alexandria
  [split-sequence]: https://github.com/sharplispers/split-sequence
  [should-test]: https://github.com/vseloved/should-test
  [readtables]: http://common-lisp.net/projects/named-readtables/
  [cl21]: http://www.reddit.com/r/lisp/comments/1vtueu/cl21_common_lisp_in_the_21st_century/
  [cont]: http://common-lisp.net/projects/cl-cont/
  [optima]: https://github.com/m2ym/optima
