# Đ (Edh) - The next-big-things ought to happen with Haskell not C/C++

## Quick Start

- Take [A Tour of Đ (Edh)](./Tour/)
- Clone or derive the scaffold:
  [EdhIm - Đ (Edh) doing Instant Messaging](https://github.com/e-wrks/edhim)

## What is Đ (Edh)

**Edh** as a programming language is born just recently,
but it's vastly neither new, major ideas all come from successful languages
like **Python**, **Go**, **JavaScript**, and especially **Haskell**,
verified in the field. There're a few creative attempts though, but in mind
is kept that overly smart not to be about.

**Edh** code is imperative, runs embedded in **Haskell** (GHC), interpreted.

The killer feature may be the very
[Haskell implementation](http://hackage.haskell.org/package/stm)
of
[Software transactional memory](https://en.wikipedia.org/wiki/Software_transactional_memory)
brought into an _Object_ layer, giving you lock-free (a.k.a optimistic locking)
transactions, with intuitive as well as pragmatic constructs to
program data consistency under heavy concurrency.

- [Quick Start](#quick-start)
- [What is Đ (Edh)](#what-is-%c4%90-edh)
- [The name](#the-name)
- [Philosophy](#philosophy)
  - [About Everything](#about-everything)
  - [Object? - Yes, Oriented? - No](#object---yes-oriented---no)
  - [Functional? - Try not to abuse this concept](#functional---try-not-to-abuse-this-concept)
  - [Performance Goals](#performance-goals)
- [Zen of Edh](#zen-of-edh)
- [Licensing](#licensing)
- [Academic relationship](#academic-relationship)
- [A joke](#a-joke)

**Edh** competes with [Python](https://python.org) to help **Haskell**
instead of **C**/**C++** to be the breeding ground for next phenomenal
pieces of software, after [TensorFlow](https://tensorflow.org),
[Pandas](https://pandas.pydata.org/), [Numpy](https://numpy.org/) etc.
by providing equaly good or even better language constructs to rivals
in **Python**.

Take the [Tour](./Tour/) to see what **Edh** has to offer.

[**Julia**](https://julialang.org) is an alternative all-in-one solution
for the **next-big-things**, but has a learning curve not too less
steep than **Haskell**, **Edh** is faithful to get people with just
**Python**/**JavaScript**/**Go** knowledge and skills started with
a [world](./Tour/#world) in **Haskell**.

## The name

**Edh** stands for **Event Distributing & Hosting**

Đ is a more stylish, single letter, symbolic name of **Edh** the language

## Philosophy

### About Everything

In **Edh**:

> Everything is a **value**,
> the **object** is a type of **value** among other (mostly immutable)
> types

This is part of the reason why **Edh** is not an _Object Oriented_
language (see next section), to be contrasted with **Python**, where:

> Everything is an **object**,
> a **value** is an **object** of some type among other types

### Object? - Yes, Oriented? - No

Many don't consider **Go** ([GoLang](https://golang.org)) an
_Object Oriented_ programming language, neither is **Edh** in similar
respect. **Edh** does pointer-wise
[Type Embedding](https://go101.org/article/type-embedding.html)
in **Go** spirit, while it takes a small step further to offer `that`
reference, which refers to a descendant record from an ancestor
method, in addition to `this` reference which refers to the lexical
self record.

### Functional? - Try not to abuse this concept

In a pure _functional_ language like **Haskell**, everything is a computation,
[Referencial Transparency](https://wiki.haskell.org/Referential_transparency)
is an intrinsic (and really fundamental) property. Bearing the world-changing
potential, a procedure in **Edh** can never qualify as a _function_.

But if you ask about
[Functional programming](https://www.geeksforgeeks.org/functional-programming-paradigm/)
as a possible paradigm you _DIY_, **Edh** is supportive as well as
other mainstream languages.

### Performance Goals

**Edh** struggles to dig performance improvements majorly out of the
_human_ aspect from the _human:machine_ pair, offer wider tolerance,
therefore better leverage, over diversified skill sets/levels among
all crew members onboard.

Raw machine performance squeezing is offloaded to
[GHC](https://wiki.haskell.org/GHC) who has been undescribable good
at it since inception.

## Zen of Edh

> Always keep in mind that **Edh** the language can be as horrible as
> **JavaScript** if overly used in wrong ways (clues as described
> [here](https://medium.com/javascript-non-grata/the-top-10-things-wrong-with-javascript-58f440d6b3d8)
> , some issues have been fixed in **Edh** while others remain pretty
> the same), so do programing, modeling and thinking in
> [**Haskell**](https://www.haskell.org),
> (i.e. be a _Haskeller_). For someone who you must work with but
> hasn't feel comfortable with **Haskell** code, ask him/her to use
> **Edh**, and write
> [host procedures](https://github.com/e-wrks/edh/Tour/#host-procedures)
> wrapping idiomatic models in **Haskell** to help get his/her job done
>
> Under certain circumstances, there are sweet spots balanced between
> _imperative_ and _declarative_ stylish, when put together, **Edh**
> code can be made even more concise than **Haskell** code (
> [see proof here](https://github.com/e-wrks/edhim#full-%C4%91-edh-code-95-loc)
> ). Do **Edh** programming around such sweet spots when you can find
> them, and when you do, be _Edhic_, that to be _Edhic_, is to be
> more _Pythonic_ than being
> [**Pythonic**](https://www.python.org/dev/peps/pep-0020)
>
> Whenever you're not sure how to get a job done, think about how a
> [**Gopher**](https://blog.golang.org/gopher) would do it

## Licensing

I (Compl Yue) choose to distribute **Edh** related code under BSD,
I believe BSD license is proper, it is even permissive for you
to re-license it under GPL etc. given the BSD clauses kept distributed
together. Though I sincerely hope no one is to attack me by patent
infringement.

## Academic relationship

No academic background relevant so far, but I (Compl Yue) do feel some
ideas here worth further exploration, to be consolidated or even
formalized on theoretical ground. If you are doing relevant CS
researches, **Edh** is yet another piece of friendly _BSD_ licensed
software at your hand, PRs updating information here, including
citation requests for your relevant work are welcomed.

## A joke

Finally let me tell you a joke:

> Q:
>
> > So what's good on **Edh** the programming language?
>
> A:
>
> > **Edh** is good because it's arguably a _three star_ language
> > (`***argspk`), as **Python** is arguably a _two star_ language
> > (`**kwargs`), while others are at most _one star_ languages or
> > even _no star_ ones.
