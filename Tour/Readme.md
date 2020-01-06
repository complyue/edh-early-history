# A Tour of Edh

- [Philosophy](#philosophy)
  - [About Everything](#about-everything)
  - [Object? - Yes, Oriented? - No](#object---yes-oriented---no)
  - [Performance Goals](#performance-goals)
- [Zen of Edh](#zen-of-edh)
- [Concepts](#concepts)
  - [Functions (or lack thereof)](#functions-or-lack-thereof)
  - [Procedure](#procedure)
  - [Entity](#entity)
  - [Scope](#scope)
  - [Object](#object)
- [Module Structures](#module-structures)
- [Code Structure](#code-structure)
  - [Operators](#operators)
  - [Branches](#branches)
  - [Case-Of](#case-of)
  - [Pattern Matching](#pattern-matching)
- [Procedures](#procedures)
  - [Host Procedures](#host-procedures)
  - [Method Procedure](#method-procedure)
  - [Generator Procedure](#generator-procedure)
  - [Class (Constructor) Procedure](#class-constructor-procedure)
  - [Multiple Inheritance](#multiple-inheritance)
  - [Interpreter Procedure](#interpreter-procedure)
- [Go-Routine and (Event) Sink](#go-routine-and-event-sink)
- [Indexing and Magic Methods](#indexing-and-magic-methods)
- [Reflection](#reflection)

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
_Object Oriented_ programming language, **Edh** is not so in similar
respect. **Edh** does pointer-wise
[Type Embedding](https://go101.org/article/type-embedding.html)
in **Go** spirit, while it takes a small step further to offer you
`that` reference, which refers to a descendant record from an ancestor
method, in addition to `this` reference which refers to the lexical
self record. (There's potential to implement parameterized modules
from this ground up)

### Performance Goals

**Edh** struggles to offer performance advantages majorly out of the
_human_ aspect from the _human:machine_ pair, though the overall
performance should be even better when the gluing is done right.
Raw machine performance squeezing is offloaded to
[GHC](https://wiki.haskell.org/GHC) who has been undescribable good
at it since inception.

## Zen of Edh

> Do program and think in [Haskell](https://www.haskell.org), (i.e. be
> a _Haskeller_), for anyone who you must work with but hasn't feel
> comfortable with **Haskell** code, ask him/her to use **Edh**, and
> write [host procedures](#host-procedures) to help get his/her
> job done
>
> When programming in **Edh**, be _Edhic_, that to be _Edhic_, is
> to be more _Pythonic_ than being
> [_Pythonic_](https://www.python.org/dev/peps/pep-0020)
>
> And whenever you're not sure how to get a job done, think
> about how a [Gopher](https://blog.golang.org/gopher) would do it

## Concepts

### Functions (or lack thereof)

### Procedure

### Entity

### Scope

### Object

## Module Structures

Very similar to deep-nested
[`node_modules` folder structures](https://nodejs.org/api/modules.html#modules_loading_from_node_modules_folders)
with [NodeJS](https://nodejs.org), map the following folder/file names:

- `node_modules` -> `edh_modules`
- `index.js` -> `__init__.edh`
- `*.js` -> `*.edh`

_Just_ find&replace all occurences according to above rules, then
_Maybe_ https://github.com/npm/cli is right ported to manage **Edh**
projects already (kidding).

## Code Structure

### Operators

### Branches

### Case-Of

### Pattern Matching

## Procedures

### Host Procedures

### Method Procedure

### Generator Procedure

### Class (Constructor) Procedure

### Multiple Inheritance

`this` and `that`

### Interpreter Procedure

The [Indexing](#indexing-and-magic-methods) section demonstrates
more sophiscated usage of pattern matching, a simpler example
resides in the `range()` implementation from default batteries:

```javascript
  /**
   * resembles `range` in Python
   */
  generator range(start, stop=nil, step=nil) {

    if nil == stop && nil == step then case start of {
      // enable the hidden *Edhic* version of `range` using pair
      {(start:stop:step)} -> {fallthrough}
      {(start:stop)} -> {fallthrough}
    }

...
```

## Go-Routine and (Event) Sink

## Indexing and Magic Methods

## Reflection
