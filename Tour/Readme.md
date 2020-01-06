# A Tour of Edh

- [Philosophy](#philosophy)
- [Zen of Edh](#zen-of-edh)
- [Concepts](#concepts)
  - [Function (or lack thereof)](#function-or-lack-thereof)
  - [Procedure](#procedure)
  - [Entity](#entity)
  - [Scope](#scope)
  - [Object](#object)
- [Procedures](#procedures)
  - [Host Procedures](#host-procedures)
  - [Method Procedure](#method-procedure)
  - [Generator Procedure](#generator-procedure)
  - [Class (Constructor) Procedure](#class-constructor-procedure)
  - [Interpreter Procedure](#interpreter-procedure)
- [Operators](#operators)
- [Branches](#branches)
- [Pattern Matching](#pattern-matching)
- [Case-Of](#case-of)
- [Go-Routine and (Event) Sink](#go-routine-and-event-sink)
- [Indexing](#indexing)
- [Reflection](#reflection)

## Philosophy

In **Edh**:

> Everything is a **value**,
> an **object** is a **value**

To be contrasted with **Python**, where:

> Everything is an **object**,
> a **value** is an **object**

## Zen of Edh

> Program in [Haskell](https://www.haskell.org), (i.e. be a
> _Haskeller_), for anyone who you must work with but hasn't feel
> comfortable with _Haskell_ code, ask him/her to use **Edh**, and
> write [host procedures](#host-procedures) to help get his/her
> job done
>
> When programming in **Edh**, be _Edhic_, that to be _Edhic_, is
> to be more _Pythonic_ than being
> [_Pythonic_](https://www.python.org/dev/peps/pep-0020)
>
> And whenever you're not sure how do get a job done, think
> about how a [Gopher](https://blog.golang.org/gopher) would do it

## Concepts

### Function (or lack thereof)

### Procedure

### Entity

### Scope

### Object

## Procedures

### Host Procedures

### Method Procedure

### Generator Procedure

### Class (Constructor) Procedure

### Interpreter Procedure

## Operators

## Branches

## Pattern Matching

The [Indexing](#indexing) section demonstrates more sophiscated usage
of pattern matching, a simpler example resides in the `range()`
implementation from default batteries:

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

## Case-Of

## Go-Routine and (Event) Sink

## Indexing

## Reflection
