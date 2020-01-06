# A Tour of Edh

- [Philosophy](#philosophy)
- [Zen](#zen)
- [Concepts](#concepts)
  - [Function (or lack thereof)](#function-or-lack-thereof)
  - [Procedure](#procedure)
  - [Entity](#entity)
  - [Scope](#scope)
  - [Object](#object)
- [Procedures](#procedures)
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

## Zen

> The _zen_ of **Edh** is to be _Edhic_, that to be _Edhic_,
> is to be more _Pythonic_ than being _Pythonic_

## Concepts

### Function (or lack thereof)

### Procedure

### Entity

### Scope

### Object

## Procedures

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
