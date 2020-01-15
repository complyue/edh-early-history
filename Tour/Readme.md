# A Tour of Edh

- [Concepts](#concepts)
  - [World](#world)
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

## Concepts

### World

You create a world in **Haskell**, for some **Edh** programs to change,
the world is a niche, sandbox or similar concepts you can call it.

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
