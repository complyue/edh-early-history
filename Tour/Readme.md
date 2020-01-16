# A Tour of Đ (Edh)

You want to run a bare **Edh** interpreter (the REPL) to play with the language
basics, to develop new batteries written in **Edh** the surface language, etc.
While a real **Haskell** + **Edh** application would certainly include it's
unique **Edh** interpreter with more powerful batteries written in the host
language (i.e. **Haskell**) installed, in addition to the default ones.
See [Edh Im](https://github.com/e-wrks/edhim) for an example.

- [Running the REPL (a bare interpreter)](#running-the-repl-a-bare-interpreter)
  - [Favouring Cabal](#favouring-cabal)
  - [Favouring Stack](#favouring-stack)
  - [Run with verbose (or lean) log level](#run-with-verbose-or-lean-log-level)
  - [Multi/Single line input modes](#multisingle-line-input-modes)
  - [Paste code snippets from this Tour](#paste-code-snippets-from-this-tour)
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

## Running the REPL (a bare interpreter)

```bash
git clone https://github.com/e-wrks/edh
cd edh
```

### Favouring [Cabal](https://www.haskell.org/cabal)

```shell
cabal v2-install edhi --overwrite-policy=always
```

### Favouring [Stack](https://haskellstack.org)

```shell
stack install
```

### Run with verbose (or lean) log level

```shell
export EDH_LOG_LEVEL=DEBUG
# export EDH_LOG_LEVEL=WARN
edhi
```

```shell
$ export EDH_LOG_LEVEL=DEBUG
$ edhi
>> Bare Đ (Edh) Interpreter <<
* Blank Screen Syndrome ? Take the Tour as your companion, checkout:
  https://github.com/e-wrks/edh/Tour/
Đ:
```

### Multi/Single line input modes

The repl runs in single-line input mode by default, while **unindented**
curly braces start & end multi-line input mode:

```bash
$ edhi
>> Bare Đ (Edh) Interpreter <<
* Blank Screen Syndrome ? Take the Tour as your companion, checkout:
  https://github.com/e-wrks/edh/Tour/
Đ: a=3
3
Đ: {
Đ|  1: x=1
Đ|  2: y=2
Đ|  3: }
2
Đ: (x, y)
( 1, 2, )
Đ:
```

### Paste code snippets from this Tour

Code in `*.edh` files in this **Tour** directory are designed to be
copy-and-paste-able to the **REPL**, e.g. pasting:

```c++
{
  operator 📣 5 (lhv, rhv) {
    runtime.info <| rhv ++ ' is telling ' ++ lhv
  }

  operator 🆚 1 (lhv, rhv) {
    runtime.info <| "🌀 What's the difference?\n     "
      ++ lhv ++ '\n  🆚\n     ' ++ rhv
  }
}

'a tale' 📣 'the goat'

let (a, b) = ( 'Orange', 'Apple', )
a 🆚 b
```

You'll see:

```bash
Đ: {
Đ|  1:   operator 📣 5 (lhv, rhv) {
Đ|  2:     runtime.info <| rhv ++ ' is telling ' ++ lhv
Đ|  3:   }
Đ|  4:
Đ|  5:   operator 🆚 1 (lhv, rhv) {
Đ|  6:     runtime.info <| "🌀 What's the difference?\n     "
Đ|  7:       ++ lhv ++ '\n  🆚\n     ' ++ rhv
Đ|  8:   }
Đ|  9: }
<operator: (🆚) 1>
Đ:
Đ: 'a tale' 📣 'the goat'
Đ:
Đ: let (a, b) = ( 'Orange', 'Apple', )
ℹ️ <interactive>:2:5
the goat is telling a tale
Đ: a 🆚 b
Đ: ℹ️ <interactive>:6:5
🌀 What's the difference?
     Orange
  🆚
     Apple

Đ:
```

## Concepts

### World

While impure computer programs tend to change the world, you create ones
for them to manipulate, but with potentials limited.

The world is a niche, sandbox or similar concepts you can call it.

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
