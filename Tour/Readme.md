# A Tour of Đ (Edh)

You want to run a bare **Edh** interpreter (the **REPL**), to play with the
language basics, to develop new batteries merely in **Edh** the surface
language, or do other sorts of fun stuffs. (Note there can be
[File Operating Edh](https://github.com/e-wrks/foedh)
for shell scripting support and other derivatives)

While a real **Haskell** + **Edh** application would certainly include it's
unique **Edh** interpreter with more powerful batteries written in the host
language (i.e. **Haskell**) installed, in addition to the default ones.
See [Edh Im](https://github.com/e-wrks/edhim) for an example.

- [Running the REPL (a bare interpreter)](#running-the-repl-a-bare-interpreter)
  - [Favouring Cabal](#favouring-cabal)
  - [Favouring Stack](#favouring-stack)
  - [Run with verbose (or lean) log level](#run-with-verbose-or-lean-log-level)
  - [Multi / Single line input modes](#multi--single-line-input-modes)
  - [Paste code snippets from this Tour](#paste-code-snippets-from-this-tour)
- [Package / Module Structures](#package--module-structures)
- [Code Structure](#code-structure)
  - [Operators](#operators)
  - [Comprehensions](#comprehensions)
  - [Branches with Value/Pattern Matching](#branches-with-valuepattern-matching)
    - [Case-Of](#case-of)
- [Procedures](#procedures)
  - [Host Procedures](#host-procedures)
  - [Method Procedures](#method-procedures)
  - [Generator Procedures](#generator-procedures)
  - [Interpreter Procedures](#interpreter-procedures)
  - [Class (Constructor) Procedures](#class-constructor-procedures)
  - [Inheritance Hierarchy](#inheritance-hierarchy)
- [Go-Routine / Defer](#go-routine--defer)
- [Event Sink / Reactor](#event-sink--reactor)
- [Indexing](#indexing)
- [More Magic Methods](#more-magic-methods)
- [Reflection](#reflection)
- [Terminology](#terminology)
  - [World](#world)
  - [Package](#package)
  - [Module](#module)
  - [Function (or lack thereof)](#function-or-lack-thereof)
  - [Operator](#operator)
  - [Procedure](#procedure)
  - [Entity](#entity)
    - [Attribute](#attribute)
    - [Symbol](#symbol)
  - [Scope](#scope)
  - [Value / Type](#value--type)
  - [Object / Class](#object--class)
    - [This reference](#this-reference)
    - [That reference](#that-reference)
    - [Super](#super)
  - [EHI (Edh Host Interface)](#ehi-edh-host-interface)

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

### Multi / Single line input modes

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

## Package / Module Structures

Very similar to the deeply-nested
[`node_modules` folder structures](https://nodejs.org/api/modules.html#modules_loading_from_node_modules_folders)
as with [NodeJS](https://nodejs.org), for an **Edh** program, _Just_
translate the following folder/file names:

- `node_modules` -> `edh_modules`
- `index.js` -> `__init__.edh`
- `*.js` -> `*.edh`

_Maybe_ find & replace all occurences according to above rules, then
https://github.com/npm/cli is right ported to manage **Edh** projects
already (kidding).

## Code Structure

### Operators

Check out [operator.edh](./operator.edh)

```bash
Đ: {
Đ|  1:   method localOverrides * {
Đ|  2:     # overide the (++) operator within this procedure only, avoid
Đ|  3:     # polluting the module scope
Đ|  4:
Đ|  5:     before = 'You' ++ ' and ' ++ 'me'
Đ|  6:     operator ++ (lhv, rhv) {
Đ|  7:       # inside the overriding operator definition, the overridden,
Đ|  8:       # original operator is available as was before the overide
Đ|  9:       lhv ++ ' ⭕ ' ++ rhv
Đ| 10:     }
Đ| 11:     after = 'You' ++ ' and ' ++ 'me'
Đ| 12:
Đ| 13:     before 🆚 after
Đ| 14:   }
Đ| 15: }
<method: localOverrides>
Đ:
Đ: localOverrides()
Đ: ℹ️ <interactive>:6:5
🌀 What's the difference?
     You and me
  🆚
     You ⭕  and  ⭕ me

Đ:
```

### Comprehensions

Check out [comprehension.edh](./comprehension.edh)

You do **list** / **dict** / **tuple** comprehensions in **Edh** with
the _comprehension_ / _concatenation_ operator (**=<**):

The (**=<**) operator does comprehension as well as concatenation by default:

```bash
Đ: [] =< for n from range(5) do n
[ 0, 1, 2, 3, 4, ]
Đ:
Đ: {} =< for n from range(5) do 'square of '++n : n*n
{ "square of 0":0, "square of 1":1, "square of 2":4, "square of 3":9, "square of 4":16, }
Đ:
Đ: () =< for n from range(5) do n
( 0, 1, 2, 3, 4, )
Đ:
Đ: {} =< for (k,v) from zip(('a', 'b', 'd'), [3, 7]) do k:v
{ "a":3, "b":7, }
Đ:
```

If you would like comprehension be aligned with traditional semantics that only
create fresh ones, you can do this:

```bash
Đ: import * 'batteries/SeparateConcatAndComprehOp'
<object: <module>>
Đ: [3, 7] <=< [2, 9]
[ 3, 7, 2, 9, ]
Đ: [3, 7] =< [2, 9]
* 😱 *
💔
📜 <interactive> 🔎 <adhoc>:1:1
📜 =< 🔎 /qw/m3works/edh/edh_modules/batteries/SeparateConcatAndComprehOp.edh:11:37
📜 error 🔎 <hostcode>:1:1
💣 You don't comprehend into non-empty ones!
👉 <Genesis>:1:1
Đ:
```

### Branches with Value/Pattern Matching

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

#### Case-Of

## Procedures

### Host Procedures

### Method Procedures

### Generator Procedures

### Interpreter Procedures

### Class (Constructor) Procedures

### Inheritance Hierarchy

`this` and `that`

This is by far a much under explored area in **Edh**, it's supposed to
be the rival of
[Decorators in Python](https://wiki.python.org/moin/PythonDecorators)

And maybe a rival of
[Macros in Julia](https://docs.julialang.org/en/v1/manual/metaprogramming/#man-macros-1)
? Though never in performance-wise respects.

## Go-Routine / Defer

## Event Sink / Reactor

## Indexing

## More Magic Methods

## Reflection

## Terminology

### World

> While impure computer programs tend to change the world, you create ones for
> them to manipulate, but with potentials limited.

The _world_ is a **niche**, **sandbox** or similar concepts you can call it.
To develop a **Haskell** + **Edh** application is somewhat like **playing God**,
you define an **Edh** world with its physics in entirety - no more, no less,
just what you **say** (_\*_), that ought to be there.

_\*_ not really enough to just say it,
[talk is cheap](https://lkml.org/lkml/2000/8/25/132)
, put the code there.

See this 190 lines of
[world modeling code](https://github.com/e-wrks/edhim#world-modeling-code-in-haskell)
for an example.

### Package

Just a folder tree containing various `*.edh` file, resides directly or nested
some levels within a `edh_modules` parent directory, so that reachable from
`import` statements from **Edh** code.

See [Package / Module Structures](#package--module-structures)

By far no meta data format has been defined for an **Edh** package, but there
ought to be one, together with a package tool managing version dependencies
as well as to auto download & install packages from some shared repositories.

### Module

A **module** in **Edh** is a single `*.edh` file, imported by some **Edh**
code or **Haskell** code via the **EHI**.

See [Package / Module Structures](#package--module-structures)

### Function (or lack thereof)

To stay conceptually clear for the object system (which is
[procedural](https://en.wikipedia.org/wiki/Procedural_programming)
per se) living together with the
[functional](https://en.wikipedia.org/wiki/Functional_programming)
parts in the host language (i.e. **Haskell**), there are only **procedures**
but no **function** in **Edh** the surface language.

Simply put, in **Edh** terminology, a **procedure** tends to _change the world_,
while a **function** must stay
[side effect](<https://en.wikipedia.org/wiki/Side_effect_(computer_science)>)
free (well, there's no such a thing qualifies within an **Edh** world).

### Operator

**Edh** allows **operator** declaration and override similar to how **Haskell**
allows you. You can even roll your own, arbitrary new operators with a
precendence you'd like with.

You should be supprised that the following language constructs are all
implemented as overridable **operator**s in **Edh**:

- branch
  - (`->`)
- assignment
  - (`=`), (`+=`), (`-=`), (`*=`), (`/=`)
- logical arithmetic
  - (`&&`), (`||`)
- ternary
  - (`&>`), (`|>`)
- list/dict/tuple comprehension/concatenation
  - (`=<`)
- list prepend / dict insert/update
  - (`=>`)
- string coercing concatenation
  - (`++`)
- event publish
  - (`<-`)

Meaning you can override them for the entire **Edh** world, or part of the
program **scope** (e.g. a **module**, a **class**).

Also see [**operator** procedure] below.

> Note that all are _left-associative_ _infix_ operators in **Edh**, except
> a few hardcoded _prefix_ operators:

- (`+`) prefix plus
- (`-`) prefix minus
- (`not`) prefix bool negation
- (`|`) guard

### Procedure

There are 2 kinds of procedures:

- **constructor** procedure, including:

  - **class** procedure (runs with a new object as `this`/`that`)

    defined by a `class`statement in **Edh** code

  - **module** procedure (runs with the new module object as `this`/`that`)

    defined by a `*.edh` file which is imported by **Edh** code

- **method** procedure (runs with lexical `this` and hierarchical `that`), including:

  - vanilla **method** procedure

    defined by a `method` statement in **Edh**,
    which is a vanilla callable after an alphanumeric name

  - **operator** procedure

    defined by an `operator` statement in **Edh**,
    which is a **binary** or **trinary** callable after a name with just operator symbols
    i.e. `=~!@#$%^&|:<>?+-*/` plus other
    [Unicode](http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table)
    [Symbols](https://hackage.haskell.org/package/base/docs/Data-Char.html#v:isSymbol)
    .

    the **binary** form receives `left'hand'value` and `right'hand'value` as arguments

    the **trinary** form receives `callerScope`, `left'hand'expr` and `right'hand'expr`
    as arguments

  - **generator** procedure

    defined by a `generator` statement in **Edh**,
    which is only callable by a `for-from-do` loop, and contains `yield` expressions

  - **interpreter** procedure

    defined by an `interpreter` statement in **Edh**,
    which is same as a vanilla **method** procedure except it receives arguments in
    reflective expr forms rather than evaluated values, in addition to the reflective
    `callerScope` as first argument

### Entity

An **entity** in **Edh** is the backing storage for a **scope**, with possibly an
**object** mounted to it with one **class** and many **supers**

> Well there actually can exist multiple **object**s mounted to one same entity,
> with different **class**es and/or set of **supers**, but that's the black magic
> you want to avoid.

#### Attribute

An entity stores **attribute**s associated with alphanumeric or symbolic keys.

#### Symbol

**Symbol** in **Edh** is much the same as
[Symbol in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol)

A **symbol** can stand in place of an alphanumeric name, used to
address an attribute from an **object entity**, but symbols are
private to its owning scope, can not be imported from out side
of the scope, thus serves encapsulation purpose in object
structure designs.

And symbol values reside in a lexical outer scope are available
to its lexical inner scopes, e.g. a **symbol** bound to a **module** is
available to all **procedure**s defined in the **module**, and a **symbol**
bound within a **class procedure** is available to all its **method**s
as well as nested **class**es.

### Scope

Especially note that **Edh** has no block **scope** as in **C**
family languages, **JavaScript** neither does before **ES6**,
**Python** neither does until now (2020).

There is only **procedure scope** in **Edh**, and there are 2 kinds of
procedures, see [Procedure](#procedure).

Every **procedure** call will create a new **scope**, with a new
[entity](#entity) created for it, that:

- if it is a **constructor procedure** call, a new **object** of the
  called **class**, or the `<module>` class defined by the world,
  is allocated mounting the **entity**, serving `this` object of the
  **scope**;

- if it is a **methd procedure** call, no new **object** is created,
  and the **scope** inherits `this` **object** from the lexical outer
  **scope**, and the original **object** from which the **method**
  was obtained, becomes `that` **object** in this **scope**. `that`
  either contains the **method** in its **entity** as an **attribute**,
  or inherits the **method** from one of its **supers**.

### Value / Type

> Everything in **Edh** is a value

Code may tell better here:

```haskell
-- | everything in Edh is a value
data EdhValue =
  -- | type itself is a kind of (immutable) value
      EdhType !EdhTypeValue
  -- * end values (immutable)
    | EdhNil
    | EdhDecimal !Decimal
    | EdhBool !Bool
    | EdhString !Text
    | EdhSymbol !Symbol

  -- * direct pointer (to entities) values
    | EdhObject !Object

  -- * mutable containers
    | EdhDict !Dict
    | EdhList !List

  -- * immutable containers
  --   the elements may still pointer to mutable data
    | EdhPair !EdhValue !EdhValue
    | EdhTuple ![EdhValue]
    | EdhArgsPack ArgsPack

  -- * host procedures callable from Edh world
    | EdhHostProc !HostProcedure
    | EdhHostOper !Precedence !HostProcedure
    | EdhHostGenr !HostProcedure

  -- * precedures defined by Edh code
    | EdhClass !Class
    | EdhMethod !Method
    | EdhOperator !Operator
    | EdhGenrDef !GenrDef
    | EdhInterpreter !Interpreter

  -- * flow control
    | EdhBreak
    | EdhContinue
    | EdhCaseClose !EdhValue
    | EdhFallthrough
    | EdhYield !EdhValue
    | EdhReturn !EdhValue

  -- * event sink
    | EdhSink !EventSink

  -- * reflection
    | EdhExpr !Expr

edhValueStr :: EdhValue -> Text
edhValueStr (EdhString s) = s
edhValueStr v             = T.pack $ show v

edhValueNull :: EdhValue -> STM Bool
edhValueNull EdhNil                = return True
edhValueNull (EdhDecimal d       ) = return $ D.decimalIsNaN d || d == 0
edhValueNull (EdhBool    b       ) = return $ b == False
edhValueNull (EdhString  s       ) = return $ T.null s
edhValueNull (EdhDict    (Dict d)) = Map.null <$> readTVar d
edhValueNull (EdhList    (List l)) = null <$> readTVar l
edhValueNull (EdhTuple   l       ) = return $ null l
edhValueNull (EdhArgsPack (ArgsPack args kwargs)) =
  return $ null args && Map.null kwargs
edhValueNull _ = return False
```

### Object / Class

A **class** in **Edh** is defined by a `class` statement in **Edh**
code in form of a **class** procedure.

A **class** procedure constructs objects when called, on each invocation,
it implicitly creates an object taking the **class** and _many_ (reads
zero-to-multiple) **supers** (declared by _many_ `extends` statements
within the **class** procedure) mounted on the **scope** **entity** of
the **class** procedure call.

> A **class** procedure can explicitly return another **object**, another
> **value** or even `nil`, but again, this is the black magic you want to
> avoid.

Note in traditional **Object Oriented** languages, the **class** is
sensibly a part of the definition of the world structure, which carries
rather _static_ semantics, **Python** had made the **class** more
dynamic in constitutional respects, but seemed most people have ignored
that and continued to model the program's world with statically sensing
classes.

The **class** descends to be just a (well still 1st class) citizen in
an **Edh** world, it's conventionally serving _attribute grouping_
purpose for the majority cases, i.e. an **object** should be sensed as
a _namespace_ for **attribute**s otherwise not possible to stay together
except being members of a same, single **object**'s underlying
**entity**.

#### This reference

`this` refers to the nearest context **object** in lexical scope hierarchy.

#### That reference

`that` refers to the **object** against which current running
**procedure** is resolved, i.e. the target **object** of attribute
resolution in finding the current (**method**) **procedure** as a callee.

`that` either contains the **method** procedure in its own **entity** as
an **attribute**, in which case `that` should be the same as `this`,
or `that` inherits the **method** from one of its **supers**, which is,
`this`.

It makes sense to remember that `this` **object** was (or is being)
constructed by the nearest lexical **class** **procedure**, while `that`
**object** was not.

#### Super

During the run of a **class** procedure, each `extends` statement adds
its specified **object** as a **super** object of `this` object under
construction.

> Well a **method** procedure can contain `extends` statements too and
> when called will add **super**(s) possibly after its `this` **object**
> has been constructed. Still this is the black magic you want to avoid.

Last **super** added is the first attempted target in attribute resolution
against `this` object, if no attribute associated with the interesting key
directly presents in `this` object's underlying **entity**.

### EHI (Edh Host Interface)

With **Haskell** as the host language, **Edh** as the surface language,
the **EHI** defines the interface for host code in **Haskell** to create
& control embedded **Edh** worlds, and to splice host (typically
side-effects free, i.e. pure, and fast-in-machine-speed)
functions, wrapped as host procedures, with procedures written
in **Edh**, those do arbitrary manipulations on arbitrary objects
in the world, but, less speedy as with interpreted execution.
