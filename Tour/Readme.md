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
- [Micro Structures](#micro-structures)
  - [Lossless Decimal for Numbers](#lossless-decimal-for-numbers)
  - [Arguments (Un / Re) Packing](#arguments-un--re-packing)
    - [Compatible with Pythonic arguments](#compatible-with-pythonic-arguments)
    - [pkargs() the utility](#pkargs-the-utility)
    - [Args Unpacking on receiving](#args-unpacking-on-receiving)
  - [Args Repacking on receiving](#args-repacking-on-receiving)
    - [let does args](#let-does-args)
    - [for-from-do loop does args](#for-from-do-loop-does-args)
    - [import does args](#import-does-args)
    - [generator does args](#generator-does-args)
  - [Comprehensions](#comprehensions)
  - [Operators](#operators)
  - [list/dict modification](#listdict-modification)
  - [Tenary](#tenary)
  - [Logging](#logging)
  - [Type Inspections](#type-inspections)
  - [Branches with Value Matching / Pattern Matching](#branches-with-value-matching--pattern-matching)
    - [Value Matching](#value-matching)
    - [Case-Of](#case-of)
    - [Pattern Matching](#pattern-matching)
    - [Branching semantics, Fallthrough](#branching-semantics-fallthrough)
- [Procedures](#procedures)
  - [Host Procedures](#host-procedures)
  - [Method Procedures](#method-procedures)
  - [Generator Procedures](#generator-procedures)
  - [Interpreter Procedures](#interpreter-procedures)
  - [Class (Constructor) Procedures](#class-constructor-procedures)
  - [Inheritance Hierarchy](#inheritance-hierarchy)
- [Go Routine / Defer](#go-routine--defer)
- [Event Sink / Reactor](#event-sink--reactor)
- [Indexing](#indexing)
- [More Magic Methods](#more-magic-methods)
- [Reflection](#reflection)
- [Terminology](#terminology)
  - [World](#world)
  - [Package](#package)
  - [Module](#module)
  - [Entity](#entity)
    - [Attribute](#attribute)
    - [Symbol](#symbol)
  - [Scope](#scope)
  - [No Variable](#no-variable)
    - [Attribute Assignment](#attribute-assignment)
    - [Attribute Read](#attribute-read)
    - [Transactional Semantics Amplification](#transactional-semantics-amplification)
  - [Transaction (STM)](#transaction-stm)
    - [The ai keyword](#the-ai-keyword)
  - [Function (or lack thereof)](#function-or-lack-thereof)
  - [Operator](#operator)
  - [Procedure](#procedure)
    - [Host Procedure](#host-procedure)
      - [Vanilla Host Procedure](#vanilla-host-procedure)
      - [Host Operator Procedure](#host-operator-procedure)
      - [Host Generator Procedure](#host-generator-procedure)
    - [Constructor Procedure](#constructor-procedure)
      - [Class Procedure](#class-procedure)
      - [Module Procedure](#module-procedure)
    - [Method Procedure](#method-procedure)
      - [Vanilla Method Procedure](#vanilla-method-procedure)
      - [Operator Procedure](#operator-procedure)
      - [Generator Procedure](#generator-procedure)
      - [Interpreter Procedure](#interpreter-procedure)
  - [Go Routine](#go-routine)
    - [The go keyword](#the-go-keyword)
    - [The defer keyword](#the-defer-keyword)
  - [Event Sink](#event-sink)
    - [The reactor Procedure](#the-reactor-procedure)
    - [Break the thread from reactor](#break-the-thread-from-reactor)
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

Note that module `batteries/root` will be imported into root scope of an
**Edh** **world** when default batteries are installed (the most common case
when a **world** is created).

## Micro Structures

### Lossless Decimal for Numbers

In **JavaScript**, even today, all numbers are `float64`, it puzzled me a while,
before I figured out this is the reason why we can not have `Int64Array`
besides `Int32Array`/`Int16Array`/`Float64Array`/`Float32Array` etc. that
the language simply unable to handle elements from an `Int64Array` if it's
ever provided.

**Python** gives you lossless integers by default, but `float64` for decimal:

```python
>>> 1.1 + 2.2
3.3000000000000003
>>>
```

**Haskell** forces you to choose a type for every number in your program,
normal options including the lossless `Integer`, bounded `Int`, and
precision-lossing `Double`/`Float`:

```haskell
λ> 1.1 + 2.2 :: Double
3.3000000000000003
λ> 1.1 + 2.2 :: Float
3.3000002
λ>
```

And you have `Data.Scientific` for the rescure, but with its own quirks
from practical daily use:

```haskell
λ> :m +Data.Scientific
λ> x = 1.1 + 2.2 :: Scientific
λ> x
3.3
λ> x / 0
*** Exception: Ratio has zero denominator
λ>
```

> Your compiled **Haskell** program will crash on division-by-zero if not
> handled specifically.

Then here's `Data.Lossless.Decimal` from package
[lossless-decimal](../lossless-decimal)

```haskell
λ> x = 1.1 + 2.2 :: Decimal
λ> x
3.3
λ> x / 0
inf
λ> (-x) / 0
-inf
λ> 0/0 :: Decimal
nan
λ> (0/0 :: Decimal) == 0/0
False
λ> pi = Decimal 1 (-40) 31415926535897932384626433832795028841971
λ> pi
3.1415926535897932384626433832795028841971
λ>
```

All numbers in **Edh** are `Data.Lossless.Decimal` from
[lossless-decimal](../lossless-decimal)
, by default and by all means:

```haskell
Đ: x = 1.1 + 2.2
3.3
Đ: x / 0
inf
Đ: -x / 0
-inf
Đ: 0/0
nan
Đ: nan == nan
false
Đ: y = 7/11
7/11
Đ: ( x, y, x * y )
( 3.3, 7/11, 2.1, )
Đ: pi
3.1415926535897932384626433832795028841971
Đ:
```

### Arguments (Un / Re) Packing

`ArgsPack` is an immutable type of values in **Edh**, it is also part of
the **Edh** call convention.

#### Compatible with Pythonic arguments

```python
method f (x, y=10, *ns, c=3, **kwargs) pass

f (3, 7, 21, *[9, 11], name='doer', **{'msg': "you've got it", 'keynum': 2})
```

#### pkargs() the utility

```bash
Đ: pkargs
<hostproc: pkargs>
Đ:
Đ: apk = pkargs(3,7,5,z=9,y=11)
pkargs( 3, 7, 5, y=11, z=9, )
Đ:
```

#### Args Unpacking on receiving

```bash
Đ: method f (x, y, z, a, b) [x, y, z, a, b]
<method: f>
Đ: f (***apk)
[ 3, 11, 9, 7, 5, ]
Đ:
```

### Args Repacking on receiving

```bash
Đ: method f (*args, **kwargs) [args, kwargs]
<method: f>
Đ: f (***apk)
[ pkargs( 3, 7, 5, ), pkargs( y=11, z=9, ), ]
Đ:
Đ: method f (***argspk) { 'full args': argspk }
<method: f>
Đ: f (***apk)
{ "full args":pkargs( 3, 7, 5, y=11, z=9, ), }
Đ:
```

#### let does args

```bash
Đ: let (x, y, z, a, b) = (***apk)
Đ: [x, y, z, a, b]
[ 3, 11, 9, 7, 5, ]
Đ:
Đ: let (*args, **kwargs) = (***apk); [args, kwargs]
[ pkargs( 3, 7, 5, ), pkargs( y=11, z=9, ), ]
Đ:
Đ: let (***argspk) = (***apk); { 'full args': argspk }
{ "full args":pkargs( 3, 7, 5, y=11, z=9, ), }
Đ:
```

#### for-from-do loop does args

```bash
Đ: for (x, y, z, a, b) from [apk] do runtime.info <| [x, y, z, a, b]
Đ: ℹ️ <interactive>:1:1
[ 3, 11, 9, 7, 5, ]

Đ: for (*args, **kwargs) from [apk] do runtime.info <| [args, kwargs]
Đ: ℹ️ <interactive>:1:1
[ pkargs( 3, 7, 5, ), pkargs( y=11, z=9, ), ]

Đ: for (***argspk) from [apk] do runtime.info <| { 'full args': argspk }
Đ: ℹ️ <interactive>:1:1
{ "full args":pkargs( 3, 7, 5, y=11, z=9, ), }

Đ:
```

#### import does args

```bash
Đ: import (**magics) 'batteries/magic'
<object: <module>>
Đ: magics
pkargs( *=<operator: (*) 7>, +=<operator: (+) 6>, -=<operator: (-) 6>, /=<operator: (/) 7>, )
Đ:
```

#### generator does args

Checkout [argspk.edh](./argspk.edh)

```bash
Đ: {
Đ|  1:   generator g (n) {
Đ|  2:     for i from range(n) do
Đ|  3:       # pack an arguments sender to yield out,
Đ|  4:       # you'd feel it like calling a callback
Đ|  5:       yield pkargs (i, i * i, desc="square of " ++ i)
Đ|  6:   }
Đ|  7:
Đ|  8:   # arguments receiver syntax in for expression,
Đ|  9:   # you'd feel it like defining a callback
Đ| 10:   for (x, y, desc="the result") from g(5) do
Đ| 11:     runtime.info <| (x ++ ": " ++ desc ++ " is " ++ y)
Đ| 12: }
Đ: ℹ️ <interactive>:10:3
0: square of 0 is 0
ℹ️ <interactive>:10:3
1: square of 1 is 1
ℹ️ <interactive>:10:3
2: square of 2 is 4
ℹ️ <interactive>:10:3
3: square of 3 is 9
ℹ️ <interactive>:10:3
4: square of 4 is 16

Đ:
```

### Comprehensions

Check out [comprehension.edh](./comprehension.edh)

You do **list** / **dict** / **tuple** comprehensions in **Edh** with
the _comprehension_ / _concatenation_ operator (**=<**):

The (**=<**) operator does comprehension as well as concatenation by default:

```bash
Đ: [7, 11] =< for n from range(5) do n
[ 7, 11, 0, 1, 2, 3, 4, ]
Đ:
Đ: {'a': 7, 'b': 11} =< for n from range(5) do 'square of '++n : n*n
{ "a":7, "b":11, "square of 0":0, "square of 1":1, "square of 2":4, "square of 3":9, "square of 4":16, }
Đ:
Đ: (31, 17) =< for n from range(5) do n
( 31, 17, 0, 1, 2, 3, 4, )
Đ:
Đ: {} =< for (k,v) from zip(('a', 'b', 'd'), [3, 7]) do k:v
{ "a":3, "b":7, }
Đ:
```

If you would like comprehension be aligned with traditional semantics that only
fresh ones are created, you can do this:

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

### Operators

Check out [operator.edh](./operator.edh)

You can inspect an operator at the REPL, just print it:

```bash
Đ: (++)
<hostop: (++) 2>
Đ: (+=)
<operator: (+=) 2>
Đ:
```

All operators can be overridden in **Edh**

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

Also the implementation of
[SeparateConcatAndComprehOp](../edh_modules/batteries/SeparateConcatAndComprehOp.edh)
demonstrates more operator syntaxes and mechanisms:

```c++
# re-declare (=<) as the concat operator (<=<)
operator <=< 1 () (=<)

# Override operator (=<) to comprehend into empty ones only
operator =< (callerScope, lhe, rhe) {
  lhv = callerScope.eval(lhe)
  if not null(lhv) then
    error("You don't comprehend into non-empty ones!")
  # left-hand value is empty, can do it
  callerScope.eval( makeOp(lhe, "<=<", rhe) )

  # the overridden original (=<) operator is in scope of this proc,
  # but don't do `lhv =< callerScope.eval(rhe)` here, as it won't
  # work as expected when `rhe` is a for-from-do loop
}
```

### list/dict modification

- append to a list/dict
  - just use the comprehension operator as well
- prepend to a list, insert/update single entry to a dict
  - there's the `cons` operator (**=>**) for it

```bash
Đ: let (l, d) = ([3,'foo',5], {'a': 'good', 'b': 9})
Đ: l =< [2,'bar',9]
[ 3, "foo", 5, 2, "bar", 9, ]
Đ: d =< {'b': 1, 'm': 'cool!'}
{ "a":"good", "b":1, "m":"cool!", }
Đ: 'baz' => l
[ "baz", 3, "foo", 5, 2, "bar", 9, ]
Đ: ('n', 'yeah') => d
{ "a":"good", "b":1, "m":"cool!", "n":"yeah", }
Đ:
```

### Tenary

in **C** you do:

```C
onCnd ? oneThing : theOther
```

and in **Python** you do:

```python
onCnd and oneThing or theOther
```

well in **Edh** you do:

```haskell
onCnd &> oneThing |> theOther
```

```bash
Đ: 2 < 1 &> 'no way!' |> 'of coz'
of coz
```

### Logging

Logging is done by an operator (**<|**) too, surprise!

`runtime.xxx` are just number values to specify the target level of
a log record, and the process environment variable `EDH_LOG_LEVEL`
if set, will cause log records with lower target levels be dropped.

```bash
$ export EDH_LOG_LEVEL=WARN
$ edhi
>> Bare Đ (Edh) Interpreter <<
* Blank Screen Syndrome ? Take the Tour as your companion, checkout:
  https://github.com/e-wrks/edh/Tour/
Đ: runtime.warn <| "You won't see source location info in log if the level is WARN or higher"
Đ: ⚠️ You won't see source location info in log if the level is WARN or higher
Đ:
$ export EDH_LOG_LEVEL=DEBUG
$ edhi
>> Bare Đ (Edh) Interpreter <<
* Blank Screen Syndrome ? Take the Tour as your companion, checkout:
  https://github.com/e-wrks/edh/Tour/
Đ: runtime.info <| "Source location is informative most of the time, right?"
Đ: ℹ️ <interactive>:1:1
Source location is informative most of the time, right?
Đ: runtime.debug <| "Especially when trouble shooting some unexpected results."
Đ: 🐞 <interactive>:1:1
Especially when trouble shooting some unexpected results.
Đ: runtime.warn
30
Đ: 30<|'use a number works the same way!'
Đ: ⚠️ <interactive>:1:1
use a number works the same way!
Đ:
```

### Type Inspections

```haskell
Đ: type(7)
DecimalType
Đ: type(3, 'abc', 2:5)
( DecimalType, StringType, PairType, )
Đ: type(type, (+), type(1))
( HostProcType, HostOperType, TypeType, )
Đ: type(pkargs(1,2,k1='a'), type'of'dict={,}, type'of'tuple=(,), type'of'list=[], type'of'nil=nil)
pkargs( ArgsPackType, type'of'dict=DictType, type'of'list=ListType, type'of'nil=nil, type'of'tuple=TupleType, )
Đ:
```

### Branches with Value Matching / Pattern Matching

You may know (**->**) in **Haskell**, it is also in **Edh** but called
the **branch** operator:

```bash
Đ: (->)
<hostop: (->) 0>
Đ: 1 < 2 -> 'it must be!'
it must be!
Đ: 1 > 2 -> 'not the case!'
<fallthrough>
Đ: 'more than truth' -> 'Edh do JavaScript style value coercing?'
<fallthrough>
Đ: _ -> 'the wildcard always matches!'
the wildcard always matches!
Đ:
```

#### Value Matching

`<fallthrough>` is the result value from evaluating a non-matched branch,
it signals the _try-next-case_ semantic. And without an enclosing `case-of`
construct, the _left-hand-value_ to (**->**) operator is matched against
the value `true`. And since **Edh** is strongly typed (though dynamically
typed), there is no _trueish_ semantics for another type of value to be
matched with `true`.

> You'll soon see [Pattern Matching](#pattern-matching), the **branch**
> does [Value Matching](#value-matching) unless **Pattern Matching** is
> invoked by _curly-brace-quoting_ at _left-hand-side_ of the (**->**)
> operator.

#### Case-Of

Check out [case-of.edh](./case-of.edh)

To match against some value determined at runtime, you use `case-of`:

```bash
Đ: case type('the question', 5) of (StringType, DecimalType) -> 'yeath'
yeath
Đ: case type('the question', 5) of (StringType, StringType) -> 'possible?'
<fallthrough>
```

```bash
Đ: {
Đ|  1:   method essay (v) case type(v) of {
Đ|  2:     BoolType -> "to be or not to be, that's a problem"
Đ|  3:
Đ|  4:     DecimalType -> {
Đ|  5:         v<2 -> "consume less, produce more";
Đ|  6:         v<10 -> "no more than " ++ v ++ " cups of coffee a day";
Đ|  7:         _ -> "every one get his/her lucky number"
Đ|  8:     }
Đ|  9:
Đ| 10:     StringType -> {quiz=v fallthrough}
Đ| 11:
Đ| 12:     SymbolType -> {quiz='mistery attracts most people' fallthrough}
Đ| 13:
Đ| 14:     ObjectType -> {
Đ| 15:       quiz = 'I live in ' ++ v?__name__ |> 'no where';
Đ| 16:       fallthrough
Đ| 17:     }
Đ| 18:
Đ| 19:     "do you known, that " ++ quiz ++ " ?"
Đ| 20:   }
Đ| 21: }
<method: essay>
Đ:
Đ: essay(true)
to be or not to be, that's a problem
Đ:
Đ: essay(1)
consume less, produce more
Đ:
Đ: essay(5)
no more than 5 cups of coffee a day
Đ:
Đ: essay(25)
every one get his/her lucky number
Đ:
Đ: essay('magic happens')
do you known, that magic happens ?
Đ:
Đ: essay(Symbol('hidden-secrete'))
do you known, that mistery attracts most people ?
Đ:
Đ: essay(this)
do you known, that I live in <interactive> ?
Đ:
Đ: class C * pass
<class: C>
Đ: essay(C())
do you known, that I live in no where ?
Đ:
```

#### Pattern Matching

Check out [patterns.edh](./patterns.edh)

**Pattern Matching** is invoked by _curly-brace-quoting_ at _left-hand-side_
of the (**->**) operator.

Patterns available by now:

```bash
Đ: {
Đ|  1:   case 3:2:1 of {
Đ|  2:     { x:y } -> 'pair pattern matches the length'
Đ|  3:     { x:y:z } -> 'so this one fires'
Đ|  4:   }
Đ|  5: }
so this one fires
Đ:
Đ: case 3:2 of { (x:y) } -> 'the pair pattern can be parenthesised'
the pair pattern can be parenthesised
Đ:
Đ: case 3*7-5 of { result } -> 'a wild capture pattern receives the ' ++ result
a wild capture pattern receives the 16
Đ:
Đ: case [7, 3, 5] of { head => tail } -> 'snoc pattern does snoc, got ' ++ (head, tail)
snoc pattern does snoc, got ( 7, [ 3, 5, ], )
Đ:
Đ: case (3, 5, 7) of { (x, y, z) } -> 'tuple pattern matches the length'
tuple pattern matches the length
Đ:
Đ: case (3, 5, 7) of { (x, y) } -> 'tuple pattern matches the length'
<fallthrough>
Đ:
Đ: class B () pass
<class: B>
Đ: class C () extends B()
<class: C>
Đ: c = C()
<object: C>
Đ:
Đ: case c of {{ B:b }} -> 'instance resolving pattern obtains the right super instance: ' ++ b
instance resolving pattern obtains the right super instance: <object: B>
Đ:
```

More patterns can be added in the future.

#### Branching semantics, Fallthrough

Check out [branch.edh](./branch.edh)

Once a **branch** has its _left-hand-side_ value or pattern matched, its
_right-hand-side_ expression is evaluated, then its immediate enclosing
block is considered to have been evaluated to the result value, without
consulting any following statements in this block; unless the
_right-hand-side_ of (**->**) is evaluated to a result of `<fallthrough>`,
in which case the rest statements in the block continue to be evaluated
sequentially.

```bash
Đ: {
Đ|  1:   method countdown(n) case type(n) of {
Đ|  2:
Đ|  3:     DecimalType -> {
Đ|  4:
Đ|  5:       n < 1 -> runtime.info <| '  🎉 instantly !!'
Đ|  6:
Đ|  7:       n > 5 -> {
Đ|  8:         runtime.warn <| "  😓 that's too many to count, doing my most ..."
Đ|  9:         n = 5
Đ| 10:         fallthrough # similar to `fallthrough` in Go
Đ| 11:       }
Đ| 12:
Đ| 13:       # (:-) will be parsed as another operator, sep (1 space used below) needed here
Đ| 14:       for i from range(n : 0 : -1) do runtime.info <| '  ⏲️  ' ++ i
Đ| 15:       runtime.info <| '  🎉 !!'
Đ| 16:
Đ| 17:     }
Đ| 18:
Đ| 19:     _ -> # the underscore condition always matches, similar to underscore in Haskell
Đ| 20:       runtime.error <| "I don't know what you want from a " ++ type(n) ++ ': ' ++ n
Đ| 21:
Đ| 22:     runtime.fatal <| "don't worry, this will never happen."
Đ| 23:   }
Đ| 24: }
<method: countdown>
Đ:
Đ: countdown(3)
ℹ️ <interactive>:14:7
  ⏲️  3
ℹ️ <interactive>:14:7
  ⏲️  2
ℹ️ <interactive>:14:7
  ⏲️  1
ℹ️ <interactive>:15:7
  🎉 !!
Đ:
Đ: countdown(50)
⚠️ <interactive>:8:9
  😓 that's too many to count, doing my most ...
ℹ️ <interactive>:14:7
  ⏲️  5
ℹ️ <interactive>:14:7
  ⏲️  4
ℹ️ <interactive>:14:7
  ⏲️  3
ℹ️ <interactive>:14:7
  ⏲️  2
ℹ️ <interactive>:14:7
  ⏲️  1
ℹ️ <interactive>:15:7
  🎉 !!
Đ:
Đ: countdown(-1)
ℹ️ <interactive>:5:7
  🎉 instantly !!
Đ:
Đ: countdown('the hell')
Đ: ❗ <interactive>:19:5
I don't know what you want from a StringType: the hell
Đ:
```

## Procedures

### Host Procedures

Host procedures are written in the host language (i.e. **Haskell**),
e.g.

```haskell
-- | utility null(*args,**kwargs) - null tester
isNullProc :: EdhProcedure
isNullProc !argsSender !exit = do
  !pgs <- ask
  packEdhArgs argsSender $ \(ArgsPack !args !kwargs) -> if null kwargs
    then case args of
      [v] -> contEdhSTM $ do
        isNull <- EdhBool <$> edhValueNull v
        exitEdhSTM pgs exit isNull
      _ -> contEdhSTM $ do
        argsNulls <- sequence $ ((EdhBool <$>) . edhValueNull) <$> args
        exitEdhSTM pgs exit (EdhTuple argsNulls)
    else contEdhSTM $ do
      argsNulls   <- sequence $ ((EdhBool <$>) . edhValueNull) <$> args
      kwargsNulls <- sequence $ Map.map ((EdhBool <$>) . edhValueNull) kwargs
      exitEdhSTM pgs exit (EdhArgsPack $ ArgsPack argsNulls kwargsNulls)
```

```haskell
-- | operator (:) - pair constructor
consProc :: EdhProcedure
consProc [SendPosArg !lhExpr, SendPosArg !rhExpr] !exit = do
  pgs <- ask
  -- make sure left hand and right hand values are evaluated in same tx
  local (const pgs { edh'in'tx = True })
    $ evalExpr lhExpr
    $ \(OriginalValue !lhVal _ _) ->
        evalExpr rhExpr $ \(OriginalValue !rhVal _ _) ->
          contEdhSTM $ exitEdhSTM pgs exit (EdhPair lhVal rhVal)
consProc !argsSender _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)
```

```haskell
timelyNotify :: Int -> EdhGenrCaller -> STM ()
timelyNotify !delayMicros genr'caller@(!pgs', !iter'cb) = do
  nanos <- (toNanoSecs <$>) $ unsafeIOToSTM $ do
    threadDelay delayMicros
    getTime Realtime
  -- yield the nanosecond timestamp to iterator
  runEdhProg pgs' $ iter'cb (EdhDecimal $ fromInteger nanos) $ \_ ->
    timelyNotify delayMicros genr'caller

-- | host generator runtime.everyMicros(n) - with fixed interval
rtEveryMicrosProc :: EdhProcedure
rtEveryMicrosProc !argsSender _ = ask >>= \pgs ->
  case generatorCaller $ edh'context pgs of
    Nothing          -> throwEdh EvalError "Can only be called as generator"
    Just genr'caller -> case argsSender of
      [SendPosArg !nExpr] -> evalExpr nExpr $ \(OriginalValue nVal _ _) ->
        case nVal of
          (EdhDecimal (Decimal d e n)) | d == 1 ->
            contEdhSTM $ timelyNotify (fromIntegral n * 10 ^ e) genr'caller
          _ -> throwEdh EvalError $ "Invalid argument: " <> T.pack (show nVal)
      _ ->
        throwEdh EvalError $ "Invalid argument: " <> T.pack (show argsSender)
```

### Method Procedures

e.g. checkout [str.edh](../edh_modules/batteries/root/str.edh)

```javascript
method join(fst, *rest, sep=', ') {
  s = '' ++ fst # do coerce to string now, in case only one arg passed
  for one from rest do s = s ++ sep ++ one
  s
}
```

```bash
Đ: join(3,5,7)
3, 5, 7
Đ:
Đ: join('I','am','good', sep=' ')
I am good
Đ:
```

### Generator Procedures

Values can be exchanged between the generator and the `do` expr

```bash
Đ: generator ss n while true n = yield n*n
<generator: ss>
Đ:
Đ: for n from ss(3) do { runtime.info<|n; if n > 100 then break else n }
Đ: ℹ️ <interactive>:1:23
9
ℹ️ <interactive>:1:23
81
ℹ️ <interactive>:1:23
6561
Đ:
```

### Interpreter Procedures

Check out [interpreter.edh](./interpreter.edh)

```bash
Đ: {
Đ|  1:
Đ|  2:   interpreter lazy(callerScope, expr) {
Đ|  3:     method lazyEval () callerScope.eval(expr)
Đ|  4:   }
Đ|  5:
Đ|  6:   a = 5; b = 3
Đ|  7:   sum = lazy(a + b)
Đ|  8:
Đ|  9:   runtime.info <| " once upon a time it's " ++ sum()
Đ| 10:
Đ| 11:   a = 7
Đ| 12:   runtime.info <| " then later it's " ++ sum()
Đ| 13:
Đ| 14: }
Đ: ℹ️ <interactive>:9:3
 once upon a time it's 8
ℹ️ <interactive>:12:3
 then later it's 10
Đ:
```

This is by far a much under explored area in **Edh**, it's supposed to
be the rival of
[Decorators in Python](https://wiki.python.org/moin/PythonDecorators)

And maybe a rival of
[Macros in Julia](https://docs.julialang.org/en/v1/manual/metaprogramming/#man-macros-1)
? Though never in performance-wise respects.

But lacks a well thought out, reflective **AST** (especially **Expr**)
manipulation API.

### Class (Constructor) Procedures

### Inheritance Hierarchy

`this` and `that`

## Go Routine / Defer

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
some levels within the **Edh** interpreter's
[cwd](https://en.wikipedia.org/wiki/Working_directory)
or a `edh_modules` parent directory, so that reachable from `import` statements
from **Edh** code.

See [Package / Module Structures](#package--module-structures)

By far no meta data format has been defined for an **Edh** package, but there
ought to be one, together with a package tool managing version dependencies
as well as to auto download & install packages from some shared repositories.

### Module

A **module** in **Edh** is a single `*.edh` file, imported by some **Edh**
code or **Haskell** code via the **EHI**.

See [Package / Module Structures](#package--module-structures)

Note that module `batteries/root` will be imported into root scope of an
**Edh** **world** when default batteries are installed (the most common case
when a **world** is created).

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

There is only **procedure scope** in **Edh**, see [Procedure](#procedure).

Every non-host **procedure** call will create a new **scope**, with
a new [entity](#entity) created for it, that:

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

### No Variable

Despite the existence of
[IORef](https://hackage.haskell.org/package/base/docs/Data-IORef.html),
[STRef](https://hackage.haskell.org/package/base/docs/Data-STRef.html),
[MVar](https://hackage.haskell.org/package/base/docs/Control-Concurrent-MVar.html)
and the very
[TVar](https://hackage.haskell.org/package/stm/docs/Control-Concurrent-STM-TVar.html)
and friends, the
[word "variable" as applied to **Haskell** variables](https://wiki.haskell.org/Variable)
is quite misleading.

**Edh** tries to get rid of it completely, by making **variable** a nonconcept.
But given the strong _imperative_ stylish of **Edh** code, **entity** **attribute**s
(see [Attribute](#attribute)) are likely to be misperceived as _variables_ in
traditional sense of imperative programming languages.

> I urge you to quarantine the _variable_ concept in context of a programming
> language and programming in general (it's still safe and sound in mathematics,
> statistics etc. of course), because that's how current generation of computing
> hardwares work, not how computing works in general.
>
> I still remember the hard time to wrap my head around `x = x + 1` when I was
> young and learning [BASIC](https://wiki.c2.com/?BasicLanguage) in enlightenment
> to programming (in fact to computer at all, on a
> [80286 computer](https://en.wikipedia.org/wiki/IBM_Personal_Computer_XT#IBM_XT_286)),
> the pleasure when I finally established the perception.
>
> But I'm not happy and feel false faith with that now.
>
> Using _variables_ in thoughts is anti-human, imagine yourself allocating a
> box area on a piece of paper, along with other tasks performed, you write sth
> into the box, erase it later followed by writing sth new, and repeat and repeat
> ... I would feel ridiculous. Computers ought to do that on their own, without
> the aid from human programmers.

At least, you should really convert your intuition when coding in **Edh**, for the
sake of the fact that you are programming transactional concurrency in contrast
to traditional single-threaded programs, or multi-threaded programs synchronized.

#### Attribute Assignment

Assignments in **Edh** may look like exactly as to change the value of a variable,
but actually, it is modification to the owning **entity** of the target
**attribute**.

Without an addressing prefix (`xxx.attr`), assigning to a named attribute
(`attr = expr`) is to update the **entity** of the nearest lexical **scope**,
associating with `attr` as the key, to evaluated value from the
_right-hand-side_ expr.

See [Object / Class](#object--class) for the concept of an **object**.

With an addressing prefix (`xxx.attr = expr`), `xxx` must evaluate to a value of
**object** type, assigning to a named attribute (`xxx.attr = expr`) or symbolic
attribute (`xxx.@sym = expr`), is to update the **entity** underlying the
**object** specified by the prefix, associating with `attr` or `@sym` as the key,
to evluated value from the _right-hand-side_ expr.

#### Attribute Read

Similar to the assignment, when `attr` `xxx.attr` and `xxx.@sym` appear in an
expression to be evaluated, those are not _variable_ reads as it looks like,
but actually reads of their respective owning **entities**.

#### Transactional Semantics Amplification

As said above, the read / write of attributes are semantically amplified to
their owning **entities**, to perceive this sense intuitively is crucial in
writing effective **Edh** code, or working **Edh** code at all. Because there
will definitely be chances you must delimit transaction boundaries for some
sections of your **Edh** code, see the [ai keyword](#the-ai-keyword), doing
that improperly or plainly wrong, you will be punished with excessive **stm**
retries or even dropped into infinite vain loops without progress.

Above said may sound pretty dreadful, but it should make you feel better if I
tell you that, when coding an **Edh** world, you can forget about all kinds of
[synchronization primitives](http://www.cs.columbia.edu/~hgs/os/sync.html)
scattered [here](https://docs.python.org/3/library/asyncio-sync.html),
[there](https://golang.org/pkg/sync) and many _otherwheres_
(despite of many **async** frameworks trying to mitigate that disputable
complexity), with every methods you attempt to program concurrency otherwise.

### Transaction (STM)

> EAFP in **Python** spirit:
>
> > Easier to ask for forgiveness than permission. This common Python coding
> > style assumes the existence of valid keys or attributes and catches
> > exceptions if the assumption proves false. This clean and fast style is
> > characterized by the presence of many try and except statements.
> > The technique contrasts with the LBYL style common to many other
> > languages such as C.

Well with
[Software transactional memory](https://en.wikipedia.org/wiki/Software_transactional_memory)
, you don't even ask for either forgiveness or permission, just do
what seems right to do, and it's done (with
[CAVEATs](#transactional-semantics-amplification)
though). It's called
[Optimistic Locking](http://en.wikipedia.org/wiki/Optimistic_locking)
elsewhere but the spirit is an upgraded version of **EAFP**.

In **Edh**, you get an implicit transaction in each of the following cases:

- Attribute assignment

```python
x = y + z
x += y # desugared to x = x+y
```

- Let assignment

```python
let (a.balance, b.balance) = (a.balance + amount, b.balance - amount)
```

- Arguments packing for a procedure call

  including explicit `pkargs(*,**,***)` call to construct an `ArgsPack`,
  which is to be yielded from a generator, published to an event sink etc.

```python
processItemsOfInterest ( *a.cart, *a.wishlist )
```

- pair / tuple / list / dict construction

  while multi-dimensional index construction (`a[x+o, y-3, z*5]`) is a
  special case of tuple construction

```haskell
case (x.pendingItems : x.doneItems) of {( p : d )} -> "Dash:\n"
  ++ join(p, sep='\n') ++ " --- \n" ++ join(d, sep='\n')
```

So above cases are intrinsically _atomic_, while for other cases that
atomicity / isolation are needed, you use:

#### The ai keyword

`ai` stands for **Atomically Isolatedly**, or `AtoIso` for short, it is used
as the transaction boundary delimiter.

`ai <expr>` is a statement where the expr (being a block is common case) is
guaranteed to be evaluated in a same transaction.

As based on [stm](http://hackage.haskell.org/package/stm), **Edh** will
atomically retry the whole transaction if inconsistency found before the tx
can commit successfully. And **Edh** will
[trace](https://hackage.haskell.org/package/base/docs/Debug-Trace.html#v:trace)
out the retries so end users will be aware of them.

Nested **ai** (block) expressions are technically doable, but should be
considered bad smell in the code under common circumstances.

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

Checkout the implementation of the default batteries of **Edh**:
[parts written in **Haskell**](../edh/src/Language/Edh/Batteries/)
and
[parts written in **Edh**](../edh_modules/batteries/)

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

There are 3 kinds of procedures in **Edh**:

- [Host Procedure](#host-procedure)
- [Constructor Procedure](#constructor-procedure)
- [Method Procedure](#method-procedure)

#### Host Procedure

Host procedures are written in the host language (i.e. **Haskell**), and do
not create new **scope**s when called, including:

##### Vanilla Host Procedure

e.g.

```haskell
-- | utility null(*args,**kwargs) - null tester
isNullProc :: EdhProcedure
isNullProc !argsSender !exit = do
  !pgs <- ask
  packEdhArgs argsSender $ \(ArgsPack !args !kwargs) -> if null kwargs
    then case args of
      [v] -> contEdhSTM $ do
        isNull <- EdhBool <$> edhValueNull v
        exitEdhSTM pgs exit isNull
      _ -> contEdhSTM $ do
        argsNulls <- sequence $ ((EdhBool <$>) . edhValueNull) <$> args
        exitEdhSTM pgs exit (EdhTuple argsNulls)
    else contEdhSTM $ do
      argsNulls   <- sequence $ ((EdhBool <$>) . edhValueNull) <$> args
      kwargsNulls <- sequence $ Map.map ((EdhBool <$>) . edhValueNull) kwargs
      exitEdhSTM pgs exit (EdhArgsPack $ ArgsPack argsNulls kwargsNulls)
```

##### Host Operator Procedure

e.g.

```haskell
-- | operator (:) - pair constructor
consProc :: EdhProcedure
consProc [SendPosArg !lhExpr, SendPosArg !rhExpr] !exit = do
  pgs <- ask
  -- make sure left hand and right hand values are evaluated in same tx
  local (const pgs { edh'in'tx = True })
    $ evalExpr lhExpr
    $ \(OriginalValue !lhVal _ _) ->
        evalExpr rhExpr $ \(OriginalValue !rhVal _ _) ->
          contEdhSTM $ exitEdhSTM pgs exit (EdhPair lhVal rhVal)
consProc !argsSender _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)
```

##### Host Generator Procedure

e.g.

```haskell
timelyNotify :: Int -> EdhGenrCaller -> STM ()
timelyNotify !delayMicros genr'caller@(!pgs', !iter'cb) = do
  nanos <- (toNanoSecs <$>) $ unsafeIOToSTM $ do
    threadDelay delayMicros
    getTime Realtime
  -- yield the nanosecond timestamp to iterator
  runEdhProg pgs' $ iter'cb (EdhDecimal $ fromInteger nanos) $ \_ ->
    timelyNotify delayMicros genr'caller

-- | host generator runtime.everyMicros(n) - with fixed interval
rtEveryMicrosProc :: EdhProcedure
rtEveryMicrosProc !argsSender _ = ask >>= \pgs ->
  case generatorCaller $ edh'context pgs of
    Nothing          -> throwEdh EvalError "Can only be called as generator"
    Just genr'caller -> case argsSender of
      [SendPosArg !nExpr] -> evalExpr nExpr $ \(OriginalValue nVal _ _) ->
        case nVal of
          (EdhDecimal (Decimal d e n)) | d == 1 ->
            contEdhSTM $ timelyNotify (fromIntegral n * 10 ^ e) genr'caller
          _ -> throwEdh EvalError $ "Invalid argument: " <> T.pack (show nVal)
      _ ->
        throwEdh EvalError $ "Invalid argument: " <> T.pack (show argsSender)
```

#### Constructor Procedure

Including:

##### Class Procedure

Runs with a new object as both `this` and `that`

Defined by a `class`statement in **Edh** code

##### Module Procedure

Runs with the new module object as both `this` and `that`

Defined by a `*.edh` file which is imported by **Edh** code

#### Method Procedure

Runs with _lexical_ `this` and _inheriting descendant_ `that`, including:

##### Vanilla Method Procedure

Defined by a `method` statement in **Edh**,
which is a vanilla callable after an alphanumeric name

##### Operator Procedure

Defined by an `operator` statement in **Edh**,
which is a **binary** or **trinary** callable after a name with just operator symbols
i.e. `=~!@#$%^&|:<>?+-*/` plus other
[Unicode](http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table)
[Symbols](https://hackage.haskell.org/package/base/docs/Data-Char.html#v:isSymbol)
.

The **binary** form receives `left'hand'value` and `right'hand'value` as arguments

The **trinary** form receives `callerScope`, `left'hand'expr` and `right'hand'expr`
as arguments

##### Generator Procedure

Defined by a `generator` statement in **Edh**,
which is only callable by a `for-from-do` loop, and contains `yield` expressions

##### Interpreter Procedure

Defined by an `interpreter` statement in **Edh**,
which is same as a vanilla **method** procedure except it receives arguments in
reflective expr forms rather than evaluated values, in addition to the reflective
`callerScope` as first argument

### Go Routine

Checkout [Goroutine](https://tour.golang.org/concurrency) as in
[Go](https://golang.org/), if not already.

A **go** _routine_ in **Edh** is very similar to **goroutine**:

#### The go keyword

`go <expr>` is a statement in **Edh** for the specified computation be scheduled
to run in a separate _lightweigth-thread_ , while in case a forked thread throws
an exception, the execption will be
[throwTo](https://hackage.haskell.org/package/base/docs/Control-Concurrent.html#v:throwTo)
the main thread of the **Edh** program.

The sames as in **Go**:

- Things are prepared before you **go**

  i.e. callee procedure arguments are packed, `case-of` target is evaluated,
  `for-from-do` loop iterable is constructed, all in a transaction run by the
  current thread

- There is no _return_

  Once you **go**, you are not _awaited_ or _joined_ anyway

Yet there are differences:

- **Edh** can not only **go** a procedure call:

  You can **go** a **for-from-do** loop, a block, or even a single expression
  e.g. a `if-then` or a `case-of`

- Whenever not **go** calling another procedure:

  The lexical **scope** is shared among all threads forked from the originating
  thread, this is quite the opposite to
  [Share Memory By Communicating](https://blog.golang.org/share-memory-by-communicating)
  in **Go** spirit.

  Well in **Edh** you'd better neither do the **Go** anti-pattern described as

  > communicate by sharing memory

  While it's quite okay to read / modify the shared **scope** if it seems right,
  but for communication purpose, you use one or more [Event Sink](#event-sink)s,
  this is quite the same in **Go** spirit as communicating through
  [channels](https://tour.golang.org/concurrency/2) in **Go**.

  Guess what, you have even the same operator (**<-**) on **event sink**s in
  **Edh** as the **channel** operator in **Go**. But just remember a **Go**
  **channel** does _unicast_ while an **Edh** **sink** does _broadcast_ .

#### The defer keyword

See [Break the thread from reactor](#break-the-thread-from-reactor),
as **Edh** threads can be terminated by `break`s from **reactor**s,
even exception handlers on the thread can cease to run on such terminations.

You use **defer** to do necessary cleanups or to schedule other computations
to run upon thread termination.

`defer <expr>` is a statement in **Edh** for the specified computation be
scheduled to run upon current thread's termination.

Similar to [defer](https://golang.org/ref/spec#Defer_statements) in
**Go**:

- Input to the execution is prepared at scheduling time

  i.e. callee procedure arguments are packed, `case-of` target is evaluated,
  `for-from-do` loop iterable is constructed, all in a transaction run
  immediately by the current thread on encounter of the **defer** statement

- Multiple **defer**s are run in _last-in-first-out_ order upon thread
  termination

Differences with [defer](https://golang.org/ref/spec#Defer_statements) in
**Go**:

- A **defer**ed computation runs upon thread termination in **Edh** while in
  **Go** a **defer**ed function call runs on return of the surrounding
  function

- **Edh** can not only **defer** a procedure call:

  You can **defer** a **for-from-do** loop, a block, or even a single
  expression e.g. a `if-then` or a `case-of`

- Whenever not **defer** calling another procedure:

  The lexical **scope** is shared among the originating thread and other
  **defer**ed computations

### Event Sink

An **event sink** is a value of special type in **Edh**, a new **sink** is
created each time the `sink` keyword is evaluated in **Edh**, as the name
implies, events from everywhere can flow into a **sink**, a **Go** **channel**
can have multiple writers so they are similar in this respect.

But write to a **channel** will block when there is no reader (and the buffer
is full if writing to a buffered **channel**), while write to a **sink**
will always success immediately, the written event is silently dropped when
there is no reader draining the sink.

And when there are multiple readers, a **channel** gives one **msg** to a
unique reader only (i.e. load balancing semantics), while a **sink** gives
the same **event** to all readers (i.e. broadcasting semantics).

#### The reactor Procedure

There is actually a special type of **procedure** in **Edh** not listed in
section [Procedure](#procedure) , a **reactor procedure** is a
[Method Procedure](#method-procedure) per se, but what makes it special
is that it is not bound to a **scope** (as an attribute of the **scope**
**entity**), instead, it is associated with an **event sink** and attached
to a running **Edh** **thread** (**go** routine).

A **reactor** is defined by a `reactor` statement in **Edh** code, specifying
the **sink** to be associated with, a procedure body consists of **Edh** code,
and an **arguments receiver** to receive the event value into the procedure
body's **scope** before running.

Draining of events from each's respective **sink** by **reactor**s attached
to a thread, are run interleaved with normal transactions on the thread, i.e.
between each 2 normal tx processed, all **reactors** are tried to process one
event from each's associated **sink**.

#### Break the thread from reactor

Another special mechanism with **reactor** is that it can conditionally
evaluate to a `<break>` result (by a `break` statement in it's body), in
which case the thread having it attached will be terminated.

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
