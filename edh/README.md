# Đ - (WIP) a modern Object layer spliced with Haskell

Missing Python/JavaScript or even Go in shifting to
Haskell? Now you can have Edh, with many goodies from
those languages but running embedded in Haskell.

## Why splice

I have long been amused by how Python and C++ work together.

> I use the modern `pybind11` toolchain, while C++ with `boost` has worked
> longer, and C with Python has a even longer history.

With Python code, you set a stage up with tensors forming a computation network,
with those tensors implemented in C++, while during full speed sprinting,
the tensor code can call Python from C++ through some bindings (`pybind` does
most of that out-of-the-box), to tell the framework to _shift gear_ in some
parts.

Take away is: you use one language/runtime for flexibility/expressiveness,
and another for performance.

Haskell on its own has long achieved both in many domains, but in numeric
crunching, the overwhelming success of Numpy/Pandas come with good reasons.

## Teasers

### Python style argument passing

```javascript
// the Pythonic way of argument receiving
method f (x, y=10, *ns, **kwargs) {
    // ...
}
// the Pythonic way of argument sending
f (3, 7, 21, *[9, 11], name='doer', **{'msg': "you've got it", 'keynum': 2})
```

### and more of argument receiving

```javascript
// wild argument receiver
method g * {
    return "I guess you passed me a name '" ++ name ++ "', or I should've crashed!"
}
// but this is not really a good idea for api signature
g (name="Yesman")

// argument retargetting
method setName (name as this.name) pass
```

### import by argument receiving

```javascript
// while the wild argument receiver is bad for procedures, it's fairly
// acceptable for project local imports:
import * from './lib.edh'

// surely there'are more explicit forms for general imports,
// also see argument renaming here:
import (doA, doB as thatB, **newGoodiesToKnow) from 'awsomelib'
if len(newGoodiesToKnow) > 0 then
    console.debug(
        "there're more functions from awsomelib, check that out:\n\t"
        ++ newGoodiesToKnow.keys())
```

> TODO demostrate `edh_modules` directory structure for dependency management
> akin to npm's `node_modules` structure.

### Go embedding style multiple "inheritance"

This is not geniune object-oriented inheritance though, but you'll go as far
and well as Go [Type Embedding](https://go101.org/article/type-embedding.html)
let you.

```javascript
class B (name) {
    method greeting(guest) {
        console.log("Hello "++guest++", I am "++name++', your guide.')
    }
}

class C () {
    extends B('Nobleman')
}

class D () {
    extends B('Farmer')

    method hello() {
        console.log(`Hello there!`)
    }
}

class E () {
    extends C()
    extends D()
    // an E object will contain 2 copies of B object,
    // the order above matters in resolving the `greeting` method.
}
```

### ES6 style symbol

```javascript
...
```

### list/dict/tuple comprehension

```haskell
[] =< for x from range(100) do x*x
{} =< for x from range(100) do ("square of " ++ x, x*x)
(,) =< for x from range(100) do x*x
```

You can even comprehend into list/dict with existing data,
this may be good or bad depending on your opinion. But you
know what, you can override the `(=<)` operator to refuse
to comprehend into non-empty list/dict!

> TODO Đ repl session demo here

### list/dict modification

```javascript
let (l, d) = ([3,7,5], {'a': 2, 'b': 9})
// append to a list/dict, just use the comprehension operator as well
l =< [2,9]; d =< {'b': 1, 'm': 'cool!'}
// prepend to a list, insert/update single entry to a dict,
// there's a dedicated operator (=>) for it
11 => l; ('n', 'yeah') => d
```

### generators

```haskell
g = generator (n) {
    for i from range(n) do
        yield (i, i * i)
}

for (x, y) from g(100) do
    console.log('got ' ++ x ++ '>>' ++ y)
```

### goroutine, defer and concurrency ctrl

```haskell
class EventMonitor (chSub, name="observer") {
    method ackEvents () (
        for inEvent from chSub do
            console.info(name ++ " got " ++ inEvent)
    )
}

class EventProducer (chPub, chStop, name="announcer") {
    evtCnt = 0
    method reportSummary() {
        console.info(name ++ " totally dispatched " ++ evtCnt ++ " event(s).")
    }
    method giveTiming (interval) {
        defer this.reportSummary()

        altog (
            for _ from chStop do pass,
            for currentTime from runtime.everyMillisN(interval) do (
                this.evtCnt += 1
                chPub <- "[Event-" ++ evtCnt ++ "@" ++ currentTime ++ "]"
            )
        )
    }
}

chPubSub = chan
let (mon1, mon2) = (
    EventMonitor(chPubSub, "Mon1"),
    EventMonitor(chPubSub, "Mon2"),
)
go mon1.ackEvents()
go mon2.ackEvents()

chStop = chan
prod = EventProducer (chPubSub, chStop)
go prod.giveTiming(1.5e3)

for _ from runtime.afterMillisN(60e3) do
    chStop.close()
```

We have `altog(*tasks)` (as shown above) and `concur(n,*tasks)` for
concurrency control. The implementation is trivial with Haskell's
`throwTo` construct for async cancellation.

> TODO should we implement `select` (as in Go) on multiple channel
> reads ? This may not be trivial when we simply implement channels
> with `MVar`s.

### Haskell style case-of, with Go style fallthrough

```haskell
let essay = case type(v) of (
   BoolType -> "to be or not to be, that's a problem"
   DecimalType -> (
      |v<2 -> "consume less, produce more"
      |v<10 -> "no more than " ++ v ++ " cups of coffee a day"
      true -> "every one get his/her lucky number"
   )
   StringType -> (quiz=v fallthrough)
   SymbolType -> (quiz='mistery attracts most people' fallthrough)
   ModuleType -> (quiz='I live in ' ++ v.__name__; fallthrough)
   "do you known, that " ++ quiz ++ " ?"
)
```

### Ternary operator

in C you do:

```C
onCnd ? oneThing : theOther
```

and in Python you do:

```python
onCnd and oneThing or theOther
```

well in Edh you do:

```haskell
onCnd &= oneThing |= theOther
```

### operator override / creation

You should be supprised that the following language constructs are all
implemented as overridable operators:

- assignment
  - (`=`), (`+=`), (`-=`), (`*=`), (`/=`)
- logical arithmetic
  - (`&&`), (`||`)
- ternary
  - (`&=`), (`|=`)
- list/dict comprehension
  - (`=<`)
- list/dict prepend/insert
  - (`=>`)
- channel read/write
  - (`<-`)
- case branch
  - (`->`)
- string concat
  - (`++`)

Meaning you can override them for the program scope under your control,
and you can even roll your own, arbitrary new operators with a precendence
you'd like with, as very well as in Haskell.

All operators in Edh are left associative, infix only, though. Well, except
a few hardcoded prefix operators:

- (`+`) prefix plus
- (`-`) prefix minus
- (`not`) prefix bool negation
- (`|`) guard
- the keyword `go` and `defer` are implemented as prefix operator

### indexing override

The indexing syntax is a special case (not an infix operator), but very
crucial for tasks e.g. porting Numpy to Edh. As Edh allows parenthesis
quoted operator symbol as attribute name, such a method can be written:

```haskell
class ndarray (*args, **kwargs) {
    method ([]) (ix) {
        ix can be scalar, slice or tuple of them ...
    }
}
```

With some additional tricks in the language, including parsing Python style
index keys, i.e. slice syntax with (:) and (,) to create tuple key within
the indexing bracket ([]) pair, then finally redirecting indexing against
an object to method ([]), it'll be much similar as implementing
`__getitem__(self, ix)` as in Python.

### lossless decimal for numbers

In JavaScript, even today, all numbers are `float64`, it puzzled me a while,
before I figured out this is the reason why we can not have `Int64Array`
besides `Int32Array`/`Int16Array`/`Float64Array`/`Float32Array` etc. that
the language simply unable to handle elements from an `Int64Array` if it's
ever provided.

Python gives you lossless integers by default, but `float64` for decimal:

```python
>>> 1.1 + 2.2
3.3000000000000003
>>>
```

Haskell forces you to choose a type for every number in your program,
normal options are the lossless `Integer` and precision-lossing `Double`
(which is `float64` per se):

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
λ> 1.1 + 2.2 :: Scientific
3.3
λ> x = 1.1 + 2.2 :: Scientific
λ> x / 0
*** Exception: Ratio has zero denominator
λ>
```

> Your compiled Haskell program will crash on division-by-zero if not
> handled specifically.

Then here's `Data.Lossless.Decimal` from package
[lossless-decimal](https://github.com/complyue/edh/tree/master/lossless-decimal)

```haskell
λ> x = 1.1 + 2.2 :: Decimal
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

All numbers in Edh are `Data.Lossless.Decimal` from
[lossless-decimal](https://github.com/complyue/edh/tree/master/lossless-decimal)
, by default and by all means:

> will treat numeric data inside `Repa` / `AccerlerateHS` arrays as foreign data,
> given we port `Numpy`/`Pandas` to Edh to manipulate them

```
...
```

### Reflection

...

## the name

Edh stands for **Event Distributing & Hosting**

Đ is a more stylish, single letter, symbolic name of Edh the language

## Acknowledgement

Edh's parser is built with
[Megaparsec](https://github.com/mrkkrp/megaparsec) (
[on hackage](https://hackage.haskell.org/package/megaparsec) ),
parser making with Megaparsec is so pleasant.

The development of Edh was started from cloning
[monkey-hs](https://github.com/utatti/monkey-hs)
by Hyunje Jun, but later re-written entirely.
