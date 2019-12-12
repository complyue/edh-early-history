# Đ - (WIP) a modern Object layer spliced with Haskell

Missing Python/JavaScript or even Go in shifting to
Haskell? Now you can have Edh, with many goodies from
those languages but running embedded in Haskell.

## Teasers

### Python style argument passing, and more

```javascript
// the Pythonic way of argument receiving
method f (x, y=10, *ns, **kwargs) {
    // ...
}
// the Pythonic way of argument sending
f (3, 7, 21, *[9, 11], name='doer', **{'msg': "you've got it", 'keynum': 2})

// wild argument receiver
method g * {
    return "I guess you passed me a name '" ++ name ++ "', or I'll crash!"
}
// but this is not really a good idea for api signatures
g (name="Yesman")
// while it's acceptable for imports
import * from './lib.edh'
// compared to the more explicit forms
import (doA, doB, **newGoodiesToKnow) from './lib'

// argument retargetting
method setName (name=this.name) pass
```

### Go embedding style multiple "inheritance"

This is not real object-oriented inheritance though, but you'll go as far and
well as Go [Type Embedding](https://go101.org/article/type-embedding.html)
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

### list/dict comprehension

```haskell
[] =< for x from range(100) do x*x
{} =< for x from range(100) do ("square of " ++ x, x*x)
```

### goroutine, defer, and channels

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

        select (
            for _ from chStop do pass,
            for currentTime from runtime.everyMillisN(interval) do (
                this.evtCnt += 1
                chPub <- "[Event-" ++ evtCnt ++ "@" ++ currentTime ++ "]"
            )
        )
    }
}

chPubSub = chan
(mon1, mon2) = (
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
  - (=), (+=), (-=), (\*=), (/=)
- logical arithmetic
  - (&&), (||)
- ternary
  - (&=), (|=)
- list/dict comprehension
  - (=<)
- channel read/write
  - (<-)
- case branch
  - (->)
- string concat
  - (++)

Meaning you can override them for the program scope under your control,
and you can even roll your own, arbitrary new operators with a precendence
you'd like with, as very well as in Haskell.

All operators in Edh are left associative, infix only, though. Well, except
a few hardcoded prefix operators:

- (+) prefix plus
- (-) prefix minus
- (not) prefix bool negation
- (|) gard
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
`__getitem__(self, ix)` in Python.

### lossless decimal for numbers

Python gives you lossless integers by default, but `float64` for decimal:

```python
>>> 1.1 + 2.2
3.3000000000000003
>>>
```

Haskell does pretty the same:

```haskell
λ> 1.1 + 2.2
3.3000000000000003
λ>
```

While Edh uses
[lossless-decimal](https://github.com/complyue/edh/tree/master/lossless-decimal)
by default and by all means:

> (TODO this is the usage of the type in Haskell, add Đ repl session demo
> once the (+) operator works)

```haskell
λ> 1.1 + 2.2 :: Decimal
3.3
λ>
```

### Reflection

...

## Acknowledgement

Edh's parser is built with
[Megaparsec](https://github.com/mrkkrp/megaparsec) (
[on hackage](https://hackage.haskell.org/package/megaparsec) ),
parser making with Megaparsec is so pleasant.

The development of Edh was started from cloning
[monkey-hs](https://github.com/utatti/monkey-hs)
by Hyunje Jun, but later re-written entirely.
