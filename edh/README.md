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
import (doA, doB, **goodies) from './lib'

// argument retargetting
method setName (name=this.name) pass
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

and in Edh you do:

```haskell
onCnd &= oneThing |= theOther
```

### list/dict comprehension

```haskell
[] =< for x from range(100) do x*x
{} =< for x from range(100) do (x, x*x)
```

### goroutine and channels

```haskell
class EventMonitor (chSub, name="observer") {
    method ackEvents () (
        for inEvent from chSub do
            console.info(name ++ " got " ++ inEvent)
    )
}

class EventProducer (chPub, name="announcer") {
    evtCnt = 0
    method reportSummary() {
        console.info(name ++ " totally dispatched " ++ evtCnt ++ " event(s).")
    }
    method giveTiming (interval) {
        defer this.reportSummary()
        for currentTime from Runtime.everyMillisN(interval) do (
            this.evtCnt += 1
            chPub <- "[Event-" ++ evtCnt ++ "@" ++ currentTime ++ "]"
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

prod = EventProducer (chPubSub)
go prod.giveTiming(1.5e3)

for _ from Runtime.afterMillisN(60e3) do
    Runtime.endTheWorld()
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

### Reflection

...

### operator override

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
and you can define arbitrary new operators with the precendence you'd
like.

## Acknowledgement

Edh's parser is built with
[Megaparsec](https://github.com/mrkkrp/megaparsec) (
[on hackage](https://hackage.haskell.org/package/megaparsec) ),
parser making with Megaparsec is so pleasant.

The development of Edh was started from cloning
[monkey-hs](https://github.com/utatti/monkey-hs)
by Hyunje Jun, but later re-written entirely.
