{-# LANGUAGE TupleSections #-}

module Language.Edh.Batteries where

import           Prelude

import           Control.Monad.Reader
import           Control.Concurrent.STM

import           Data.Lossless.Decimal         as D

import           Language.Edh.Runtime

import           Language.Edh.Batteries.Math
import           Language.Edh.Batteries.Assign
import           Language.Edh.Batteries.Utils


installEdhBatteries :: MonadIO m => EdhWorld -> m ()
installEdhBatteries world = liftIO $ do
  let rootEntity = (objEntity . worldRoot) world

  -- TODO survey for best practices & advices on precedences here
  --      once it's declared, can not be changed in the world.

  declareEdhOperators
    world
    "<batteries>"
    [ -- format: (symbol, precedence)

  -- the cons operator, creates pairs in Edh
      ( ":"
      , 1
      ) -- ^ why brittany insists on formatting it like this ?.?

  -- assignments
    , ("=" , 1)
    , ("+=", 1)
    , ("-=", 1)
    , ("/=", 1)
    , ( "*="
      , 1
      ) -- ^ why brittany insists on formatting it like this ?.?

  -- arithmetic
    , ("+", 6)
    , ("-", 6)
    , ("*", 7)
    , ("/", 7)
    , ( "**"
      , 8
      )
  -- comparations
    , ( "~="
      , 4
      ) -- value-wise equality test
    , ( "=="
      , 4
      ) -- identity-wise equality test
    , ( "!="
      , 4
      ) -- C style here, as (/=) is used for inplace division
    , (">" , 4)
    , (">=", 4)
    , ("<" , 4)
    , ( "<="
      , 4
      )
  -- logical arithmetic
    , ("&&", 3)
    , ( "||"
      , 3
      ) -- ^ why brittany insists on formatting it like this ?.?

  -- emulate the ternary operator in C:
  --       onCnd ? oneThing : theOther
  --  * Python
  --       onCnd and oneThing or theOther
  --  * Edh
  --       onCnd &= oneThing |= theOther
    , ("&=", 2)
    , ( "|="
      , 2
      ) -- ^ why brittany insists on formatting it like this ?.?

  -- comprehension
  --  * list comprehension:
  --     [] =< for x from range(100) do x*x
  --  * dict comprehension:
  --     {} =< for x from range(100) do (x, x*x)
  --  * tuple comprehension:
  --     (,) =< for x from range(100) do x*x
    , ( "=<"
      , 3
      ) -- ^ why brittany insists on formatting it like this ?.?
  -- prepand to list
  --     l = [3,7,5]
  --     [2,9] => l
    , ( "=>"
      , 3
      )

  -- publish to sink
  --     evsPub <- outEvent
    , ( "<-"
      , 5
      )

  -- case branch (| guard is a hardcoded prefix operator)
  --    let essay = case type(v) of (
  --       BoolType -> "to be or not to be, that's a problem"
  --       DecimalType -> (
  --          |v<2 -> "consume less, produce more"
  --          |v<10 -> "no more than " ++ v ++ " cups of coffee a day"
  --          true -> "every one get his/her lucky number"
  --       )
  --       StringType -> (quiz=v fallthrough)
  --       SymbolType -> (quiz='mistery attracts most people' fallthrough)
  --       ModuleType -> (quiz='I live in ' ++ v.__name__; fallthrough)
  --       "do you known, that " ++ quiz ++ " ?"
  --    )
    , ( "->"
      , 1
      )

  -- string coercing concatenation
    , ("++", 5)
    ]


  operators <- mapM
    (\(sym, hp) -> (AttrByName sym, ) . EdhHostProc <$> mkHostProc sym hp)
    [ (":"   , consProc)
    , ("+"   , addProc)
    , ("-"   , subsProc)
    , ("*"   , mulProc)
    , ("/"   , divProc)
    , ("="   , assignProc)
    , ("pack", packProc)
    , ("++"  , concatProc)
    , ("type", typeProc)
    , ("dict", dictProc)
    ]

  atomically
    $  installEdhAttrs rootEntity
    $  operators
    ++ [
  -- math constants
  -- todo figure out proper ways to make these really **constant**,
  --      i.e. not rebindable to other values
         ( AttrByName "pi"
         , EdhDecimal
           $ Decimal 1 (-40) 31415926535897932384626433832795028841971
         )
       ]

