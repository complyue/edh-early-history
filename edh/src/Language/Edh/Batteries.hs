{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Batteries where

import           Prelude

import           System.Environment
import           Text.Read

import           Control.Exception
import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Map.Strict               as Map

import           Data.Lossless.Decimal         as D

import           Language.Edh.Runtime

import           Language.Edh.Batteries.Data
import           Language.Edh.Batteries.Math
import           Language.Edh.Batteries.Assign
import           Language.Edh.Batteries.Reflect
import           Language.Edh.Batteries.Ctrl
import           Language.Edh.Batteries.Runtime


installEdhBatteries :: MonadIO m => EdhWorld -> m ()
installEdhBatteries world = liftIO $ do
  envLogLevel <- lookupEnv "EDH_LOG_LEVEL"
  atomically $ do

     -- TODO survey for best practices & advices on precedences here
     --      once it's declared, can not be changed in the world.

    declareEdhOperators
      world
      "<batteries>"
      [ -- format: (symbol, precedence)

      -- | the cons operator, creates pairs in Edh
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
        ) -- ^ deep-value-wise equality test
      , ( "=="
        , 4
        ) -- ^ identity-wise equality test
      , ( "!="
        , 4
        ) -- ^ inversed identity-wise equality test
          -- C style here, as (/=) is used for inplace division
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
        --       onCnd &> oneThing |> theOther
      , ("&>", 2)
      , ( "|>"
        , 2
        ) -- ^ why brittany insists on formatting it like this ?.?

        -- | comprehension
        --  * list comprehension:
        --     [] =< for x from range(100) do x*x
        --  * dict comprehension:
        --     {} =< for x from range(100) do (x, x*x)
        --  * tuple comprehension:
        --     (,) =< for x from range(100) do x*x
      , ( "=<"
        , 3
        ) -- ^ why brittany insists on formatting it like this ?.?
        -- | prepand to list
        --     l = [3,7,5]
        --     [2,9] => l
      , ( "=>"
        , 3
        )

        -- | publish to sink
        --     evsPub <- outEvent
      , ( "<-"
        , 5
        )

        -- | case branch (| guard is a hardcoded prefix operator)
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

        -- | string coercing concatenation
      , ( "++"
        , 5
        )

        -- | logging
      , ("<|", 1)
      ]


    !rootOperators <- mapM
      (\(sym, hp) -> (AttrByName sym, ) <$> mkHostOper world sym hp)
      [ (":" , consProc)
      , ("++", concatProc)
      , ("+" , addProc)
      , ("-" , subsProc)
      , ("*" , mulProc)
      , ("/" , divProc)
      , ("**", powProc)
      , ("&&", logicalAndProc)
      , ("||", logicalOrProc)
      , ("~=", valEqProc)
      , ("==", idEqProc)
      , (">" , isGtProc)
      , (">=", isGeProc)
      , ("<" , isLtProc)
      , ("<=", isLeProc)
      , ("=" , assignProc)
      , ("->", branchProc)
      , ("<|", loggingProc)
      ]

    !rootProcedures <- mapM
      (\(nm, hp) -> (AttrByName nm, ) <$> mkHostProc nm hp)
      [ ("pkargs"     , pkargsProc)
      , ("dict"       , dictProc)
      , ("type"       , typeProc)
      , ("constructor", ctorProc)
      , ("supers"     , supersProc)
      , ("scope"      , scopeObtainProc)
      , ("makeOp"     , makeOpProc)
      , ("makeExpr"   , makeExprProc)
      ]

    !rtEntity <- newTVar $ Map.fromList
      [ (AttrByName "debug", EdhDecimal 10)
      , (AttrByName "info" , EdhDecimal 20)
      , (AttrByName "warn" , EdhDecimal 30)
      , (AttrByName "error", EdhDecimal 40)
      , (AttrByName "fatal", EdhDecimal 50)
      ]
    !rtSupers <- newTVar []
    let !runtime = Object { objEntity = rtEntity
                          , objClass  = moduleClass world
                          , objSupers = rtSupers
                          }

    installEdhAttrs rootEntity
      $  rootOperators
      ++ rootProcedures
      ++ [
          -- runtime module 
           ( AttrByName "runtime"
           , EdhObject runtime
           )

          -- math constants
          -- todo figure out proper ways to make these really **constant**,
          --      i.e. not rebindable to other values
         , ( AttrByName "pi"
           , EdhDecimal
             $ Decimal 1 (-40) 31415926535897932384626433832795028841971
           )
         ]


    !scopeMethods <- mapM
      (\(sym, hp) -> (AttrByName sym, ) <$> mkHostProc sym hp)
      [ ("eval"     , scopeEvalProc)
      , ("attrs"    , scopeAttrsProc)
      , ("traceback", scopeTraceBackProc)
      , ("stack"    , scopeStackProc)
      ]

    installEdhAttrs scopeManiMethods scopeMethods

    case envLogLevel of
      Nothing      -> return ()
      Just "DEBUG" -> setLogLevel 10
      Just "INFO"  -> setLogLevel 20
      Just "WARN"  -> setLogLevel 30
      Just "ERROR" -> setLogLevel 40
      Just "FATAL" -> setLogLevel 50
      Just ns      -> case readEither ns of
        Left  _           -> return ()
        Right (ln :: Int) -> setLogLevel ln


 where
  !rootEntity = objEntity $ worldRoot world
  scopeManiMethods :: Entity
  !scopeManiMethods = objEntity $ scopeSuper world
  !wrt              = worldRuntime world
  setLogLevel :: LogLevel -> STM ()
  setLogLevel l = do
    rt <- takeTMVar wrt
    catchSTM (putTMVar wrt rt { runtimeLogLevel = l })
      $ \(e :: SomeException) -> tryPutTMVar wrt rt >> throwSTM e
