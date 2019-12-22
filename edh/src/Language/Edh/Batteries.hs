
module Language.Edh.Batteries where

import           Prelude
import           Debug.Trace

import           Control.Concurrent.STM
import           Control.Monad.IO.Class

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Data.IORef
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           System.Mem.Weak
import           System.IO.Unsafe

import           Data.Lossless.Decimal         as D

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime
import           Language.Edh.Event


installEdhBatteries :: MonadIO m => EdhWorld -> m ()
installEdhBatteries world = liftIO $ do
  let rootEntity = (objEntity . worldRoot) world

  -- TODO survey for best practices & advices on precedences here
  --      once it's declared, can not be changed in the world.

  declareEdhOperators
    world
    "<batteries>"
    [ -- format: (symbol, precedence)

  -- dict key:value seperator, is dummy in operator regards
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

  -- string/list concatenation
    , ("++", 5)
    ]

  assignHP <- mkHostProc "=" assignProc
  concatHP <- mkHostProc "++" concatProc
  typeHP   <- mkHostProc "type" typeProc
  dictHP   <- mkHostProc "dict" dictProc

  atomically $ putEdhAttrs
    rootEntity
    [
  -- operators
      (AttrByName "=", EdhHostProc assignHP)
    , ( AttrByName "++"
      , EdhHostProc concatHP
      )

  -- introspection
    , ( AttrByName "type"
      , EdhHostProc typeHP
      )

  -- utility
    , ( AttrByName "dict"
      , EdhHostProc dictHP
      )

  -- math constants
  -- todo figure out proper ways to make these really **constant**,
  --      i.e. not rebindable to other values
    , ( AttrByName "pi"
      , EdhDecimal $ Decimal 1 (-40) 31415926535897932384626433832795028841971
      )
    ]

  return ()


assignProc :: EdhProcedure
assignProc callerCtx (PackSender [SendPosArg lhExpr, SendPosArg rhExpr]) _ exit
  = case lhExpr of
    AttrExpr !addr -> case addr of
      DirectRef !addr' -> resolveAddr callerScope addr' $ \key ->
        withEdhTx $ eval' rhExpr $ \result@(_, !val) ->
          edhWriteAttr ent key $ \wtr -> do
            wtr val
            exit result
      IndirectRef !expr !addr' -> case expr of
        AttrExpr (ThisRef) -> resolveAddr callerScope addr' $ \key ->
          withEdhTx $ eval' rhExpr $ \result@(_, !val) ->
            edhWriteAttr thisEnt key $ \wtr -> do
              wtr val
              exit result
        AttrExpr (SupersRef) ->
          throwEdh $ EvalError "Can not assign an attribute to supers"
        _ -> resolveAddr callerScope addr' $ \key ->
          withEdhTx $ eval' expr $ \(_, !tgt) -> case tgt of
            EdhObject (Object !tgtEnt _ _) ->
              eval' rhExpr $ \result@(_, !val) ->
                edhWriteAttr tgtEnt key $ \wtr -> do
                  wtr val
                  exit result
            tgtVal ->
              throwEdh $ EvalError $ "Invalid assignment target: " <> T.pack
                (show tgtVal)
      ThisRef   -> throwEdh $ EvalError "Can not assign to this"
      SupersRef -> throwEdh $ EvalError "Can not assign to supers"
    x ->
      throwEdh $ EvalError $ "Invalid left hand expr for assignment: " <> T.pack
        (show x)
 where
  thisEnt                      = objEntity this
  callerScope@(Scope ent this) = contextScope callerCtx
  eval'                        = evalExpr callerCtx
assignProc _ argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)


concatProc :: EdhProcedure
concatProc callerCtx argsSender procScope exit = undefined


typeProc :: EdhProcedure
typeProc callerCtx argsSender procScope exit =
  packEdhArgs callerCtx argsSender $ \(ArgsPack !args !kwargs) ->

    let !argsType = map edhTypeOf args
        kwargsType = -- note: leave this lazy, not always needed
            Map.fromList $ (<$> Map.toAscList kwargs) $ \(attrName, val) ->
              (ItemByStr attrName, edhTypeOf val)
    in  if null kwargs
          then case argsType of
            [t] -> exit (procScope, t)
            _   -> exit (procScope, EdhTuple argsType)
          else do
            d <- liftIO $ newTVarIO $ Map.union kwargsType $ Map.fromAscList
              [ (ItemByNum (fromIntegral i), t)
              | (i, t) <- zip [(0 :: Int) ..] argsType
              ]
            exit (procScope, EdhDict (Dict d))


dictProc :: EdhProcedure
dictProc callerCtx argsSender procScope exit =
  packEdhArgs callerCtx argsSender $ \(ArgsPack !args !kwargs) ->
    let kwDict =
            Map.fromAscList $ (<$> Map.toAscList kwargs) $ \(attrName, val) ->
              (ItemByStr attrName, val)
    in  do
          d <- liftIO $ newTVarIO $ Map.union kwDict $ Map.fromAscList
            [ (ItemByNum (fromIntegral i), t)
            | (i, t) <- zip [(0 :: Int) ..] args
            ]
          exit (procScope, EdhDict (Dict d))

