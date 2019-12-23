
module Language.Edh.Batteries where

import           Prelude
import           Debug.Trace

import           Control.Monad.Reader
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

  -- string/list concatenation
    , ("++", 5)
    ]

  consHP   <- mkHostProc ":" consProc
  assignHP <- mkHostProc "=" assignProc
  concatHP <- mkHostProc "++" concatProc
  typeHP   <- mkHostProc "type" typeProc
  dictHP   <- mkHostProc "dict" dictProc

  atomically $ installEdhAttrs
    rootEntity
    [
  -- operators
      (AttrByName ":", EdhHostProc consHP)
    , (AttrByName "=", EdhHostProc assignHP)
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


consProc :: EdhProcedure
consProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  -- make sure left hand and right hand values are evaluated in same tx
  local (\s -> s { edh'in'tx = True }) $ evalExpr lhExpr $ \(_, lhVal) ->
    evalExpr rhExpr
      $ \(_, rhVal) -> exitEdhProc exit (scope, EdhPair lhVal rhVal)
consProc !argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)


assignProc :: EdhProcedure
assignProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  !pgs <- ask
  let txq                             = edh'main'queue pgs
      !inTx                           = edh'in'tx pgs
      !callerCtx                      = edh'context pgs
      !callerScope@(Scope !ent !this) = contextScope callerCtx
      !thisEnt                        = objEntity this
      !pgs'tx                         = pgs { edh'in'tx = True }
      finishAssign :: Entity -> AttrKey -> (Scope, EdhValue) -> EdhProg (STM ())
      finishAssign tgtEnt key (_, !val) = return $ do
        let result = (callerScope, val) -- assigment result as if come from the calling scope
        modifyTVar' tgtEnt $ \em -> Map.insert key val em
        if inTx
          then join $ runReaderT (exit result) pgs
          else writeTQueue txq (result, exit)
  case lhExpr of
    AttrExpr !addr -> case addr of
      DirectRef !addr' -> return $ resolveAddr callerScope addr' >>= \key ->
        join $ runReaderT (evalExpr rhExpr (finishAssign ent key)) pgs'tx
      IndirectRef !tgtExpr !addr' -> case tgtExpr of
        AttrExpr ThisRef ->
          return
            $   resolveAddr callerScope addr'
            >>= \key -> join $ runReaderT
                  (evalExpr rhExpr (finishAssign thisEnt key))
                  pgs'tx
        AttrExpr SupersRef ->
          throwEdh $ EvalError "Can not assign an attribute to supers"
        _ -> local (const pgs'tx) $ evalExpr tgtExpr $ \(_, !tgtVal) ->
          case tgtVal of
            EdhObject (Object !tgtEnt _ _) ->
              return
                $   resolveAddr callerScope addr'
                >>= \key -> join $ runReaderT
                      (evalExpr rhExpr (finishAssign tgtEnt key))
                      pgs'tx
            _ -> throwEdh $ EvalError $ "Invalid assignment target: " <> T.pack
              (show tgtVal)
      ThisRef   -> throwEdh $ EvalError "Can not assign to this"
      SupersRef -> throwEdh $ EvalError "Can not assign to supers"
    x ->
      throwEdh
        $  EvalError
        $  "Invalid left hand value for assignment: "
        <> T.pack (show x)
assignProc !argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)


concatProc :: EdhProcedure
concatProc argsSender procScope exit = undefined


typeProc :: EdhProcedure
typeProc argsSender procScope exit = undefined
  -- packEdhArgs  argsSender $ \(ArgsPack !args !kwargs) ->

  --   let !argsType = map edhTypeOf args
  --       kwargsType = -- note: leave this lazy, not always needed
  --           Map.fromList $ (<$> Map.toAscList kwargs) $ \(attrName, val) ->
  --             (ItemByStr attrName, edhTypeOf val)
  --   in  if null kwargs
  --         then case argsType of
  --           [t] -> exit (procScope, t)
  --           _   -> exit (procScope, EdhTuple argsType)
  --         else do
  --           d <- liftIO $ newTVarIO $ Map.union kwargsType $ Map.fromAscList
  --             [ (ItemByNum (fromIntegral i), t)
  --             | (i, t) <- zip [(0 :: Int) ..] argsType
  --             ]
  --           exit (procScope, EdhDict (Dict d))


dictProc :: EdhProcedure
dictProc argsSender procScope exit = undefined
  -- packEdhArgs  argsSender $ \(ArgsPack !args !kwargs) ->
  --   let kwDict =
  --           Map.fromAscList $ (<$> Map.toAscList kwargs) $ \(attrName, val) ->
  --             (ItemByStr attrName, val)
  --   in  do
  --         d <- liftIO $ newTVarIO $ Map.union kwDict $ Map.fromAscList
  --           [ (ItemByNum (fromIntegral i), t)
  --           | (i, t) <- zip [(0 :: Int) ..] args
  --           ]
  --         exit (procScope, EdhDict (Dict d))

