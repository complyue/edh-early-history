
module Language.Edh.Batteries.Utils where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


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


packProc :: EdhProcedure
packProc !argsSender _ !exit = packEdhArgs argsSender exit


concatProc :: EdhProcedure
concatProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr lhExpr $ \(_, lhVal) -> evalExpr rhExpr $ \(_, rhVal) -> exitEdhProc
    exit
    (scope, EdhString $ edhValueStr lhVal <> edhValueStr rhVal)
concatProc !argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)


typeProc :: EdhProcedure
typeProc !argsSender !procScope !exit =
  packEdhArgs argsSender $ \(_, EdhArgsPack (ArgsPack !args !kwargs)) ->
    let !argsType = edhTypeOf <$> args
    in  if null kwargs
          then case argsType of
            [t] -> exitEdhProc exit (procScope, t)
            _   -> exitEdhProc exit (procScope, EdhTuple argsType)
          else exitEdhProc
            exit
            ( procScope
            , EdhArgsPack $ ArgsPack argsType $ Map.map edhTypeOf kwargs
            )


dictProc :: EdhProcedure
dictProc !argsSender _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  packEdhArgs argsSender $ \(_, EdhArgsPack (ArgsPack !args !kwargs)) ->
    let !kwDict =
            Map.fromAscList $ (<$> Map.toAscList kwargs) $ \(attrName, val) ->
              (ItemByStr attrName, val)
    in  return $ do
          d <- newTVar $ Map.union kwDict $ Map.fromAscList
            [ (ItemByNum (fromIntegral i), t)
            | (i, t) <- zip [(0 :: Int) ..] args
            ]
          join $ runReaderT (exitEdhProc exit (scope, EdhDict (Dict d))) pgs

