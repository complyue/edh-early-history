
module Language.Edh.Batteries.Data where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | operator (:)
consProc :: EdhProcedure
consProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit =
  do
    !pgs <- ask
    let !callerCtx   = edh'context pgs
        !callerScope = contextScope callerCtx
    -- make sure left hand and right hand values are evaluated in same tx
    local (\s -> s { edh'in'tx = True })
      $ evalExpr that lhExpr
      $ \(_, _, lhVal) -> evalExpr that rhExpr $ \(_, _, rhVal) ->
          exitEdhProc exit (that, callerScope, EdhPair lhVal rhVal)
consProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | utility pack(*args,**kwargs)
packProc :: EdhProcedure
packProc !argsSender that _ !exit = packEdhArgs that argsSender exit


-- | operator (++)
concatProc :: EdhProcedure
concatProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit =
  do
    !pgs <- ask
    let !callerCtx   = edh'context pgs
        !callerScope = contextScope callerCtx
    evalExpr that lhExpr $ \(_, _, lhVal) ->
      evalExpr that rhExpr $ \(_, _, rhVal) -> exitEdhProc
        exit
        (that, callerScope, EdhString $ edhValueStr lhVal <> edhValueStr rhVal)
concatProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | utility type(*args,**kwargs)
typeProc :: EdhProcedure
typeProc !argsSender !that _ !exit = do
  !pgs <- ask
  let !callerCtx   = edh'context pgs
      !callerScope = contextScope callerCtx
  packEdhArgs that argsSender
    $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) ->
        let !argsType = edhTypeOf <$> args
        in  if null kwargs
              then case argsType of
                [t] -> exitEdhProc exit (that, callerScope, t)
                _   -> exitEdhProc exit (that, callerScope, EdhTuple argsType)
              else exitEdhProc
                exit
                ( that
                , callerScope
                , EdhArgsPack $ ArgsPack argsType $ Map.map edhTypeOf kwargs
                )


-- | utility dict(**kwargs,*args)
dictProc :: EdhProcedure
dictProc !argsSender that _ !exit = do
  !pgs <- ask
  let !callerCtx   = edh'context pgs
      !callerScope = contextScope callerCtx
  packEdhArgs that argsSender
    $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) ->
        let !kwDict =
                Map.fromAscList
                  $ (<$> Map.toAscList kwargs)
                  $ \(attrName, val) -> (ItemByStr attrName, val)
        in  contEdhSTM $ do
              d <- newTVar $ Map.union kwDict $ Map.fromAscList
                [ (ItemByNum (fromIntegral i), t)
                | (i, t) <- zip [(0 :: Int) ..] args
                ]
              exitEdhSTM pgs exit (that, callerScope, EdhDict (Dict d))

