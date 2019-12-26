
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
typeProc !argsSender !that !procScope !exit =
  packEdhArgs that argsSender
    $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) ->
        let !argsType = edhTypeOf <$> args
        in  if null kwargs
              then case argsType of
                [t] -> exitEdhProc exit (that, procScope, t)
                _   -> exitEdhProc exit (that, procScope, EdhTuple argsType)
              else exitEdhProc
                exit
                ( that
                , procScope
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


-- | utility supers(*args,**kwargs)
supersProc :: EdhProcedure
supersProc !argsSender that _thisHome !exit = do
  !pgs <- ask
  let !callerCtx   = edh'context pgs
      !callerScope = contextScope callerCtx
      !argCnt      = case argsSender of
        SingleSender _       -> 1
        PackSender   senders -> length senders
  if argCnt < 1
    then contEdhSTM $ do
      supers <- map EdhObject <$> (readTVar $ objSupers that)
      exitEdhSTM pgs exit (that, callerScope, EdhTuple supers)
    else
      packEdhArgs that argsSender
        $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) -> if null kwargs
            then case args of
              [v] -> contEdhSTM $ do
                supers <- supersOf v
                exitEdhSTM pgs exit (that, callerScope, supers)
              _ -> contEdhSTM $ do
                supersList <- sequence $ supersOf <$> args
                exitEdhSTM pgs exit (that, callerScope, EdhTuple supersList)
            else contEdhSTM $ do
              supersDict <- sequence $ Map.map supersOf kwargs
              let kwDict =
                    Map.fromAscList
                      $ (<$> Map.toAscList supersDict)
                      $ \(attrName, val) -> (ItemByStr attrName, val)
              supersList <- sequence $ supersOf <$> args
              d          <- newTVar $ Map.union kwDict $ Map.fromAscList
                [ (ItemByNum (fromIntegral i), t)
                | (i, t) <- zip [(0 :: Int) ..] supersList
                ]
              exitEdhSTM pgs exit (that, callerScope, EdhDict (Dict d))

 where
  supersOf :: EdhValue -> STM EdhValue
  supersOf v = case v of
    EdhObject o ->
      map EdhObject <$> readTVar (objSupers o) >>= return . EdhTuple
    _ -> return nil
