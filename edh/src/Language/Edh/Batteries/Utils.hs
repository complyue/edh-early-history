
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
consProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ _ !exit = do
  !pgs <- ask
  let !callerCtx                     = edh'context pgs
      !callerScope@(Scope _ !this _) = contextScope callerCtx
  -- make sure left hand and right hand values are evaluated in same tx
  local (\s -> s { edh'in'tx = True }) $ evalExpr lhExpr $ \(_, _, lhVal) ->
    evalExpr rhExpr $ \(_, _, rhVal) ->
      exitEdhProc exit (this, callerScope, EdhPair lhVal rhVal)
consProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | utility pack(*args,**kwargs)
packProc :: EdhProcedure
packProc !argsSender _ _ !exit = packEdhArgs argsSender exit


-- | operator (++)
concatProc :: EdhProcedure
concatProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ _ !exit = do
  !pgs <- ask
  let !callerCtx                     = edh'context pgs
      !callerScope@(Scope _ !this _) = contextScope callerCtx
  evalExpr lhExpr $ \(_, _, lhVal) -> evalExpr rhExpr $ \(_, _, rhVal) ->
    exitEdhProc
      exit
      (this, callerScope, EdhString $ edhValueStr lhVal <> edhValueStr rhVal)
concatProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | utility type(*args,**kwargs)
typeProc :: EdhProcedure
typeProc !argsSender !that !procScope !exit =
  packEdhArgs argsSender $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) ->
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
dictProc !argsSender _ _ !exit = do
  !pgs <- ask
  let !callerCtx                     = edh'context pgs
      !callerScope@(Scope _ !this _) = contextScope callerCtx
  packEdhArgs argsSender $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) ->
    let !kwDict =
            Map.fromAscList $ (<$> Map.toAscList kwargs) $ \(attrName, val) ->
              (ItemByStr attrName, val)
    in
      return $ do
        d <- newTVar $ Map.union kwDict $ Map.fromAscList
          [ (ItemByNum (fromIntegral i), t)
          | (i, t) <- zip [(0 :: Int) ..] args
          ]
        join $ runReaderT
          (exitEdhProc exit (this, callerScope, EdhDict (Dict d)))
          pgs


-- | utility supers(*args,**kwargs)
supersProc :: EdhProcedure
supersProc !argsSender that _thisHome !exit = do
  !pgs <- ask
  let !callerCtx                     = edh'context pgs
      !callerScope@(Scope _ !this _) = contextScope callerCtx
      !argCnt                        = case argsSender of
        SingleSender _       -> 1
        PackSender   senders -> length senders
  if argCnt < 1
    then exitEdhProc
      exit
      (that, callerScope, EdhTuple (EdhObject <$> objSupers that))
    else
      packEdhArgs argsSender $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) ->
        if null kwargs
          then case args of
            [v] -> exitEdhProc exit (this, callerScope, supersOf v)
            _ ->
              exitEdhProc exit (this, callerScope, EdhTuple $ supersOf <$> args)
          else
            let !kwDict =
                  Map.fromAscList
                    $ (<$> Map.toAscList (Map.map supersOf kwargs))
                    $ \(attrName, val) -> (ItemByStr attrName, val)
            in  return $ do
                  d <- newTVar $ Map.union kwDict $ Map.fromAscList
                    [ (ItemByNum (fromIntegral i), t)
                    | (i, t) <- zip [(0 :: Int) ..] (supersOf <$> args)
                    ]
                  join $ runReaderT
                    (exitEdhProc exit (that, callerScope, EdhDict (Dict d)))
                    pgs
 where
  supersOf :: EdhValue -> EdhValue
  supersOf v = case v of
    EdhObject o -> EdhTuple $ EdhObject <$> (objSupers o)
    _           -> nil
