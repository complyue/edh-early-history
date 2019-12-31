
module Language.Edh.Batteries.Data where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | operator (:) pair constructor
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


-- | operator (?) attribute tempter, 
-- address an attribute off an object if possible, nil otherwise
attrTemptProc :: EdhProcedure
attrTemptProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit
  = do
    !pgs <- ask
    let !callerCtx   = edh'context pgs
        !callerScope = contextScope callerCtx
    case rhExpr of
      AttrExpr (DirectRef (NamedAttr attrName)) ->
        evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
          EdhObject obj -> contEdhSTM $ do
            em <- readTVar $ objEntity obj
            exitEdhSTM
              pgs
              exit
              ( that
              , callerScope
              , case Map.lookup (AttrByName attrName) em of
                Nothing  -> nil
                Just val -> val
              )
          _ -> exitEdhProc exit (that, callerScope, nil)
      _ -> throwEdh EvalError $ "Invalid attribute expression: " <> T.pack
        (show rhExpr)
attrTemptProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | the Symbol() constructor
symbolCtorProc :: EdhProcedure
symbolCtorProc !argsSender that _ !exit = do
  !pgs <- ask
  let !callerCtx@(Context _ _ _ (StmtSrc (srcPos, _))) = edh'context pgs
      !callerScope = contextScope callerCtx
  packEdhArgs that argsSender
    $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) -> contEdhSTM $ do
        posSyms <- sequence $ ctorSym <$> args
        kwSyms  <- sequence $ Map.map ctorSym kwargs
        if null kwargs
          then case posSyms of
            [] -> do
              sym <- ctorSym $ EdhString $ T.pack (sourcePosPretty srcPos)
              exitEdhSTM pgs exit (that, callerScope, sym)
            [sym] -> exitEdhSTM pgs exit (that, callerScope, sym)
            _     -> exitEdhSTM pgs exit (that, callerScope, EdhTuple posSyms)
          else exitEdhSTM
            pgs
            exit
            (that, callerScope, EdhArgsPack $ ArgsPack posSyms kwSyms)
 where
  ctorSym :: EdhValue -> STM EdhValue
  ctorSym = \case
    sym@(EdhSymbol _) -> return sym
    val               -> EdhSymbol <$> (mkSymbol $ T.unpack $ edhValueStr val)


-- | utility pkargs(*args,**kwargs,***packed) - arguments packer
pkargsProc :: EdhProcedure
pkargsProc !argsSender that _ !exit = packEdhArgs that argsSender exit


-- | operator (++) string coercing concatenation
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


-- | utility type(*args,**kwargs) - value type introspection
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


-- | utility dict(***pkargs, **kwargs,*args) - dict from arguments
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

