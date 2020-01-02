
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
consProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) !that _ !exit =
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
attrTemptProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) !that _ !exit
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
symbolCtorProc !argsSender !that _ !exit = do
  !pgs <- ask
  let !callerCtx@(Context _ _ _ _ (StmtSrc (srcPos, _))) = edh'context pgs
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
pkargsProc !argsSender !that _ !exit = packEdhArgs that argsSender exit


-- | operator (++) string coercing concatenation
concatProc :: EdhProcedure
concatProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) !that _ !exit
  = do
    !pgs <- ask
    let !callerCtx   = edh'context pgs
        !callerScope = contextScope callerCtx
    evalExpr that lhExpr $ \(_, _, lhVal) ->
      evalExpr that rhExpr $ \(_, _, rhVal) -> exitEdhProc
        exit
        (that, callerScope, EdhString $ edhValueStr lhVal <> edhValueStr rhVal)
concatProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | utility null(*args,**kwargs) - null test
isNullProc :: EdhProcedure
isNullProc !argsSender !that _ !exit = do
  !pgs <- ask
  let !callerCtx   = edh'context pgs
      !callerScope = contextScope callerCtx
  packEdhArgs that argsSender
    $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) -> if null kwargs
        then case args of
          [v] -> contEdhSTM $ do
            isNull <- EdhBool <$> edhValueNull v
            exitEdhSTM pgs exit (that, callerScope, isNull)
          _ -> contEdhSTM $ do
            argsNulls <- sequence $ ((EdhBool <$>) . edhValueNull) <$> args
            exitEdhSTM pgs exit (that, callerScope, EdhTuple argsNulls)
        else contEdhSTM $ do
          argsNulls   <- sequence $ ((EdhBool <$>) . edhValueNull) <$> args
          kwargsNulls <- sequence
            $ Map.map ((EdhBool <$>) . edhValueNull) kwargs
          exitEdhSTM
            pgs
            exit
            (that, callerScope, EdhArgsPack $ ArgsPack argsNulls kwargsNulls)


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
dictProc !argsSender !that _ !exit = do
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


-- | operator (=<) comprehension
--  * list comprehension:
--     [] =< for x from range(10) do x*x
--  * dict comprehension:
--     {} =< for x from range(10) do (x, x*x)
--  * tuple comprehension:
--     (,) =< for x from range(10) do x*x
--  * list append
--      [] =< (...) / [...] / {...}
--  * dict append
--      {} =< (...) / [...] / {...}
--  * tuple append
--      (,) =< (...) / [...] / {...}
cprhProc :: EdhProcedure
cprhProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) !that _ !exit =
  do
    !pgs <- ask
    let
      !callerCtx   = edh'context pgs
      !callerScope = contextScope callerCtx
      pvlToDict :: [EdhValue] -> STM DictStore
      pvlToDict ps = Map.fromList <$> sequence (val2ItemPair <$> ps)
      val2ItemPair :: EdhValue -> STM (ItemKey, EdhValue)
      val2ItemPair (EdhPair (EdhType    t) v) = return (ItemByType t, v)
      val2ItemPair (EdhPair (EdhString  s) v) = return (ItemByStr s, v)
      val2ItemPair (EdhPair (EdhSymbol  s) v) = return (ItemBySym s, v)
      val2ItemPair (EdhPair (EdhDecimal n) v) = return (ItemByNum n, v)
      val2ItemPair (EdhPair (EdhBool    b) v) = return (ItemByBool b, v)
      val2ItemPair (EdhPair k _v) =
        throwEdhFromSTM pgs EvalError $ "Invalid key for dict: " <> T.pack
          (show k)
      val2ItemPair (EdhTuple [EdhType    t, v]) = return (ItemByType t, v)
      val2ItemPair (EdhTuple [EdhString  s, v]) = return (ItemByStr s, v)
      val2ItemPair (EdhTuple [EdhSymbol  s, v]) = return (ItemBySym s, v)
      val2ItemPair (EdhTuple [EdhDecimal n, v]) = return (ItemByNum n, v)
      val2ItemPair (EdhTuple [EdhBool    b, v]) = return (ItemByBool b, v)
      val2ItemPair (EdhTuple [k, _v]) =
        throwEdhFromSTM pgs EvalError $ "Invalid key for dict: " <> T.pack
          (show k)
      val2ItemPair val =
        throwEdhFromSTM pgs EvalError $ "Invalid entry for dict: " <> T.pack
          (show val)
      insertToDict :: EdhValue -> (TVar DictStore) -> STM ()
      insertToDict p d = do
        (k, v) <- val2ItemPair p
        modifyTVar' d $ Map.insert k v
    case rhExpr of
      ForExpr argsRcvr iterExpr doExpr ->
        evalExpr that lhExpr $ \lhResult@(_, _, lhVal) -> case lhVal of
          EdhList (List l) -> runForLoop
            that
            argsRcvr
            iterExpr
            doExpr
            (\(_, _, val) -> modifyTVar' l (++ [val]))
            (\_ -> exitEdhProc exit lhResult)
          EdhDict (Dict d) -> runForLoop that
                                         argsRcvr
                                         iterExpr
                                         doExpr
                                         (\(_, _, val) -> insertToDict val d)
                                         (\_ -> exitEdhProc exit lhResult)
          EdhTuple vs -> contEdhSTM $ do
            l <- newTVar []
            runEdhProg pgs
              $ runForLoop that
                           argsRcvr
                           iterExpr
                           doExpr
                           (\(_, _, val) -> modifyTVar' l $ (val :))
              $ \_ -> contEdhSTM $ do
                  vs' <- readTVar l
                  exitEdhSTM pgs
                             exit
                             (that, callerScope, EdhTuple $ vs ++ reverse vs')
          _ ->
            throwEdh EvalError
              $  "Don't know how to comprehend into "
              <> T.pack (show $ edhTypeOf lhVal)
              <> ": "
              <> T.pack (show lhVal)
      _ -> evalExpr that lhExpr $ \lhResult@(_, _, lhVal) ->
        evalExpr that rhExpr $ \(_, _, rhVal) -> case lhVal of
          EdhTuple vs -> case rhVal of
            EdhTuple vs' ->
              exitEdhProc exit (that, callerScope, EdhTuple $ vs ++ vs')
            EdhList (List l) -> contEdhSTM $ do
              ll <- readTVar l
              exitEdhSTM pgs exit (that, callerScope, EdhTuple $ vs ++ ll)
            EdhDict (Dict d) -> contEdhSTM $ do
              ds <- readTVar d
              exitEdhSTM pgs
                         exit
                         (that, callerScope, EdhTuple $ vs ++ toPairList ds)
            _ ->
              throwEdh EvalError
                $  "Don't know how to comprehend from: "
                <> T.pack (show $ edhTypeOf rhVal)
                <> ": "
                <> T.pack (show rhVal)
          EdhList (List l) -> case rhVal of
            EdhTuple vs -> contEdhSTM $ do
              modifyTVar' l (++ vs)
              exitEdhSTM pgs exit lhResult
            EdhList (List l') -> contEdhSTM $ do
              ll <- readTVar l'
              modifyTVar' l (++ ll)
              exitEdhSTM pgs exit lhResult
            EdhDict (Dict d) -> contEdhSTM $ do
              ds <- readTVar d
              modifyTVar' l (++ (toPairList ds))
              exitEdhSTM pgs exit lhResult
            _ ->
              throwEdh EvalError
                $  "Don't know how to comprehend from: "
                <> T.pack (show $ edhTypeOf rhVal)
                <> ": "
                <> T.pack (show rhVal)
          EdhDict (Dict d) -> case rhVal of
            EdhTuple vs -> contEdhSTM $ do
              d' <- pvlToDict vs
              modifyTVar d $ Map.union d'
              exitEdhSTM pgs exit lhResult
            EdhList (List l) -> contEdhSTM $ do
              ll <- readTVar l
              d' <- pvlToDict ll
              modifyTVar d $ Map.union d'
              exitEdhSTM pgs exit lhResult
            EdhDict (Dict d') -> contEdhSTM $ do
              ds <- readTVar d'
              modifyTVar d $ Map.union ds
              exitEdhSTM pgs exit lhResult
            _ ->
              throwEdh EvalError
                $  "Don't know how to comprehend from: "
                <> T.pack (show $ edhTypeOf rhVal)
                <> ": "
                <> T.pack (show rhVal)
          _ ->
            throwEdh EvalError
              $  "Don't know how to comprehend into "
              <> T.pack (show $ edhTypeOf lhVal)
              <> ": "
              <> T.pack (show lhVal)
    -- return $ return ()
cprhProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)
