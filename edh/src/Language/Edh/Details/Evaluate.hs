{-# LANGUAGE TupleSections #-}

module Language.Edh.Details.Evaluate where

import           Prelude
import           Debug.Trace

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Concurrent.MVar
import           Control.Concurrent.STM

import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes
import           Language.Edh.Details.Tx
import           Language.Edh.Details.Utils


evalStmt
  :: Context -> StmtSrc -> ((Scope, EdhValue) -> EdhProg ()) -> EdhProg ()
evalStmt ctx (StmtSrc (srcPos, stmt)) exit = do
  txs <- ask
  liftIO
    $ handleJust
        Just
        (\(EvalError msg) -> runReaderT
          (unEdhProg
            (throwEdh $ EvalError $ msg <> "\nℹ️ " <> T.pack
              (sourcePosPretty srcPos)
            )
          )
          txs
        )
    $ runReaderT (unEdhProg (evalStmt' ctx stmt exit)) txs


evalStmt' :: Context -> Stmt -> ((Scope, EdhValue) -> EdhProg ()) -> EdhProg ()
evalStmt' ctx stmt exit = case stmt of

  ExprStmt expr             -> evalExpr ctx expr exit

  LetStmt argsRcvr argsSndr -> undefined

  BreakStmt                 -> exit (scope, EdhBreak)
  ContinueStmt              -> exit (scope, EdhContinue)
  FallthroughStmt           -> exit (scope, EdhFallthrough)
  -- TODO impl. this
  YieldStmt asend           -> undefined -- EdhYield <$>  
  ReturnStmt expr ->
    evalExpr ctx expr $ \(scope', v) -> exit (scope', EdhReturn v)


  ImportStmt ar srcExpr -> case srcExpr of
    LitExpr (StringLiteral moduPath) ->
      exit (scope, EdhString $ "wana import " <> moduPath <> ".edh huh?")
    expr ->
      throwEdh $ EvalError $ "don't know how to import " <> T.pack (show expr)


  VoidStmt -> exit (scope, nil)
  _ -> throwEdh $ EvalError $ "Eval not yet impl for: " <> T.pack (show stmt)
  where scope = contextScope ctx


evalExpr :: Context -> Expr -> ((Scope, EdhValue) -> EdhProg ()) -> EdhProg ()
evalExpr ctx expr exit = case expr of
  LitExpr lit -> case lit of
    DecLiteral    v -> exit (scope, EdhDecimal v)
    StringLiteral v -> exit (scope, EdhString v)
    BoolLiteral   v -> exit (scope, EdhBool v)
    NilLiteral      -> exit (scope, nil)
    TypeLiteral v   -> exit (scope, EdhType v)
    -- TODO impl this
    SinkCtor        -> throwEdh $ EvalError "sink ctor not impl. yet"

  PrefixExpr prefix expr' -> case prefix of
    PrefixPlus  -> eval' expr' exit
    PrefixMinus -> eval' expr' $ \case
      (scope', EdhDecimal v) -> exit (scope', EdhDecimal (-v))
      (_scope', v) ->
        throwEdh $ EvalError $ "Can not negate: " <> T.pack (show v) <> " ❌"
    Not -> eval' expr' $ \case
      (scope', EdhBool v) -> exit (scope', EdhBool $ not v)
      (_scope', v) ->
        throwEdh
          $  EvalError
          $  "Expect bool but got: "
          <> T.pack (show v)
          <> " ❌"

    -- TODO this should probably create Thunk instead, but mind to
    --      cooperate with the branch operator (->), find a way to
    --      tell it that this is guarded value, don't compare with
    --      thunk target value. but how ?
    Guard -> eval' expr' exit

    -- TODO impl these
    Go    -> throwEdh $ EvalError "goroutine starter not impl. yet"
    Defer -> throwEdh $ EvalError "defer scheduler not impl. yet"

  IfExpr cond cseq alt -> eval' cond $ \case
    (_scope', EdhBool True ) -> evalSS cseq exit
    (_scope', EdhBool False) -> case alt of
      Just elseClause -> evalSS elseClause exit
      _               -> exit (scope, nil)
    (_scope', v) ->
      -- we are so strongly typed
      throwEdh $ EvalError $ "Not a boolean value: " <> T.pack (show v) <> " ❌"

  DictExpr ps ->
    let
      evalPair :: (Expr, Expr) -> EdhProg (TMVar (ItemKey, EdhValue))
      evalPair (kExpr, vExpr) = do
        var <- liftIO newEmptyTMVarIO
        eval' vExpr $ \(_scope'v, vVal) -> eval' kExpr $ \(_scope'k, kVal) ->
          liftIO $ case kVal of
            EdhString  k -> atomically $ putTMVar var (ItemByStr k, vVal)
            EdhSymbol  k -> atomically $ putTMVar var (ItemBySym k, vVal)
            EdhDecimal k -> atomically $ putTMVar var (ItemByNum k, vVal)
            EdhBool    k -> atomically $ putTMVar var (ItemByBool k, vVal)
            k ->
              throwIO $ EvalError $ "Invalid key: " <> T.pack (show k) <> " ❌"
        return var
    in
      mapM evalPair ps >>= \pl -> do
        pl' <- liftIO $ mapM (atomically . readTMVar) pl
        d   <- EdhDict . Dict <$> (liftIO . newTVarIO) (Map.fromList pl')
        exit (scope, d)

  ListExpr vs -> mapM eval2 vs >>= \l -> do
    l' <- liftIO $ mapM ((snd <$>) . (atomically . readTMVar)) l
    v  <- liftIO $ EdhList . List <$> newTVarIO l'
    exit (scope, v)

  TupleExpr vs -> mapM eval2 vs >>= \l -> do
    l' <- liftIO $ mapM ((snd <$>) . (atomically . readTMVar)) l
    exit (scope, EdhTuple l')

  -- TODO this should check for Thunk, and implement
  --      break/fallthrough semantics
  BlockExpr stmts     -> exit (scope, EdhBlock stmts)

  -- TODO impl this
  -- ForExpr ar iter todo -> undefined

  GeneratorExpr sp pd -> exit
    ( scope
    , EdhGenrDef $ GenrDef { generatorOwnerObject = this
                           , generatorSourcePos   = sp
                           , generatorProcedure   = pd
                           }
    )

  AttrExpr addr -> case addr of
    ThisRef         -> exit (scope, EdhObject this)
    SupersRef       -> exit (scope, EdhTuple $ EdhObject <$> objSupers this)
    DirectRef addr' -> case addr' of
      NamedAttr attrName -> resolveEdhCtxAttr scope attrName >>= \case
        Just scope'@(Scope ent _obj) ->
          edhReadAttr ent (AttrByName attrName) (exit . (scope', ))
        Nothing -> throwEdh $ EvalError $ "Not in scope: " <> attrName
      SymbolicAttr _symName ->
        throwEdh $ EvalError "Symbolic attribute not impl. yet"
    IndirectRef tgtExpr addr' -> case tgtExpr of
      -- allow symbol value to be addressed off this reference 
      AttrExpr ThisRef -> case addr' of
        NamedAttr attrName -> resolveEdhObjAttr scope attrName >>= \case
          Just scope''@(Scope _ent obj') -> edhReadAttr (objEntity obj')
                                                        (AttrByName attrName)
                                                        (exit . (scope'', ))
          Nothing ->
            throwEdh
              $  EvalError
              $  "No attribute "
              <> attrName
              <> " from this "
              <> T.pack (show this)
        SymbolicAttr _symName ->
          throwEdh $ EvalError "Symbolic attribute not impl. yet"
      -- forbid symbol value to be addressed off non-this reference
      _ -> eval' tgtExpr $ \case
        (scope', EdhObject obj) -> case addr' of
          NamedAttr attrName -> resolveEdhObjAttr scope' attrName >>= \case
            Just scope''@(Scope _ent obj') ->
              edhReadAttr (objEntity obj') (AttrByName attrName) $ \val ->
                case val of
                  EdhSymbol sym ->
                    throwEdh
                      $  EvalError
                      $  "Symbol "
                      <> T.pack (show sym)
                      <> " can not be accessed"
                  _ -> exit (scope'', val)
            Nothing ->
              throwEdh
                $  EvalError
                $  "No attribute "
                <> attrName
                <> " from "
                <> T.pack (show obj)
          SymbolicAttr _symName ->
            throwEdh $ EvalError "Symbolic attribute not impl. yet"
        (_scope', val) ->
          throwEdh $ EvalError $ "Not an object: " <> T.pack (show val)


  -- IndexExpr ixExpr tgtExpr ->

  CallExpr procExpr args -> eval' procExpr $ \case
      -- EdhClass classDef -> 
      -- EdhMethod mthExpr -> 
      -- EdhGenrDef genrDef ->

    (scope', EdhHostProc (HostProcedure _name proc)) ->
      proc ctx args scope' >>= exit . (scope, )

    (_scope', v) ->
      throwEdh
        $  EvalError
        $  "Can not call: "
        <> T.pack (show v)
        <> " ❌ expressed with: "
        <> T.pack (show procExpr)


  -- InfixExpr op lhExpr rhExpr -> 

  _ -> throwEdh $ EvalError $ "Eval not yet impl for: " <> T.pack (show expr)
 where
  this  = thisObject scope
  scope = contextScope ctx

  eval' = evalExpr ctx

  eval2 :: Expr -> EdhProg (TMVar (Scope, EdhValue))
  eval2 expr' = do
    var <- liftIO newEmptyTMVarIO
    eval' expr' $ \sv -> liftIO $ atomically $ putTMVar var sv
    return var

  evalSS = evalStmt ctx


-- The Edh call convention is so called call-by-repacking, i.e. a new pack of
-- arguments are evaluated & packed at the calling site, then passed to the
-- callee site, where arguments in the pack are received into the callee's
-- entity, the receiving may include more packing into attributes manifested
-- for rest-args.

-- This is semantically much the same as Python's call convention, regarding
-- positional and keyword argument matching, and additionally supports:
--  * wildcard receiver - receive all keyword arguments into the entity
--  * retargeting - don't receive the argument into the entity, but assign
--    to an attribute of another object, typically `this` object in scope
--  * argument renaming - match the name as sent, receive to a  differently
--     named attribute of the entity. while renaming a positional argument
--     is doable but meaningless, you'd just use the later name
--

-- recvEdhArgs :: ArgsPack -> Context -> ArgsReceiver -> IO Entity
-- recvEdhArgs pck@(ArgsPack posArgs kwArgs) ctx argsRcvr = case argsRcvr of
--   PackReceiver argRcvrs -> do
--     (pck', attrs) <- foldM (recvFromPack ctx) (pck, Map.empty) argRcvrs
--     woResidual pck' attrs
--   SingleReceiver argRcvr -> do
--     (pck', attrs) <- recvFromPack ctx (pck, Map.empty) argRcvr
--     woResidual pck' attrs
--   WildReceiver -> if null posArgs
--     then newMVar $ Map.mapKeys AttrByName kwArgs
--     else
--       throwIO
--       $  EvalError
--       $  "Unexpected "
--       <> T.pack (show $ length posArgs)
--       <> " positional argument(s) to wild receiver"
--  where
--   scope = contextScope ctx
--   eval' = evalExpr ctx

--   woResidual :: ArgsPack -> Map.Map AttrKey EdhValue -> IO Entity
--   woResidual (ArgsPack posResidual kwResidual) attrs
--     | not (null posResidual)
--     = throwIO
--       $  EvalError
--       $  "Extraneous "
--       <> T.pack (show $ length posResidual)
--       <> " positional argument(s)"
--     | not (Map.null kwResidual)
--     = throwIO $ EvalError $ "Extraneous keyword arguments: " <> T.pack
--       (show $ Map.keys kwResidual)
--     | otherwise
--     = newMVar attrs
--   recvFromPack
--     :: Context
--     -> (ArgsPack, Map.Map AttrKey EdhValue)
--     -> ArgReceiver
--     -> IO (ArgsPack, Map.Map AttrKey EdhValue)
--   recvFromPack ctx ((ArgsPack posArgs' kwArgs'), attrs) argRcvr =
--     case argRcvr of
--       RecvRestPosArgs restPosArgAttr -> do
--         argsList <- newIORef posArgs'
--         return
--           ( ArgsPack [] kwArgs'
--           , Map.insert (AttrByName restPosArgAttr) (EdhList argsList) attrs
--           )
--       RecvRestKwArgs restKwArgAttr -> do
--         argsDict <- newIORef $ Dict $ Map.mapKeys ItemByStr kwArgs'
--         return
--           ( ArgsPack posArgs' Map.empty
--           , Map.insert (AttrByName restKwArgAttr) (EdhDict argsDict) attrs
--           )
--       RecvArg argName argTgtAddr argDefault -> do
--         (argVal, posArgs'', kwArgs'') <- resolveArgValue argName argDefault
--         case argTgtAddr of
--           Nothing -> do
--             return
--               ( ArgsPack posArgs'' kwArgs''
--               , Map.insert (AttrByName argName) argVal attrs
--               )
--           Just (DirectRef addr) -> case addr of
--             NamedAttr attrName -> -- simple rename
--                                   return
--               ( ArgsPack posArgs'' kwArgs''
--               , Map.insert (AttrByName argName) argVal attrs
--               )
--             SymbolicAttr symName -> -- todo support this ?
--               throwIO $ EvalError "arg renaming to symbolic attr not supported"
--           Just (IndirectRef addrExpr addr) ->
--               -- TODO impl. this
--             throwIO $ EvalError "arg retargeting not impl. yet"
--           tgt -> throwIO $ EvalError $ "Invalid argument retarget: " <> T.pack
--             (show tgt)
--    where
--     resolveArgValue
--       :: AttrName
--       -> Maybe Expr
--       -> IO (EdhValue, [EdhValue], Map.Map AttrName EdhValue)
--     resolveArgValue argName argDefault = do
--       let (inKwArgs, kwArgs'') = takeOutFromMap argName kwArgs'
--       case inKwArgs of
--         Nothing -> case posArgs' of
--           (posArg : posArgs'') -> return (posArg, posArgs'', kwArgs'')
--           []                   -> case argDefault of
--             Nothing -> throwIO $ EvalError $ "Missing argument: " <> argName
--             Just defaultExpr -> do
--               defaultVal <- eval' defaultExpr
--               return (defaultVal, posArgs', kwArgs'')
--         Just argVal -> return (argVal, posArgs', kwArgs'')


-- packEdhArgs :: Context -> ArgsSender -> (ArgsPack -> EdhProg ()) -> EdhProg ()
-- packEdhArgs ctx argsSender exit = case argsSender of
--   PackSender packSender -> do
--     (ArgsPack posArgs kwArgs) <- foldM fillPack
--                                        (ArgsPack [] Map.empty)
--                                        packSender
--     -- fillPack have posArgs filled right-to-left, reverse needed
--     exit $ ArgsPack (reverse posArgs) kwArgs
--   SingleSender argSender -> fillPack (ArgsPack [] Map.empty) argSender
--  where

--   fillPack :: ArgsPack -> ArgSender -> EdhProg ArgsPack
--   fillPack (ArgsPack posArgs kwArgs) argSender = case argSender of
--     UnpackPosArgs listExpr -> eval' listExpr >>= \case
--       EdhList listRef -> do
--         listVal <- readIORef listRef
--         return $ ArgsPack (posArgs ++ listVal) kwArgs
--       v -> throwIO $ EvalError $ "Can not unpack posargs from: " <> T.pack
--         (show v)
--     UnpackKwArgs dictExpr -> eval' dictExpr >>= \case
--       EdhDict dictRef -> do
--         Dict dictVal <- readIORef dictRef
--         return $ ArgsPack posArgs
--                           (Map.union (Map.mapKeys dictKey2Kw dictVal) kwArgs)
--       v -> throwIO $ EvalError $ "Can not unpack kwargs from: " <> T.pack
--         (show v)
--     SendPosArg argExpr -> do
--       argVal <- eval' argExpr
--       return $ ArgsPack (argVal : posArgs) kwArgs
--     SendKwArg kw argExpr -> do
--       argVal <- eval' argExpr
--       return $ ArgsPack posArgs $ Map.insert kw argVal kwArgs

--   dictKey2Kw :: ItemKey -> AttrName
--   dictKey2Kw = \case
--     ItemByStr name -> name
--     k ->
--       unsafePerformIO
--         $  throwIO
--         $  EvalError
--         $  "Invalid argument keyword from dict key: "
--         <> T.pack (show k)

--   eval' = evalExpr ctx

--   scope = contextScope ctx

--   resolveAddr :: AttrAddressor -> (AttrKey -> EdhProg ()) -> EdhProg ()
--   resolveAddr (NamedAttr attrName) exit = exit (AttrByName attrName)
--   resolveAddr (SymbolicAttr symName) exit =
--     resolveEdhObjAttr scope symName >>= \case
--       Just ent -> edhTxRead
--         (ent, AttrByName symName)
--         \case
--           (EdhSymbol symVal) -> exit (AttrBySym symVal)
--           v -> throwEdh $ EvalError $ "Not a symbol: " <> T.pack (show v)
--       Nothing ->
--         throwEdh
--           $  EvalError
--           $  "No symbol named "
--           <> T.pack (show symName)
--           <> " available"
--       Just v ->
--         throwEdh
--           $  EvalError
--           $  "Expect a symbol named "
--           <> T.pack (show symName)
--           <> " but got: "
--           <> T.pack (show v)


resolveLexicalAttr :: MonadIO m => [Scope] -> AttrName -> m (Maybe Scope)
resolveLexicalAttr [] _ = return Nothing
resolveLexicalAttr (scope@(Scope ent _obj) : outerEntities) attrName =
  liftIO $ readTVarIO ent >>= \em -> if Map.member (AttrByName attrName) em
    then return (Just scope)
    else resolveLexicalAttr outerEntities attrName


resolveEdhCtxAttr :: MonadIO m => Scope -> AttrName -> m (Maybe Scope)
resolveEdhCtxAttr scope attr = liftIO $ readTVarIO ent >>= \em ->
  if Map.member (AttrByName attr) em
    then return (Just scope)
    else resolveLexicalAttr (classScope $ objClass obj) attr
 where
  ent = scopeEntity scope
  obj = thisObject scope


resolveEdhObjAttr :: MonadIO m => Scope -> AttrName -> m (Maybe Scope)
resolveEdhObjAttr scope attr = liftIO $ readTVarIO objEnt >>= \em ->
  if Map.member (AttrByName attr) em
    then return (Just scope)
    else resolveEdhSuperAttr (objSupers obj) attr
 where
  obj    = thisObject scope
  objEnt = objEntity obj

resolveEdhSuperAttr :: MonadIO m => [Object] -> AttrName -> m (Maybe Scope)
resolveEdhSuperAttr [] _ = return Nothing
resolveEdhSuperAttr (super : restSupers) attr =
  liftIO $ readTVarIO objEnt >>= \em -> if Map.member (AttrByName attr) em
    then return (Just (Scope objEnt super))
    else resolveEdhSuperAttr restSupers attr
  where objEnt = objEntity super

