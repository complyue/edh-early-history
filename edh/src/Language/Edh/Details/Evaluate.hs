{-# LANGUAGE TupleSections #-}

module Language.Edh.Details.Evaluate where

import           Prelude
-- import           Debug.Trace

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


evalExprToVar :: Context -> Expr -> EdhProg (TMVar (Scope, EdhValue))
evalExprToVar ctx expr = do
  var <- liftIO newEmptyTMVarIO
  evalExpr ctx expr $ \sv -> liftIO $ atomically $ putTMVar var sv
  return var


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
    Guard  -> eval' expr' exit

    AtoIso -> withEdhTx' (eval2 expr')
      $ \var -> (liftIO $ atomically $ readTMVar var) >>= exit

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

  -- TODO this eval from right to left ? correct it if so
  DictExpr ps ->
    let
      gatherDict
        :: (DictStore -> EdhProg ())
        -> (Expr, Expr)
        -> EdhProg (DictStore -> EdhProg ())
      gatherDict exit' (!kExpr, !vExpr) = return $ \ds ->
        eval' vExpr $ \(!_scope'v, !vVal) ->
          eval' kExpr $ \(!_scope'k, !kVal) -> do
            key <- case kVal of
              EdhString  k -> return $ ItemByStr k
              EdhSymbol  k -> return $ ItemBySym k
              EdhDecimal k -> return $ ItemByNum k
              EdhBool    k -> return $ ItemByBool k
              k ->
                throwEdh
                  $  EvalError
                  $  "Invalid key: "
                  <> T.pack (show k)
                  <> " ❌"
            exit' $ Map.alter
              (\case -- give later entries higher priority
                Nothing  -> Just vVal
                Just val -> Just val
              )
              key
              ds
    in
      foldM
          gatherDict
          (\ds ->
            (liftIO $ EdhDict . Dict <$> newTVarIO ds) >>= (exit . (scope, ))
          )
          ps
        >>= ($ Map.empty)

  -- TODO this eval from right to left ? correct it if so
  ListExpr xs ->
    let gatherValues
          :: ([EdhValue] -> EdhProg ())
          -> Expr
          -> EdhProg ([EdhValue] -> EdhProg ())
        gatherValues exit' expr' =
            return $ \vs -> eval' expr' $ \(_, !val) -> exit' (val : vs)
    in  foldM
            gatherValues
            (\vs ->
              (liftIO $ EdhList . List <$> newTVarIO vs) >>= (exit . (scope, ))
            )
            xs
          >>= ($ [])

  -- TODO this eval from right to left ? correct it if so
  TupleExpr xs ->
    let gatherValues
          :: ([EdhValue] -> EdhProg ())
          -> Expr
          -> EdhProg ([EdhValue] -> EdhProg ())
        gatherValues exit' expr' =
            return $ \vs -> eval' expr' $ \(_, !val) -> exit' (val : vs)
    in  foldM gatherValues (\vs -> exit (scope, EdhTuple vs)) xs >>= ($ [])

  ParenExpr x         -> eval' x exit

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
    DirectRef addr' -> resolveAddr scope addr' $ \key ->
      resolveEdhCtxAttr scope key >>= \case
        Nothing ->
          throwEdh $ EvalError $ "Not in scope: " <> T.pack (show addr')
        Just scope'@(Scope !ent !_obj) ->
          edhReadAttr ent key (exit . (scope', ))
    IndirectRef !tgtExpr !addr' -> resolveAddr scope addr' $ \key ->
      eval' tgtExpr $ \case
        (_, EdhObject !obj) ->
          resolveEdhObjAttr (Scope (objEntity obj) obj) key >>= \case
            Nothing ->
              throwEdh
                $  EvalError
                $  "No such attribute "
                <> T.pack (show key)
                <> " from "
                <> T.pack (show obj)
            Just scope''@(Scope !ent !_obj) ->
              edhReadAttr ent key (exit . (scope'', ))
        (_, v) -> throwEdh $ EvalError $ "Not an object: " <> T.pack (show v)


  -- IndexExpr ixExpr tgtExpr ->

  CallExpr procExpr args -> eval' procExpr $ \case
      -- EdhClass classDef -> 
      -- EdhMethod mthExpr -> 
      -- EdhGenrDef genrDef ->

    (scope', EdhHostProc (HostProcedure _name proc)) ->
      proc ctx args scope' exit

    (_scope', val) ->
      throwEdh
        $  EvalError
        $  "Can not call: "
        <> T.pack (show val)
        <> " ❌ expressed with: "
        <> T.pack (show procExpr)


  InfixExpr opSym lhExpr rhExpr ->
    resolveEdhCtxAttr scope (AttrByName opSym) >>= \case
      Nothing ->
        throwEdh
          $  EvalError
          $  "Operator ("
          <> T.pack (show opSym)
          <> ") not in scope"
      Just scope'@(Scope !ent !_obj) ->
        edhReadAttr ent (AttrByName opSym) $ \case
          EdhHostProc (HostProcedure !_name !proc) -> proc
            ctx
            (PackSender [SendPosArg lhExpr, SendPosArg rhExpr])
            scope'
            exit
          -- TODO handle operator procedures etc.
          val -> throwEdh $ EvalError $ "Not callable: " <> T.pack (show val)


  _ -> throwEdh $ EvalError $ "Eval not yet impl for: " <> T.pack (show expr)
 where
  !this  = thisObject scope
  !scope = contextScope ctx

  eval'  = evalExpr ctx
  eval2  = evalExprToVar ctx
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


packEdhArgs :: Context -> ArgsSender -> (ArgsPack -> EdhProg ()) -> EdhProg ()
packEdhArgs ctx argsSender exit = case argsSender of
  PackSender packSender ->
    foldM fillPack exit packSender >>= ($ ArgsPack [] Map.empty)
  SingleSender argSender ->
    fillPack exit argSender >>= ($ ArgsPack [] Map.empty)
 where
  eval' = evalExpr ctx

  fillPack
    :: (ArgsPack -> EdhProg ()) -> ArgSender -> EdhProg (ArgsPack -> EdhProg ())
  fillPack exit' argSender = return $ \(ArgsPack posArgs kwArgs) ->
    case argSender of
      UnpackPosArgs listExpr -> eval' listExpr $ \case
        (_scope, EdhList (List listVar)) -> do
          listVal <- liftIO $ readTVarIO listVar
          exit' $ ArgsPack (listVal ++ posArgs) kwArgs
        (_scope, v) ->
          throwEdh $ EvalError $ "Can not unpack args from: " <> T.pack
            (show v)
      UnpackKwArgs dictExpr -> eval' dictExpr $ \case
        (_scope, EdhDict (Dict dictVar)) -> do
          dictVal   <- liftIO $ readTVarIO dictVar
          kwAscList <- forM (Map.toAscList dictVal)
            $ \(k, v) -> (, v) <$> dictKey2Kw k
          -- let kwMap = Map.fromAscList kwAscList
          -- kwMap   <-
          --   Map.fromAscList <$> forM (Map.toAscList dictVal) $ \(k, v) ->
          --     (, v) <$> dictKey2Kw k
          -- kwArgs appear later, give them higher priority
          exit'
            $ ArgsPack posArgs (Map.union kwArgs $ Map.fromAscList kwAscList)
        (_scope, v) ->
          throwEdh $ EvalError $ "Can not unpack kwargs from: " <> T.pack
            (show v)
      SendPosArg argExpr -> eval' argExpr
        $ \(_scope, argVal) -> exit' $ ArgsPack (argVal : posArgs) kwArgs
      SendKwArg kw argExpr -> eval' argExpr $ \(_scope, argVal) ->
        -- kwArgs appear later, give them higher priority
        exit' $ ArgsPack posArgs $ Map.alter
          (\case
            Nothing  -> Just argVal
            Just val -> Just val
          )
          kw
          kwArgs

  dictKey2Kw :: ItemKey -> EdhProg AttrName
  dictKey2Kw = \case
    ItemByStr name -> return name
    k ->
      throwEdh
        $  EvalError
        $  "Invalid argument keyword from dict key: "
        <> T.pack (show k)


resolveAddr :: Scope -> AttrAddressor -> (AttrKey -> EdhProg ()) -> EdhProg ()
resolveAddr _ (NamedAttr attrName) exit = exit (AttrByName attrName)
resolveAddr scope (SymbolicAttr symName) exit =
  resolveEdhCtxAttr scope (AttrByName symName) >>= \case
    Just scope' ->
      edhReadAttr (scopeEntity scope') (AttrByName symName) $ \case
        (EdhSymbol symVal) -> exit (AttrBySym symVal)
        v ->
          throwEdh
            $  EvalError
            $  "Not a symbol: "
            <> T.pack (show v)
            <> " as "
            <> symName
            <> " from "
            <> T.pack (show $ thisObject scope') -- TODO this correct ?
    Nothing ->
      throwEdh
        $  EvalError
        $  "No symbol named "
        <> T.pack (show symName)
        <> " available"


resolveLexicalAttr :: MonadIO m => [Scope] -> AttrKey -> m (Maybe Scope)
resolveLexicalAttr [] _ = return Nothing
resolveLexicalAttr (scope@(Scope ent _obj) : outerEntities) addr =
  liftIO $ readTVarIO ent >>= \em -> if Map.member addr em
    then return (Just scope)
    else resolveLexicalAttr outerEntities addr


resolveEdhCtxAttr :: MonadIO m => Scope -> AttrKey -> m (Maybe Scope)
resolveEdhCtxAttr scope addr = liftIO $ readTVarIO ent >>= \em ->
  if Map.member addr em
    then return (Just scope)
    else resolveLexicalAttr (classScope $ objClass obj) addr
 where
  ent = scopeEntity scope
  obj = thisObject scope


resolveEdhObjAttr :: MonadIO m => Scope -> AttrKey -> m (Maybe Scope)
resolveEdhObjAttr scope addr = liftIO $ readTVarIO objEnt >>= \em ->
  if Map.member addr em
    then return (Just scope)
    else resolveEdhSuperAttr (objSupers obj) addr
 where
  obj    = thisObject scope
  objEnt = objEntity obj

resolveEdhSuperAttr :: MonadIO m => [Object] -> AttrKey -> m (Maybe Scope)
resolveEdhSuperAttr [] _ = return Nothing
resolveEdhSuperAttr (super : restSupers) addr =
  liftIO $ readTVarIO objEnt >>= \em -> if Map.member addr em
    then return (Just (Scope objEnt super))
    else resolveEdhSuperAttr restSupers addr
  where objEnt = objEntity super

