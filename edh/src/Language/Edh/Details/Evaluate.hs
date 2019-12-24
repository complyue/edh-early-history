{-# LANGUAGE TupleSections #-}

module Language.Edh.Details.Evaluate where

import           Prelude
-- import           Debug.Trace

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes
import           Language.Edh.Details.Tx
import           Language.Edh.Details.Utils


evalStmt :: StmtSrc -> EdhProcExit -> EdhProg (STM ())
evalStmt (StmtSrc (!srcPos, !stmt)) !exit = do
  !stmProg <- (evalStmt' stmt exit)
  return $ catchSTM stmProg $ \e -> case fromException e of
    Just (EvalError msg) ->
      throwSTM $ EvalError $ msg <> "\nℹ️ " <> T.pack (sourcePosPretty srcPos)
    Nothing -> throwSTM e


evalBlock :: [StmtSrc] -> EdhProcExit -> EdhProg (STM ())
evalBlock [] !exit = do
  !pgs <- ask
  let !ctx = edh'context pgs
  exitEdhProc exit (contextScope ctx, nil)
evalBlock [!ss] !exit = evalStmt ss $ \result ->
  exitEdhProc exit $ case result of
    (!scope', EdhCaseClose !val) -> (scope', val)
    _                            -> result
evalBlock (ss : rest) !exit = evalStmt ss $ \result -> case result of
  (!scope', EdhCaseClose !val) -> exitEdhProc exit (scope', val)
  brk@(!_, EdhBreak) -> exitEdhProc exit brk
  ctn@(!_, EdhContinue) -> exitEdhProc exit ctn
  rtn@(!_, EdhReturn !_) -> exitEdhProc exit rtn
  yld@(!_, EdhYield !_) -> exitEdhProc exit yld
  (!_, EdhFallthrough) -> evalBlock rest exit
  _ -> evalBlock rest exit


evalExprs :: [Expr] -> EdhProcExit -> EdhProg (STM ())
-- here 'EdhTuple' is used for intermediate tag,
-- not returning real tuple values as in Edh.
evalExprs [] exit = do
  pgs <- ask
  let !scope = contextScope ctx
      !ctx   = edh'context pgs
  exit (scope, EdhTuple [])
evalExprs (x : xs) exit = evalExpr x $ \(scope, val) ->
  evalExprs xs $ \(_, tv) -> case tv of
    EdhTuple l -> exitEdhProc exit (scope, EdhTuple (val : l))
    _          -> error "bug"


evalStmt' :: Stmt -> EdhProcExit -> EdhProg (STM ())
evalStmt' stmt exit = do
  !pgs <- ask
  let !ctx                      = edh'context pgs
      !scope@(Scope !ent !this) = contextScope ctx
  case stmt of

    ExprStmt expr -> evalExpr expr exit

    LetStmt argsRcvr argsSndr ->
      -- ensure sending and receiving happens within a same tx
      -- for atomicity of the let statement
      local (\pgs' -> pgs' { edh'in'tx = True })
        $ packEdhArgs argsSndr
        $ \(_, pkv) -> case pkv of
            EdhArgsPack pk -> recvEdhArgs argsRcvr pk $ \(_, scopeObj) ->
              case scopeObj of
                EdhObject (Object ent' cls [])
                  | cls == (scopeClass $ contextWorld ctx) -> return $ do
                    -- overwrite current scope entity with attributes from
                    -- the received entity
                    um <- readTVar ent'
                    modifyTVar' ent $ \em -> Map.union um em
                    -- let statement evaluates to nil always
                    join $ runReaderT (exitEdhProc exit (scope, nil)) pgs
                _ -> error "bug"
            _ -> error "bug"

    BreakStmt       -> exitEdhProc exit (scope, EdhBreak)
    ContinueStmt    -> exitEdhProc exit (scope, EdhContinue)
    FallthroughStmt -> exitEdhProc exit (scope, EdhFallthrough)

    YieldStmt expr ->
      evalExpr expr $ \(_, !val) -> exitEdhProc exit (scope, EdhYield val)
    ReturnStmt expr ->
      evalExpr expr $ \(_, !val) -> exitEdhProc exit (scope, EdhReturn val)

    GeneratorStmt name pd -> return $ do
      let genr = EdhGenrDef $ GenrDef { generatorOwnerObject = this
                                      , generatorName        = name
                                      , generatorProcedure   = pd
                                      }
      modifyTVar' ent $ \em -> Map.insert (AttrByName name) genr em
      join $ runReaderT (exitEdhProc exit (scope, nil)) pgs

    ImportStmt _argsRcvr srcExpr -> case srcExpr of
      LitExpr (StringLiteral moduPath) -> exitEdhProc
        exit
        (scope, EdhString $ "wana import " <> moduPath <> ".edh huh?")
      expr ->
        throwEdh $ EvalError $ "don't know how to import " <> T.pack (show expr)

    VoidStmt -> exitEdhProc exit (scope, nil)

    -- TODO comment out this once `case stmt` get total
    _ -> throwEdh $ EvalError $ "Eval not yet impl for: " <> T.pack (show stmt)


evalExpr :: Expr -> EdhProcExit -> EdhProg (STM ())
evalExpr expr exit = do
  pgs <- ask
  let ctx   = edh'context pgs
      scope = contextScope ctx
      this  = thisObject scope
  case expr of
    LitExpr lit -> case lit of
      DecLiteral    v -> exitEdhProc exit (scope, EdhDecimal v)
      StringLiteral v -> exitEdhProc exit (scope, EdhString v)
      BoolLiteral   v -> exitEdhProc exit (scope, EdhBool v)
      NilLiteral      -> exitEdhProc exit (scope, nil)
      TypeLiteral v   -> exitEdhProc exit (scope, EdhType v)
      -- TODO impl this
      SinkCtor        -> throwEdh $ EvalError "sink ctor not impl. yet"

    PrefixExpr prefix expr' -> case prefix of
      PrefixPlus  -> evalExpr expr' exit
      PrefixMinus -> evalExpr expr' $ \case
        (scope', EdhDecimal v) -> exitEdhProc exit (scope', EdhDecimal (-v))
        (_scope', v) ->
          throwEdh $ EvalError $ "Can not negate: " <> T.pack (show v) <> " ❌"
      Not -> evalExpr expr' $ \case
        (scope', EdhBool v) -> exitEdhProc exit (scope', EdhBool $ not v)
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
      Guard  -> evalExpr expr' exit

      AtoIso -> local (\s -> s { edh'in'tx = True }) $ evalExpr expr' exit

      -- TODO impl these
      Go     -> throwEdh $ EvalError "goroutine starter not impl. yet"
      Defer  -> throwEdh $ EvalError "defer scheduler not impl. yet"

    IfExpr cond cseq alt -> evalExpr cond $ \case
      (_scope', EdhBool True ) -> evalStmt cseq exit
      (_scope', EdhBool False) -> case alt of
        Just elseClause -> evalStmt elseClause exit
        _               -> exitEdhProc exit (scope, nil)
      (_scope', v) ->
        -- we are so strongly typed
        throwEdh
          $  EvalError
          $  "Not a boolean value: "
          <> T.pack (show v)
          <> " ❌"

    DictExpr xs -> -- make sure dict k:v pairs are evaluated in same tx
      local (\s -> s { edh'in'tx = True }) $ evalExprs xs $ \(_, tv) ->
        case tv of
          EdhTuple l -> return $ do
            dpl <- forM l $ \case
              EdhPair kVal vVal -> (, vVal) <$> case kVal of
                EdhType    k -> return $ ItemByType k
                EdhString  k -> return $ ItemByStr k
                EdhSymbol  k -> return $ ItemBySym k
                EdhDecimal k -> return $ ItemByNum k
                EdhBool    k -> return $ ItemByBool k
                k ->
                  throwSTM
                    $  EvalError
                    $  "Invalid dict key: "
                    <> T.pack (show k)
                    <> " ❌"
              pv ->
                throwSTM
                  $  EvalError
                  $  "Invalid dict entry: "
                  <> T.pack (show pv)
                  <> " ❌"
            ds <- newTVar $ Map.fromList dpl
            join $ runReaderT (exitEdhProc exit (scope, EdhDict (Dict ds))) pgs
          _ -> error "bug"

    ListExpr xs -> -- make sure list values are evaluated in same tx
      local (\s -> s { edh'in'tx = True }) $ evalExprs xs $ \(_, tv) ->
        case tv of
          EdhTuple l -> return $ do
            ll <- List <$> newTVar l
            join $ runReaderT (exitEdhProc exit (scope, EdhList ll)) pgs
          _ -> error "bug"

    TupleExpr xs -> -- make sure tuple values are evaluated in same tx
      local (\s -> s { edh'in'tx = True }) $ evalExprs xs $ \(_, tv) ->
        case tv of
          EdhTuple l -> exitEdhProc exit (scope, EdhTuple l)
          _          -> error "bug"

    ParenExpr x     -> evalExpr x exit

    -- TODO this should check for Thunk, and implement
    --      break/fallthrough semantics
    BlockExpr stmts -> evalBlock stmts exit

    -- TODO impl this
    -- ForExpr ar iter todo -> undefined

    AttrExpr  addr  -> case addr of
      ThisRef -> exitEdhProc exit (scope, EdhObject this)
      SupersRef ->
        exitEdhProc exit (scope, EdhTuple $ EdhObject <$> objSupers this)
      DirectRef !addr' -> return $ do
        !key <- resolveAddr scope addr'
        resolveEdhCtxAttr scope key >>= \case
          Nothing ->
            throwSTM $ EvalError $ "Not in scope: " <> T.pack (show addr')
          Just scope'@(Scope !ent !_obj) -> do
            em <- readTVar ent
            case Map.lookup key em of
              Nothing -> throwSTM $ EvalError "attr resolving bug"
              Just val ->
                join $ runReaderT (exitEdhProc exit (scope', val)) pgs
      IndirectRef !tgtExpr !addr' -> evalExpr tgtExpr $ \case
        (_, EdhObject !obj) -> return $ do
          !key <- resolveAddr scope addr'
          resolveEdhObjAttr obj key >>= \case
            Nothing ->
              throwSTM
                $  EvalError
                $  "No such attribute "
                <> T.pack (show key)
                <> " from "
                <> T.pack (show obj)
            Just scope'@(Scope !ent !_obj) -> do
              em <- readTVar ent
              case Map.lookup key em of
                Nothing -> throwSTM $ EvalError "attr resolving bug"
                Just val ->
                  join $ runReaderT (exitEdhProc exit (scope', val)) pgs
        (_, v) -> throwEdh $ EvalError $ "Not an object: " <> T.pack (show v)


    -- IndexExpr ixExpr tgtExpr ->

    CallExpr procExpr args -> evalExpr procExpr $ \case
        -- EdhClass classDef -> 
        -- EdhMethod mthExpr -> 
        -- EdhGenrDef genrDef ->

      (scope', EdhHostProc (HostProcedure _name proc)) -> proc args scope' exit

      (_scope', val) ->
        throwEdh
          $  EvalError
          $  "Can not call: "
          <> T.pack (show val)
          <> " ❌ expressed with: "
          <> T.pack (show procExpr)


    InfixExpr !opSym !lhExpr !rhExpr ->
      return $ resolveEdhCtxAttr scope (AttrByName opSym) >>= \case
        Nothing ->
          throwSTM
            $  EvalError
            $  "Operator ("
            <> T.pack (show opSym)
            <> ") not in scope"
        Just scope'@(Scope !ent !_obj) -> do
          em <- readTVar ent
          case Map.lookup (AttrByName opSym) em of
            Nothing -> error "attr resolving bug"
            Just (EdhHostProc (HostProcedure !_name !proc)) ->
              join $ runReaderT
                (proc (PackSender [SendPosArg lhExpr, SendPosArg rhExpr])
                      scope'
                      exit
                )
                pgs
            -- TODO handle operator procedures etc.
            Just val ->
              throwSTM $ EvalError $ "Not callable: " <> T.pack (show val)


    _ -> throwEdh $ EvalError $ "Eval not yet impl for: " <> T.pack (show expr)


-- | assign an evaluated value to a target expression
--
-- Note the calling procedure should declare in-tx state in evaluating the
-- right-handle value as well as running this, so the evaluation of the
-- right-hand value as well as the writting to the target entity are done
-- within the same tx, thus for atomicity of the whole assignment.
-- and it should pass the first argument to indicate whether to maintain
-- in-tx state after the assignment done.
assignEdhTarget
  :: Bool -> Expr -> EdhProcExit -> (Scope, EdhValue) -> EdhProg (STM ())
assignEdhTarget inTxAfter lhExpr exit (_, rhVal) = do
  pgs <- ask
  let !callerCtx                      = edh'context pgs
      !callerScope@(Scope !ent !this) = contextScope callerCtx
      !thisEnt                        = objEntity this
      -- assigment result as if come from the calling scope
      !result                         = (callerScope, rhVal)
      finishAssign :: Entity -> AttrKey -> STM ()
      finishAssign ent' key = do
        modifyTVar' ent' $ \em -> Map.insert key rhVal em
        -- restore in-tx state
        join $ runReaderT (exitEdhProc exit result)
                          (pgs { edh'in'tx = inTxAfter })
  case lhExpr of
    AttrExpr !addr -> case addr of
      DirectRef !addr' ->
        return $ resolveAddr callerScope addr' >>= \key -> finishAssign ent key
      IndirectRef !tgtExpr !addr' -> case tgtExpr of
        AttrExpr ThisRef -> return $ resolveAddr callerScope addr' >>= \key ->
          finishAssign thisEnt key
        AttrExpr SupersRef ->
          throwEdh $ EvalError "Can not assign an attribute to supers"
        _ -> evalExpr tgtExpr $ \(_, !tgtVal) -> case tgtVal of
          EdhObject (Object !tgtEnt _ _) ->
            return $ resolveAddr callerScope addr' >>= \key ->
              finishAssign tgtEnt key
          _ -> throwEdh $ EvalError $ "Invalid assignment target: " <> T.pack
            (show tgtVal)
      ThisRef   -> throwEdh $ EvalError "Can not assign to this"
      SupersRef -> throwEdh $ EvalError "Can not assign to supers"
    x ->
      throwEdh
        $  EvalError
        $  "Invalid left hand value for assignment: "
        <> T.pack (show x)


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
--- * rest-args repacking, in forms of:
---     *args
---     **kwargs
---     ***pkargs


recvEdhArgs :: ArgsReceiver -> ArgsPack -> EdhProcExit -> EdhProg (STM ())
recvEdhArgs !argsRcvr pck@(ArgsPack !posArgs !kwArgs) !exit = do
  pgs <- ask
  let
    !callerCtx   = edh'context pgs
    !callerScope = contextScope callerCtx
    !world       = contextWorld callerCtx
    recvFromPack
      :: (ArgsPack, EntityStore) -> ArgReceiver -> STM (ArgsPack, EntityStore)
    recvFromPack (pk@(ArgsPack posArgs' kwArgs'), em) argRcvr = case argRcvr of
      RecvRestPosArgs restPosArgAttr -> return
        ( ArgsPack [] kwArgs'
        , Map.insert (AttrByName restPosArgAttr)
                     (EdhArgsPack $ ArgsPack posArgs' Map.empty)
                     em
        )
      RecvRestKwArgs restKwArgAttr -> return
        ( ArgsPack posArgs' Map.empty
        , Map.insert (AttrByName restKwArgAttr)
                     (EdhArgsPack $ ArgsPack [] kwArgs')
                     em
        )
      RecvRestPkArgs restPkArgAttr -> return
        ( ArgsPack [] Map.empty
        , Map.insert (AttrByName restPkArgAttr) (EdhArgsPack pk) em
        )
      RecvArg argName argTgtAddr argDefault -> do
        (argVal, posArgs'', kwArgs'') <- resolveArgValue argName argDefault
        case argTgtAddr of
          Nothing ->
            return
              ( ArgsPack posArgs'' kwArgs''
              , Map.insert (AttrByName argName) argVal em
              )
          Just (DirectRef addr) -> case addr of
            NamedAttr attrName -> -- simple rename
              return
                ( ArgsPack posArgs'' kwArgs''
                , Map.insert (AttrByName attrName) argVal em
                )
            SymbolicAttr _symName -> -- todo support this ?
                                     throwSTM
              $ EvalError "arg renaming to symbolic attr not supported"
          Just addr@(IndirectRef _ _) -> do
            join $ runReaderT
              (assignEdhTarget True
                               (AttrExpr addr)
                               (const $ return $ return ())
                               (callerScope, argVal)
              )
              (pgs { edh'in'tx = True })
            return (ArgsPack posArgs'' kwArgs'', em)
          tgt ->
            throwSTM $ EvalError $ "Invalid argument retarget: " <> T.pack
              (show tgt)
     where
      resolveArgValue
        :: AttrName
        -> Maybe Expr
        -> STM (EdhValue, [EdhValue], Map.Map AttrName EdhValue)
      resolveArgValue argName argDefault = do
        let (inKwArgs, kwArgs'') = takeOutFromMap argName kwArgs'
        case inKwArgs of
          Just argVal -> return (argVal, posArgs', kwArgs'')
          _           -> case posArgs' of
            (posArg : posArgs'') -> return (posArg, posArgs'', kwArgs'')
            []                   -> case argDefault of
              Just defaultExpr -> do
                defaultVar <- newEmptyTMVar
                join $ runReaderT
                  (evalExpr defaultExpr
                            (\(_, val) -> return (putTMVar defaultVar val))
                  )
                  (pgs { edh'in'tx = True })
                defaultVal <- readTMVar defaultVar
                return (defaultVal, posArgs', kwArgs'')
              _ -> throwSTM $ EvalError $ "Missing argument: " <> argName
    doReturn :: Entity -> STM ()
    doReturn ent = join $ runReaderT
      (exitEdhProc exit
                   (callerScope, EdhObject $ Object ent (scopeClass world) [])
      )
      pgs -- execute outer code wrt what tx state originally is
  -- execution of the args receiving always in a tx for atomicity
  local (\pgs' -> pgs' { edh'in'tx = True }) $ case argsRcvr of
    PackReceiver argRcvrs -> return $ do
      (pck', em) <- foldM recvFromPack (pck, Map.empty) argRcvrs
      ent        <- woResidual pck' em
      doReturn ent
    SingleReceiver argRcvr -> return $ do
      (pck', em) <- recvFromPack (pck, Map.empty) argRcvr
      ent        <- woResidual pck' em
      doReturn ent
    WildReceiver -> return $ if null posArgs
      then do
        ent <- newTVar $ Map.mapKeys AttrByName kwArgs
        doReturn ent
      else
        throwSTM
        $  EvalError
        $  "Unexpected "
        <> T.pack (show $ length posArgs)
        <> " positional argument(s) to wild receiver"
 where
  woResidual :: ArgsPack -> EntityStore -> STM Entity
  woResidual (ArgsPack !posResidual !kwResidual) em
    | not (null posResidual)
    = throwSTM
      $  EvalError
      $  "Extraneous "
      <> T.pack (show $ length posResidual)
      <> " positional argument(s)"
    | not (Map.null kwResidual)
    = throwSTM $ EvalError $ "Extraneous keyword arguments: " <> T.unwords
      (Map.keys kwResidual)
    | otherwise
    = newTVar em


packEdhArgs :: ArgsSender -> EdhProcExit -> EdhProg (STM ())
-- make sure values in a pack are evaluated in same tx
packEdhArgs argsSender exit =
  local (\s -> s { edh'in'tx = True }) $ case argsSender of
    PackSender   argSenders -> packEdhArgs' argSenders exit
    SingleSender argSender  -> packEdhArgs' [argSender] exit

packEdhArgs' :: [ArgSender] -> EdhProcExit -> EdhProg (STM ())
packEdhArgs' [] exit = do
  pgs <- ask
  let !scope = contextScope ctx
      !ctx   = edh'context pgs
  exit (scope, EdhArgsPack $ ArgsPack [] Map.empty)
packEdhArgs' (x : xs) exit = do
  pgs <- ask
  let !scope = contextScope ctx
      !ctx   = edh'context pgs
  case x of
    UnpackPosArgs listExpr -> evalExpr listExpr $ \case
      (_, EdhList (List l)) -> packEdhArgs' xs $ \(_, pk) -> case pk of
        EdhArgsPack (ArgsPack !posArgs !kwArgs) -> return $ do
          ll <- readTVar l
          join $ runReaderT
            (exitEdhProc
              exit
              (scope, EdhArgsPack (ArgsPack (posArgs ++ ll) kwArgs))
            )
            pgs
        _ -> error "bug"
      (_, v) ->
        throwEdh $ EvalError $ "Can not unpack args from: " <> T.pack (show v)
    UnpackKwArgs dictExpr -> evalExpr dictExpr $ \case
      (_, EdhDict (Dict ds)) -> packEdhArgs' xs $ \(_, pk) -> case pk of
        EdhArgsPack (ArgsPack !posArgs !kwArgs) -> return $ do
          dm  <- readTVar ds
          kvl <- forM (Map.toAscList dm) $ \(k, v) -> (, v) <$> dictKey2Kw k
          join $ runReaderT
            (exitEdhProc
              exit
              ( scope
              , EdhArgsPack
                (ArgsPack posArgs $ Map.union kwArgs $ Map.fromAscList kvl)
              )
            )
            pgs
        _ -> error "bug"
      (_, v) ->
        throwEdh $ EvalError $ "Can not unpack kwargs from: " <> T.pack (show v)
    SendPosArg argExpr -> evalExpr argExpr $ \(_, val) ->
      packEdhArgs' xs $ \(_, pk) -> case pk of
        EdhArgsPack (ArgsPack !posArgs !kwArgs) -> exitEdhProc
          exit
          (scope, EdhArgsPack (ArgsPack (val : posArgs) kwArgs))
        _ -> error "bug"
    SendKwArg kw argExpr -> evalExpr argExpr $ \(_, val) ->
      packEdhArgs' xs $ \(_, pk) -> case pk of
        EdhArgsPack (ArgsPack !posArgs !kwArgs) -> exitEdhProc
          exit
          ( scope
          , EdhArgsPack
            (ArgsPack posArgs $ Map.alter
              (\case -- make sure latest value with same kw take effect
                Nothing       -> Just val
                Just laterVal -> Just laterVal
              )
              kw
              kwArgs
            )
          )
        _ -> error "bug"
 where
  dictKey2Kw :: ItemKey -> STM AttrName
  dictKey2Kw = \case
    ItemByStr name -> return name
    k ->
      throwSTM
        $  EvalError
        $  "Invalid argument keyword from dict key: "
        <> T.pack (show k)


-- | resolve an attribute addressor, either alphanumeric named or symbolic
resolveAddr :: Scope -> AttrAddressor -> STM AttrKey
resolveAddr _ (NamedAttr !attrName) = return (AttrByName attrName)
resolveAddr !scope (SymbolicAttr !symName) =
  resolveEdhCtxAttr scope (AttrByName symName) >>= \case
    Just scope' -> do
      em <- readTVar (scopeEntity scope')
      case Map.lookup (AttrByName symName) em of
        Just (EdhSymbol !symVal) -> return (AttrBySym symVal)
        Just v ->
          throwSTM
            $  EvalError
            $  "Not a symbol: "
            <> T.pack (show v)
            <> " as "
            <> symName
            <> " from "
            <> T.pack (show $ thisObject scope') -- TODO this correct ?
        Nothing -> error "bug in ctx attr resolving"
    Nothing ->
      throwSTM
        $  EvalError
        $  "No symbol named "
        <> T.pack (show symName)
        <> " available"


-- Edh attribute resolution

resolveEdhCtxAttr :: Scope -> AttrKey -> STM (Maybe Scope)
resolveEdhCtxAttr !scope !addr = readTVar ent >>= \em -> if Map.member addr em
  then -- directly present on current scope
       return (Just scope)
  else -- skip searching `this` object here, so within a method procedure you
       -- have to be explicit if intend to address attribute off `this` object,
       -- by writing `this.xxx`
       resolveLexicalAttr (classScope $ objClass this) addr
 where
  !ent  = scopeEntity scope
  !this = thisObject scope

resolveLexicalAttr :: [Scope] -> AttrKey -> STM (Maybe Scope)
resolveLexicalAttr [] _ = return Nothing
resolveLexicalAttr (scope@(Scope !ent !obj) : outerEntities) addr =
  readTVar ent >>= \em -> if Map.member addr em
    then -- directly present on current scope
         return (Just scope)
    else -- go for the interesting attribute from inheritance hierarchy
         -- of this object, so a module can `extends` some objects too,
         -- in addition to the `import` mechanism
         if ent == objEntity obj
      then -- go directly to supers as entity of this object just searched
           resolveEdhSuperAttr (objSupers obj) addr
      else resolveEdhObjAttr obj addr >>= \case
        Just scope'from'object -> return $ Just scope'from'object
        -- go one level outer of the lexical stack
        Nothing                -> resolveLexicalAttr outerEntities addr

resolveEdhObjAttr :: Object -> AttrKey -> STM (Maybe Scope)
resolveEdhObjAttr !this !addr = readTVar thisEnt >>= \em ->
  if Map.member addr em
    then return (Just $ Scope thisEnt this)
    else resolveEdhSuperAttr (objSupers this) addr
  where !thisEnt = objEntity this

resolveEdhSuperAttr :: [Object] -> AttrKey -> STM (Maybe Scope)
resolveEdhSuperAttr [] _ = return Nothing
resolveEdhSuperAttr (super : restSupers) !addr =
  resolveEdhObjAttr super addr >>= \case
    Just scope -> return $ Just scope
    Nothing    -> resolveEdhSuperAttr restSupers addr

