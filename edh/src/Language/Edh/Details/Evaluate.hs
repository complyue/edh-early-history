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
  let !ctx   = edh'context pgs
      !scope = contextScope ctx
  case stmt of

    ExprStmt expr             -> evalExpr expr exit

    LetStmt argsRcvr argsSndr -> undefined

    BreakStmt                 -> exitEdhProc exit (scope, EdhBreak)
    ContinueStmt              -> exitEdhProc exit (scope, EdhContinue)
    FallthroughStmt           -> exitEdhProc exit (scope, EdhFallthrough)
    -- TODO impl. this
    YieldStmt  argsSndr       -> undefined -- EdhYield <$>  
    ReturnStmt expr           -> evalExpr expr
      $ \(!scope', !val) -> exitEdhProc exit (scope', EdhReturn val)

    ImportStmt argsRcvr srcExpr -> case srcExpr of
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

    -- TODO this eval from right to left ? correct it if so
    -- DictExpr ps ->
      -- let
      --   gatherDict
      --     :: (DictStore -> EdhProg ())
      --     -> (Expr, Expr)
      --     -> EdhProg (DictStore -> EdhProg ())
      --   gatherDict exit' (!kExpr, !vExpr) = return $ \ds ->
      --     evalExpr vExpr $ \(!_scope'v, !vVal) ->
      --       evalExpr kExpr $ \(!_scope'k, !kVal) -> do
      --         key <- case kVal of
      --           EdhString  k -> return $ ItemByStr k
      --           EdhSymbol  k -> return $ ItemBySym k
      --           EdhDecimal k -> return $ ItemByNum k
      --           EdhBool    k -> return $ ItemByBool k
      --           k ->
      --             throwEdh
      --               $  EvalError
      --               $  "Invalid key: "
      --               <> T.pack (show k)
      --               <> " ❌"
      --         exit' $ Map.alter
      --           (\case -- give later entries higher priority
      --             Nothing  -> Just vVal
      --             Just val -> Just val
      --           )
      --           key
      --           ds
      -- in
      --   foldM
      --       gatherDict
      --       (\ds ->
      --         (liftIO $ EdhDict . Dict <$> newTVarIO ds) >>= (exit . (scope, ))
      --       )
      --       ps
      --     >>= ($ Map.empty)

    ListExpr xs -> -- list construction runs in an implicit tx
      local (\s -> s { edh'in'tx = True }) $ evalExprs xs $ \(_, tv) ->
        case tv of
          EdhTuple l -> return $ do
            ll <- List <$> newTVar l
            join $ runReaderT (exitEdhProc exit (scope, EdhList ll)) pgs
          _ -> error "bug"

    TupleExpr xs -> -- tuple construction runs in an implicit tx
      local (\s -> s { edh'in'tx = True }) $ evalExprs xs $ \(_, tv) ->
        case tv of
          EdhTuple l -> exitEdhProc exit (scope, EdhTuple l)
          _          -> error "bug"

    ParenExpr x         -> evalExpr x exit

    -- TODO this should check for Thunk, and implement
    --      break/fallthrough semantics
    BlockExpr stmts     -> evalBlock stmts exit

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
          resolveEdhObjAttr (Scope (objEntity obj) obj) key >>= \case
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


-- evalExprToVar :: Context -> Expr -> EdhProg (TMVar (Scope, EdhValue))
-- evalExprToVar ctx expr = do
--   var <- liftIO newEmptyTMVarIO
--   evalExpr ctx expr $ \sv -> liftIO $ atomically $ putTMVar var sv
--   return var


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


-- packEdhArgs
--   :: Context
--   -> (Maybe (TQueue EdhTxOp))
--   -> ArgsSender
--   -> (STM ArgsPack -> EdhProg (STM ()))
--   -> EdhProg (STM ())
-- packEdhArgs argsSender exit = case argsSender of
--   PackSender packSender ->
--     foldM fillPack exit packSender >>= ($ ArgsPack [] Map.empty)
--   SingleSender argSender ->
--     fillPack exit argSender >>= ($ ArgsPack [] Map.empty)
--  where
--   eval' = evalExpr

--   fillPack
--     :: (ArgsPack -> EdhProg (STM ()))
--     -> ArgSender
--     -> STM (ArgsPack -> EdhProg (STM ()))
--   fillPack exit' argSender = return $ \(ArgsPack posArgs kwArgs) ->
--     case argSender of
--       UnpackPosArgs listExpr -> eval' listExpr $ \case
--         (_scope, EdhList (List listVar)) -> do
--           listVal <- readTVar listVar
--           exit' $ ArgsPack (listVal ++ posArgs) kwArgs
--         (_scope, v) ->
--           throwEdh $ EvalError $ "Can not unpack args from: " <> T.pack
--             (show v)
--       UnpackKwArgs dictExpr -> eval' dictExpr $ \case
--         (_scope, EdhDict (Dict dictVar)) -> do
--           dictVal   <- readTVar dictVar
--           kwAscList <- forM (Map.toAscList dictVal)
--             $ \(k, v) -> (, v) <$> dictKey2Kw k
--           -- let kwMap = Map.fromAscList kwAscList
--           -- kwMap   <-
--           --   Map.fromAscList <$> forM (Map.toAscList dictVal) $ \(k, v) ->
--           --     (, v) <$> dictKey2Kw k
--           -- kwArgs appear later, give them higher priority
--           exit'
--             $ ArgsPack posArgs (Map.union kwArgs $ Map.fromAscList kwAscList)
--         (_scope, v) ->
--           throwEdh $ EvalError $ "Can not unpack kwargs from: " <> T.pack
--             (show v)
--       SendPosArg argExpr -> eval' argExpr
--         $ \(_scope, argVal) -> exit' $ ArgsPack (argVal : posArgs) kwArgs
--       SendKwArg kw argExpr -> eval' argExpr $ \(_scope, argVal) ->
--         -- kwArgs appear later, give them higher priority
--         exit' $ ArgsPack posArgs $ Map.alter
--           (\case
--             Nothing  -> Just argVal
--             Just val -> Just val
--           )
--           kw
--           kwArgs

--   dictKey2Kw :: ItemKey -> STM AttrName
--   dictKey2Kw = \case
--     ItemByStr name -> return name
--     k ->
--       throwEdh
--         $  EvalError
--         $  "Invalid argument keyword from dict key: "
--         <> T.pack (show k)


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


resolveEdhCtxAttr :: Scope -> AttrKey -> STM (Maybe Scope)
resolveEdhCtxAttr !scope !addr = readTVar ent >>= \em -> if Map.member addr em
  then return (Just scope)
  else resolveLexicalAttr (classScope $ objClass this) addr
 where
  !ent  = scopeEntity scope
  !this = thisObject scope

resolveLexicalAttr :: [Scope] -> AttrKey -> STM (Maybe Scope)
resolveLexicalAttr [] _ = return Nothing
resolveLexicalAttr (scope@(Scope !ent _) : outerEntities) addr =
  readTVar ent >>= \em -> if Map.member addr em
    then return (Just scope)
    else resolveLexicalAttr outerEntities addr


resolveEdhObjAttr :: Scope -> AttrKey -> STM (Maybe Scope)
resolveEdhObjAttr !scope !addr = readTVar objEnt >>= \em ->
  if Map.member addr em
    then return (Just scope)
    else resolveEdhSuperAttr (objSupers this) addr
 where
  this   = thisObject scope
  objEnt = objEntity this

resolveEdhSuperAttr :: [Object] -> AttrKey -> STM (Maybe Scope)
resolveEdhSuperAttr []                   _     = return Nothing
resolveEdhSuperAttr (super : restSupers) !addr = readTVar objEnt >>= \em ->
  if Map.member addr em
    then return (Just (Scope objEnt super))
    else resolveEdhSuperAttr restSupers addr
  where objEnt = objEntity super

