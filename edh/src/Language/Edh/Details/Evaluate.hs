{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Language.Edh.Details.Evaluate where

import           Prelude

import           Control.Exception
import           Control.Monad.Except

import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes
import           Language.Edh.Details.Utils


evalStmt :: Context -> StmtSrc -> IO EdhValue
evalStmt ctx (StmtSrc (srcPos, stmt)) =
    handleJust
            Just
            (\(EvalError msg) -> throwIO $ EvalError $ msg <> "\nℹ️ " <> T.pack
                (sourcePosPretty srcPos)
            )
        $ evalStmt' ctx stmt


evalStmt' :: Context -> Stmt -> IO EdhValue
evalStmt' ctx stmt = case stmt of

    ExprStmt expr         -> evalExpr ctx expr

    BreakStmt             -> return EdhBreak
    ContinueStmt          -> return EdhContinue
    FallthroughStmt       -> return EdhFallthrough
    -- TODO impl. this
    YieldStmt  asend      -> undefined -- EdhYield <$>  
    ReturnStmt expr       -> EdhReturn <$> evalExpr ctx expr


    ImportStmt ar srcExpr -> case srcExpr of
        LitExpr (StringLiteral moduPath) ->
            return $ EdhString $ "wana import " <> moduPath <> ".edh huh?"
        expr -> throwIO $ EvalError $ "don't know how to import " <> T.pack
            (show expr)


    VoidStmt -> return nil
    _ ->
        throwIO $ EvalError $ "Eval not yet impl for: " <> (T.pack $ show stmt)


evalExpr :: Context -> Expr -> IO EdhValue
evalExpr ctx expr = liftIO $ case expr of
    LitExpr lit -> case lit of
        DecLiteral    v -> return $ EdhDecimal v
        StringLiteral v -> return $ EdhString v
        BoolLiteral   v -> return $ EdhBool v
        NilLiteral      -> return nil
        TypeLiteral v   -> return $ EdhType v
        -- TODO impl this
        SinkCtor        -> throwIO $ EvalError "sink ctor not impl. yet"

    PrefixExpr prefix expr' -> case prefix of
        PrefixPlus  -> eval' expr'
        PrefixMinus -> eval' expr' >>= \case
            EdhDecimal v -> return $ EdhDecimal (-v)
            v ->
                throwIO
                    $  EvalError
                    $  "Can not negate: "
                    <> T.pack (show v)
                    <> " ❌"
        Not -> eval' expr' >>= \case
            EdhBool v -> return $ EdhBool $ not v
            v ->
                throwIO
                    $  EvalError
                    $  "Expect bool but got: "
                    <> T.pack (show v)
                    <> " ❌"

        -- TODO this should probably create Thunk instead, but mind to
        --      cooperate with the branch operator (->), find a way to
        --      tell it that this is guarded value, don't compare with
        --      thunk target value. but how ?
        Guard -> eval' expr'

        -- TODO impl these
        Go    -> throwIO $ EvalError "goroutine starter not impl. yet"
        Defer -> throwIO $ EvalError "defer scheduler not impl. yet"

    IfExpr cond cseq alt -> eval' cond >>= \case
        EdhBool True  -> evalSS cseq
        EdhBool False -> case alt of
            Just elseClause -> evalSS elseClause
            _               -> return nil
        v ->
            throwIO -- we are so strongly typed
                $  EvalError
                $  "Not a boolean value: "
                <> T.pack (show v)
                <> " ❌"

    DictExpr ps ->
        let
            evalPair :: (Expr, Expr) -> IO (ItemKey, EdhValue)
            evalPair (kExpr, vExpr) = do
                v <- eval' vExpr
                eval' kExpr >>= \case
                    EdhString  k -> return (ItemByStr k, v)
                    EdhSymbol  k -> return (ItemBySym k, v)
                    EdhDecimal k -> return (ItemByNum k, v)
                    EdhBool    k -> return (ItemByBool k, v)
                    k ->
                        throwIO
                            $  EvalError
                            $  "Invalid key: "
                            <> T.pack (show k)
                            <> " ❌"
        in
            do
                pl <- mapM evalPair ps
                EdhDict <$> newIORef (Dict $ Map.fromList pl)

    ListExpr vs -> do
        l <- mapM eval' vs
        EdhList <$> newIORef l

    TupleExpr vs        -> EdhTuple <$> mapM eval' vs

    -- TODO this should check for Thunk, and implement
    --      break/fallthrough semantics
    BlockExpr stmts     -> return $ EdhBlock stmts

    -- TODO impl this
    ForExpr ar iter act -> undefined

    GeneratorExpr sp pd -> return $ EdhGenrDef $ GenrDef
        { generatorOwnerObject = this
        , generatorSourcePos   = sp
        , generatorProcedure   = pd
        }

    -- AttrExpr addr -> 
    -- IndexExpr ixExpr tgtExpr ->

    CallExpr procExpr args -> eval' procExpr >>= \case
        -- EdhClass classDef -> 
        -- EdhMethod mthExpr -> 
        -- EdhGenrDef genrDef ->

        v ->
            throwIO
                $  EvalError
                $  "Can not call: "
                <> T.pack (show v)
                <> " ❌ expressed with: "
                <> T.pack (show procExpr)


    -- InfixExpr op lhExpr rhExpr -> 

    _ -> throwIO $ EvalError $ "Eval not yet impl for: " <> T.pack (show expr)
  where
    this   = thisObject scope
    scope  = contextScope ctx

    eval'  = evalExpr ctx
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

recvEdhArgs :: ArgsPack -> Context -> ArgsReceiver -> IO Entity
recvEdhArgs pck@(ArgsPack posArgs kwArgs) ctx argsRcvr = case argsRcvr of
    PackReceiver argRcvrs -> do
        (pck', attrs) <- foldM (recvFromPack ctx) (pck, Map.empty) argRcvrs
        woResidual pck' attrs
    SingleReceiver argRcvr -> do
        (pck', attrs) <- recvFromPack ctx (pck, Map.empty) argRcvr
        woResidual pck' attrs
    WildReceiver -> if Prelude.null posArgs
        then newIORef $ Map.mapKeys AttrByName kwArgs
        else
            throwIO
            $  EvalError
            $  "Unexpected "
            <> T.pack (show $ Prelude.length posArgs)
            <> " positional argument(s) to wild receiver"
  where
    scope = contextScope ctx
    eval' = evalExpr ctx

    woResidual :: ArgsPack -> Map.Map AttrKey EdhValue -> IO Entity
    woResidual (ArgsPack posResidual kwResidual) attrs
        | not (Prelude.null posResidual)
        = throwIO
            $  EvalError
            $  "Extraneous "
            <> T.pack (show $ Prelude.length posResidual)
            <> " positional argument(s)"
        | not (Map.null kwResidual)
        = throwIO $ EvalError $ "Extraneous keyword arguments: " <> T.pack
            (show $ Map.keys kwResidual)
        | otherwise
        = newIORef attrs
    recvFromPack
        :: Context
        -> (ArgsPack, Map.Map AttrKey EdhValue)
        -> ArgReceiver
        -> IO (ArgsPack, Map.Map AttrKey EdhValue)
    recvFromPack ctx ((ArgsPack posArgs' kwArgs'), attrs) argRcvr =
        case argRcvr of
            RecvRestPosArgs restPosArgAttr -> do
                argsList <- newIORef posArgs'
                return
                    ( ArgsPack [] kwArgs'
                    , Map.insert (AttrByName restPosArgAttr)
                                 (EdhList argsList)
                                 attrs
                    )
            RecvRestKwArgs restKwArgAttr -> do
                argsDict <- newIORef $ Dict $ Map.mapKeys ItemByStr kwArgs'
                return
                    ( ArgsPack posArgs' Map.empty
                    , Map.insert (AttrByName restKwArgAttr)
                                 (EdhDict argsDict)
                                 attrs
                    )
            RecvArg argName argTgtAddr argDefault -> do
                (argVal, posArgs'', kwArgs'') <- resolveArgValue argName
                                                                 argDefault
                case argTgtAddr of
                    Nothing -> do
                        return
                            ( ArgsPack posArgs'' kwArgs''
                            , Map.insert (AttrByName argName) argVal attrs
                            )
                    Just (DirectRef addr) -> case addr of
                        NamedAttr attrName -> -- simple rename
                                              return
                            ( ArgsPack posArgs'' kwArgs''
                            , Map.insert (AttrByName argName) argVal attrs
                            )
                        SymbolicAttr symName -> -- todo support this ?
                            throwIO
                                $ EvalError
                                      "arg renaming to symbolic attr not supported"
                    Just (IndirectRef addrExpr addr) ->
                        -- TODO impl. this
                        throwIO $ EvalError "arg retargeting not impl. yet"
                    tgt ->
                        throwIO
                            $  EvalError
                            $  "Invalid argument retarget: "
                            <> T.pack (show tgt)
      where
        resolveArgValue
            :: AttrName
            -> Maybe Expr
            -> IO (EdhValue, [EdhValue], Map.Map AttrName EdhValue)
        resolveArgValue argName argDefault = do
            let (inKwArgs, kwArgs'') = takeFromMapByKey argName kwArgs'
            case inKwArgs of
                Nothing -> case posArgs' of
                    (posArg : posArgs'') ->
                        return (posArg, posArgs'', kwArgs'')
                    [] -> case argDefault of
                        Nothing ->
                            throwIO
                                $  EvalError
                                $  "Missing argument: "
                                <> argName
                        Just defaultExpr -> do
                            defaultVal <- eval' defaultExpr
                            return (defaultVal, posArgs', kwArgs'')
                Just argVal -> return (argVal, posArgs', kwArgs'')


-- TODO load object attrs in transactional ways using EdhAddrTx
packEdhArgs :: Context -> ArgsSender -> IO ArgsPack
packEdhArgs ctx argsSender = case argsSender of
    PackSender packSender -> do
        (ArgsPack posArgs kwArgs) <- foldM fillPack
                                           (ArgsPack [] Map.empty)
                                           packSender
        -- fillPack have posArgs filled right-to-left, reverse needed
        return $ ArgsPack (Prelude.reverse posArgs) kwArgs
    SingleSender argSender -> fillPack (ArgsPack [] Map.empty) argSender
  where

    fillPack :: ArgsPack -> ArgSender -> IO ArgsPack
    fillPack (ArgsPack posArgs kwArgs) argSender = case argSender of
        UnpackPosArgs listExpr -> eval' listExpr >>= \case
            EdhList listRef -> do
                listVal <- readIORef listRef
                return $ ArgsPack (posArgs ++ listVal) kwArgs
            v ->
                throwIO $ EvalError $ "Can not unpack posargs from: " <> T.pack
                    (show v)
        UnpackKwArgs dictExpr -> eval' dictExpr >>= \case
            EdhDict dictRef -> do
                Dict dictVal <- readIORef dictRef
                return $ ArgsPack
                    posArgs
                    (Map.union (Map.mapKeys dictKey2Kw dictVal) kwArgs)
            v -> throwIO $ EvalError $ "Can not unpack kwargs from: " <> T.pack
                (show v)
        SendPosArg argExpr -> do
            argVal <- eval' argExpr
            return $ ArgsPack (argVal : posArgs) kwArgs
        SendKwArg kw argExpr -> do
            argVal <- eval' argExpr
            return $ ArgsPack posArgs $ Map.insert kw argVal kwArgs

    dictKey2Kw :: ItemKey -> AttrName
    dictKey2Kw = \case
        ItemByStr name -> name
        k ->
            unsafePerformIO
                $  throwIO
                $  EvalError
                $  "Invalid argument keyword from dict key: "
                <> T.pack (show k)

    eval' = evalExpr ctx


resolveLexicalAttr :: [Entity] -> AttrName -> IO (Maybe EdhValue)
resolveLexicalAttr []                    _        = return Nothing
resolveLexicalAttr (ent : outerEntities) attrName = readIORef ent >>= \em ->
    case Map.lookup (AttrByName attrName) em of
        Just v  -> return $ Just v
        Nothing -> resolveLexicalAttr outerEntities attrName

resolveEdhSuperAttr :: [Object] -> AttrName -> IO (Maybe EdhValue)
resolveEdhSuperAttr []                   _    = return Nothing
resolveEdhSuperAttr (super : restSupers) attr = do
    ent <- readIORef $ objEntity super
    case Map.lookup (AttrByName attr) ent of
        Just v  -> return $ Just v
        Nothing -> resolveEdhSuperAttr restSupers attr

resolveEdhObjAttr :: Scope -> AttrName -> IO (Maybe EdhValue)
resolveEdhObjAttr scope attr = do
    ent <- readIORef $ objEntity obj
    case Map.lookup (AttrByName attr) ent of
        Just v  -> return $ Just v
        Nothing -> resolveEdhSuperAttr (objSupers obj) attr >>= \case
            Just v  -> return $ Just v
            Nothing -> resolveLexicalAttr (scopeStack scope) attr
    where obj = thisObject scope


-- | A transaction for grouped reads to address attributes off entities
type EdhAddrTx = [(Entity, [(AttrKey, (AttrKey -> EdhValue -> IO ()))])]


-- | A transaction for grouped assignments of attributes onto entities
--
-- TODO a list here should not scale well to large transactions, but
--      can't use Entity (IORef per se) as Map key, need more tweaks.
type EdhAssignTx = [(Entity, IORef [(AttrKey, EdhValue)])]

prepareEdhAssign
    :: Context -> EdhAssignTx -> AttrAddressor -> EdhValue -> IO EdhAssignTx
prepareEdhAssign ctx tx addr v = do
    k <- resolveAddr addr
    sched2Tx (k, v) tx
  where
    ent   = objEntity $ thisObject scope
    scope = contextScope ctx

    sched2Tx :: (AttrKey, EdhValue) -> EdhAssignTx -> IO EdhAssignTx
    sched2Tx u [] = do
        lr <- newIORef [u]
        return $ (ent, lr) : tx
    sched2Tx u ((e, lr) : rest) = if e /= ent
        then sched2Tx u rest
        else do
            modifyIORef' lr (\l -> u : l)
            return tx

    resolveAddr :: AttrAddressor -> IO AttrKey
    resolveAddr (NamedAttr attrName) = return $ AttrByName attrName
    resolveAddr (SymbolicAttr symName) =
        resolveEdhObjAttr scope symName >>= \case
            Just (EdhSymbol symVal) -> return $ AttrBySym symVal
            Nothing ->
                throwIO
                    $  EvalError
                    $  "No symbol named "
                    <> T.pack (show symName)
                    <> " available"
            Just v ->
                throwIO
                    $  EvalError
                    $  "Expect a symbol named "
                    <> T.pack (show symName)
                    <> " but got: "
                    <> T.pack (show v)




