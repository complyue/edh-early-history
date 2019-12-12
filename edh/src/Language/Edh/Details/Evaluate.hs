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


evalStmt :: MonadIO m => Context -> StmtSrc -> m EdhValue
evalStmt ctx (srcPos, stmt) =
    liftIO
        $ handleJust
              Just
              (\(EvalError msg) ->
                  throwIO $ EvalError $ msg <> "\nℹ️ " <> T.pack
                      (sourcePosPretty srcPos)
              )
        $ evalStmt' ctx stmt


evalStmt' :: Context -> Stmt -> IO EdhValue
evalStmt' ctx stmt = case stmt of

    ExprStmt expr         -> evalExpr ctx expr

    BreakStmt             -> return EdhBreak
    ContinueStmt          -> return EdhContinue
    FallthroughStmt       -> return EdhFallthrough
    YieldStmt  expr       -> EdhYield <$> evalExpr ctx expr
    ReturnStmt expr       -> EdhReturn <$> evalExpr ctx expr


    ImportStmt ar srcExpr -> case srcExpr of
        LitExpr (StringLiteral moduPath) ->
            return $ EdhString $ "wana import " <> moduPath <> ".edh huh?"
        expr -> throwIO $ EvalError $ "don't know how to import " <> T.pack
            (show expr)


    VoidStmt -> return nil
    _ ->
        throwIO $ EvalError $ "Eval not yet impl for: " <> (T.pack $ show stmt)


evalExpr :: MonadIO m => Context -> Expr -> m EdhValue
evalExpr ctx expr = liftIO $ evalExpr' ctx expr

evalExpr' :: Context -> Expr -> IO EdhValue
evalExpr' ctx expr = liftIO $ case expr of
    LitExpr lit -> case lit of
        DecLiteral    v -> return $ EdhDecimal v
        StringLiteral v -> return $ EdhString v
        BoolLiteral   v -> return $ EdhBool v
        NilLiteral      -> return nil
        TypeLiteral v   -> return $ EdhType v
        -- TODO impl this
        ChanCtor        -> throwIO $ EvalError "channel ctor not impl. yet"

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

        -- TODO this impl correct ?
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
            mapM evalPair ps >>= (return . EdhDict . Dict . Map.fromList)

    ListExpr  vs        -> EdhList <$> mapM eval' vs
    TupleExpr vs        -> EdhTuple <$> mapM eval' vs

    SequeExpr vs        -> EdhSeque <$> mapM evalSS vs

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

    eval'  = evalExpr' ctx
    evalSS = evalStmt ctx


-- | The Edh call convention is so called call-by-entity, i.e.  a new
-- entity is created with its attributes filled according to a pair of
-- manifestations for argument sending and receiving respectively.
makeEdhCall :: Context -> ArgsSender -> Scope -> ArgsReceiver -> IO Entity
makeEdhCall callerCtx asend calleeScp arecv = do
    undefined


runEdhProgram
    :: MonadIO m
    => EdhWorld
    -> Module
    -> SeqStmts
    -> m (Either EvalError EdhValue)
runEdhProgram w m rs = liftIO $ runEdhProgram' ctx rs
    where ctx = moduleContext w m


evalEdhStmt
    :: MonadIO m
    => EdhWorld
    -> Module
    -> StmtSrc
    -> m (Either EvalError EdhValue)
evalEdhStmt w m s = liftIO $ evalEdhStmt' ctx s where ctx = moduleContext w m


moduleContext :: EdhWorld -> Module -> Context
moduleContext w m = ctx
  where
    mo    = moduleObject m
    scope = Scope { scopeStack = objEntity mo : (classScope . objClass) mo
                  , thisObject = mo
                  }
    ctx = Context { contextWorld = w, contextModu = m, contextScope = scope }


runEdhProgram' :: Context -> SeqStmts -> IO (Either EvalError EdhValue)
runEdhProgram' _   []       = return $ Right EdhNil
runEdhProgram' ctx [s     ] = evalEdhStmt' ctx s
runEdhProgram' ctx (s : rs) = evalEdhStmt' ctx s *> runEdhProgram' ctx rs


evalEdhStmt' :: Context -> StmtSrc -> IO (Either EvalError EdhValue)
evalEdhStmt' ctx s = tryJust Just $ evalStmt ctx s

