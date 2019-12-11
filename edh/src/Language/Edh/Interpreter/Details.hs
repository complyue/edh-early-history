{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Language.Edh.Interpreter.Details where

import           Prelude

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict

import           Data.Typeable
import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Data.Lossless.Decimal         as D

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


evalStmt :: MonadIO m => EdhWorld -> Module -> StmtSrc -> m EdhValue
evalStmt world modu (srcPos, stmt) =
    liftIO
        $ handleJust
              Just
              (\(EvalError msg) ->
                  throwIO $ EvalError $ msg <> "\nℹ️ " <> T.pack
                      (sourcePosPretty srcPos)
              )
        $ evalStmt' world modu stmt

evalExpr :: MonadIO m => EdhWorld -> Module -> Expr -> m EdhValue
evalExpr world modu expr = liftIO $ evalExpr' world modu expr


evalStmt' :: EdhWorld -> Module -> Stmt -> IO EdhValue
evalStmt' world modu stmt = case stmt of

    ExprStmt expr         -> evalExpr world modu expr

    ImportStmt ar srcExpr -> case srcExpr of
        LitExpr (StringLiteral moduPath) ->
            return $ EdhString $ "wana import " <> moduPath <> ".edh huh?"
        expr -> throwIO $ EvalError $ "don't know how to import " <> T.pack
            (show expr)


    VoidStmt -> return nil
    _ ->
        throwIO $ EvalError $ "Eval not yet impl for: " <> (T.pack $ show stmt)


evalExpr' :: EdhWorld -> Module -> Expr -> IO EdhValue
evalExpr' world modu expr = liftIO $ case expr of
    LitExpr lit -> case lit of
        DecLiteral    v -> return $ EdhDecimal v
        StringLiteral v -> return $ EdhString v
        BoolLiteral   v -> return $ EdhBool v
        NilLiteral      -> return nil
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

        Go    -> throwIO $ EvalError "goroutine starter not impl. yet"
        Defer -> throwIO $ EvalError "defer scheduler not impl. yet"

    IfExpr cond cseq alt -> eval' cond >>= \case
        EdhBool True  -> evalStmt world modu cseq
        EdhBool False -> case alt of
            Just elseClause -> evalStmt world modu elseClause
            _               -> return nil
        v ->
            throwIO
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


    _ -> throwIO $ EvalError $ "Eval not yet impl for: " <> T.pack (show expr)
    where eval' = evalExpr' world modu



