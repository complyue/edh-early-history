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

evalStmt' :: MonadIO m => EdhWorld -> Module -> Stmt -> m EdhValue
evalStmt' world modu stmt = liftIO $ case stmt of

    ExprStmt expr         -> evalExpr world modu expr

    ImportStmt ar srcExpr -> case srcExpr of
        LitExpr (StringLiteral moduPath) ->
            return $ EdhString $ "wana import " <> moduPath <> ".edh huh?"
        expr -> throwIO $ EvalError $ "don't know how to import " <> T.pack
            (show expr)


    VoidStmt -> return nil
    _ ->
        throwIO $ EvalError $ "Eval not yet impl for: " <> (T.pack $ show stmt)


evalExpr :: MonadIO m => EdhWorld -> Module -> Expr -> m EdhValue
evalExpr world modu expr = liftIO $ case expr of
    LitExpr lit -> case lit of
        DecLiteral    v -> return $ EdhDecimal v
        StringLiteral v -> return $ EdhString v
        BoolLiteral   v -> return $ EdhBool v
        NilLiteral      -> return nil
        ChanCtor        -> throwIO $ EvalError "channel ctor not impl. yet"

    PrefixExpr prefix expr' -> case prefix of
        PrefixPlus  -> evalExpr world modu expr'
        PrefixMinus -> evalExpr world modu expr' >>= \case
            EdhDecimal v -> return $ EdhDecimal (-v)
            v ->
                throwIO
                    $  EvalError
                    $  "Can not negate: "
                    <> T.pack (show v)
                    <> " ❌"
        Not -> evalExpr world modu expr' >>= \case
            EdhBool v -> return $ EdhBool $ not v
            v ->
                throwIO
                    $  EvalError
                    $  "Expect bool but got: "
                    <> T.pack (show v)
                    <> " ❌"

        Go    -> throwIO $ EvalError "goroutine starter not impl. yet"
        Defer -> throwIO $ EvalError "defer scheduler not impl. yet"

    IfExpr cond cseq alt -> evalExpr world modu cond >>= \case
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


    _ -> throwIO $ EvalError $ "Eval not yet impl for: " <> T.pack (show expr)

