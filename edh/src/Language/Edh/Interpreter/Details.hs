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
evalStmt' world modu stmt = liftIO $ do

    Prelude.putStrLn $ show stmt

    case stmt of

        VoidStmt              -> return $ nil

        ImportStmt ar srcExpr -> case srcExpr of
            LitExpr (StringLiteral moduPath) ->
                return $ EdhString $ "wana import " <> moduPath <> ".edh huh?"
            expr -> throwIO $ EvalError $ "don't know how to import " <> T.pack
                (show expr)


        _ -> throwIO $ EvalError "not impl."


