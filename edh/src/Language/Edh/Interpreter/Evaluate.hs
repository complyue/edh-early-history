{-# LANGUAGE LambdaCase #-}

module Language.Edh.Interpreter.Evaluate where

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
import           Language.Edh.Parser


runEdhProgram
    :: MonadIO m
    => EdhWorld
    -> Module
    -> SeqStmts
    -> m (Either EvalError EdhValue)
runEdhProgram _     _    []          = return $ Right EdhNil
runEdhProgram world modu (stmt : rs) = liftIO $ do

    Prelude.putStrLn $ show stmt

    evalEdhStmt world modu stmt >>= \case
        Left  err -> return $ Left err
        Right _   -> runEdhProgram world modu rs


evalEdhStmt
    :: MonadIO m
    => EdhWorld
    -> Module
    -> StmtSrc
    -> m (Either EvalError EdhValue)
evalEdhStmt world modu (_srcPos, stmt) = case stmt of

    VoidStmt -> return $ Right nil

    _        -> return $ Left $ EvalError "not impl."


