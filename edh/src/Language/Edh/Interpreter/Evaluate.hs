{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Language.Edh.Interpreter.Evaluate
    ( runEdhProgram
    , evalEdhStmt
    )
where

import           Prelude

import           Control.Exception
import           Control.Monad.Except

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime

import           Language.Edh.Interpreter.Details


runEdhProgram
    :: MonadIO m
    => EdhWorld
    -> Module
    -> SeqStmts
    -> m (Either EvalError EdhValue)
runEdhProgram _     _    []  = return $ Right EdhNil
runEdhProgram world modu [s] = evalEdhStmt world modu s
runEdhProgram world modu (s : rs) =
    evalEdhStmt world modu s *> runEdhProgram world modu rs


evalEdhStmt
    :: MonadIO m
    => EdhWorld
    -> Module
    -> StmtSrc
    -> m (Either EvalError EdhValue)
evalEdhStmt w m s = liftIO $ tryJust Just $ evalStmt w m s

