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

