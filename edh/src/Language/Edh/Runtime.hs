
module Language.Edh.Runtime
    ( runEdhProgram
    , runEdhProgram'
    , evalEdhStmt
    , evalEdhStmt'
    , moduleContext
-- TODO cherrypick what artifacts to export as for user interface
    , module RT
    , module EV
    )
where

import           Prelude

import           Control.Exception
import           Control.Monad.Except

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes  as RT
import           Language.Edh.Details.Evaluate as EV


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

