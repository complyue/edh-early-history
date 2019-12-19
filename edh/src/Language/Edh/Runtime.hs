
module Language.Edh.Runtime
  ( runEdhProgram
  , runEdhProgram'
  , moduleContext
-- TODO cherrypick what artifacts to export as for user interface
  , module RT
  , module TX
  , module EV
  )
where

import           Prelude

import           Control.Exception
import           Control.Monad.Except
import           Control.Concurrent

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes  as RT
import           Language.Edh.Details.Tx       as TX
import           Language.Edh.Details.Evaluate as EV


runEdhProgram
  :: MonadIO m
  => EdhWorld
  -> Module
  -> SeqStmts
  -> m (Either EvalError EdhValue)
runEdhProgram w m rs = liftIO $ runEdhProgram' ctx rs
  where ctx = moduleContext w m


moduleContext :: EdhWorld -> Module -> Context
moduleContext w m = Context { contextWorld = w
                            , contextModu  = m
                            , contextScope = scope
                            }
 where
  mo    = moduleObject m
  scope = Scope { scopeStack = objEntity mo : (classScope . objClass) mo
                , thisObject = mo
                }


runEdhProgram' :: Context -> SeqStmts -> IO (Either EvalError EdhValue)
runEdhProgram' _   []    = return $ Right EdhNil
runEdhProgram' ctx stmts = do
  halt  <- newEmptyMVar
  final <- newEmptyMVar
  let finalize v = liftIO $ do
        putMVar halt  ()
        putMVar final v
  tryJust Just (runEdhProg halt (ensureHalt stmts finalize) >> readMVar final)

 where

  ensureHalt :: SeqStmts -> (EdhValue -> EdhProg ()) -> EdhProg ()
  ensureHalt stmts' fin = do
    evalStmts stmts' fin
    -- fire an empty transaction, making sure the tx driver check halt
    runEdhTx [] []

  evalStmts :: SeqStmts -> (EdhValue -> EdhProg ()) -> EdhProg ()
  evalStmts []       exit = exit nil
  evalStmts [s     ] exit = evalStmt ctx s exit
  evalStmts (s : rs) exit = evalStmt ctx s (\_ -> evalStmts rs exit)

