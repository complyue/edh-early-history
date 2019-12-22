{-# LANGUAGE TupleSections #-}

module Language.Edh.Runtime
  ( createEdhWorld
  , declareEdhOperators
  , putEdhAttrs
  , putEdhAttr
  , runEdhProgram
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
import           Control.Concurrent.STM

import           Data.IORef

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

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
  scope = Scope { scopeEntity = objEntity mo, thisObject = mo }


runEdhProgram' :: Context -> SeqStmts -> IO (Either EvalError EdhValue)
runEdhProgram' _   []    = return $ Right EdhNil
runEdhProgram' ctx stmts = do
  final <- newEmptyTMVarIO
  let finalize (_scope, val) = do
        liftIO $ atomically $ putTMVar final val
  tryJust Just
    $  runEdhProg (evalStmts stmts finalize)
    >> (atomically $ readTMVar final)

 where

  evalStmts :: SeqStmts -> ((Scope, EdhValue) -> EdhProg ()) -> EdhProg ()
  evalStmts []       exit = exit (contextScope ctx, nil)
  evalStmts [s     ] exit = evalStmt ctx s exit
  evalStmts (s : rs) exit = evalStmt ctx s (\_ -> evalStmts rs exit)


createEdhWorld :: MonadIO m => m EdhWorld
createEdhWorld = liftIO $ do
  worldEntity <- newTVarIO Map.empty
  let
    !srcPos = SourcePos { sourceName   = "<Genesis>"
                        , sourceLine   = mkPos 1
                        , sourceColumn = mkPos 1
                        }
    !worldClass = Class
      { classScope     = []
      , className      = "<world>"
      , classSourcePos = srcPos
      , classProcedure = ProcDecl { procedure'args = WildReceiver
                                  , procedure'body = StmtSrc (srcPos, VoidStmt)
                                  }
      }
    !root =
      Object { objEntity = worldEntity, objClass = worldClass, objSupers = [] }
  opPD  <- newIORef Map.empty
  modus <- newIORef Map.empty
  return $ EdhWorld
    { worldRoot      = root
    , moduleClass    =
      Class
        { classScope     = [Scope worldEntity root]
        , className      = "<module>"
        , classSourcePos = srcPos
        , classProcedure = ProcDecl
                             { procedure'args = WildReceiver
                             , procedure'body = StmtSrc (srcPos, VoidStmt)
                             }
        }
    , worldOperators = opPD
    , worldModules   = modus
    }


declareEdhOperators
  :: MonadIO m => EdhWorld -> Text -> [(OpSymbol, Precedence)] -> m ()
declareEdhOperators world declLoc opps = liftIO
  $ atomicModifyIORef' (worldOperators world) declarePrecedence
 where
  declarePrecedence :: OpPrecDict -> (OpPrecDict, ())
  declarePrecedence opPD =
    (, ())
      $ Map.unionWithKey chkCompatible opPD
      $ Map.fromList
      $ flip map opps
      $ \(op, p) -> (op, (p, declLoc))
  chkCompatible
    :: OpSymbol
    -> (Precedence, Text)
    -> (Precedence, Text)
    -> (Precedence, Text)
  chkCompatible op (prevPrec, prevDeclLoc) (newPrec, newDeclLoc) =
    if prevPrec /= newPrec
      then throw $ EvalError
        (  "precedence change from "
        <> T.pack (show prevPrec)
        <> " (declared "
        <> prevDeclLoc
        <> ") to "
        <> T.pack (show newPrec)
        <> " (declared "
        <> T.pack (show newDeclLoc)
        <> ") for operator: "
        <> op
        )
      else (prevPrec, prevDeclLoc)


putEdhAttrs :: Entity -> [(AttrKey, EdhValue)] -> STM ()
putEdhAttrs e as = modifyTVar' e $ \em -> Map.union ad em
  where ad = Map.fromList as

putEdhAttr :: Entity -> AttrKey -> EdhValue -> STM ()
putEdhAttr e k v = modifyTVar' e $ \em -> Map.insert k v em

