{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.Edh.Runtime
  ( createEdhWorld
  , declareEdhOperators
  , installEdhAttrs
  , installEdhAttr
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

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map
import           Data.List.NonEmpty             ( NonEmpty(..) )

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
runEdhProgram !w !m !rs = liftIO $ runEdhProgram' ctx rs
  where !ctx = moduleContext w m


moduleContext :: EdhWorld -> Module -> Context
moduleContext !w !m = Context
  { contextWorld = w
  , contextStack = moduScope :| [rootScope]
  , contextStmt  = StmtSrc (_genesisSourcePosition, VoidStmt)
  }
 where
  !mo        = moduleObject m
  !moduScope = Scope (objEntity mo) mo (classProcedure $ moduleClass w)
  !root      = worldRoot w
  !rootScope = Scope (objEntity root) root (classProcedure $ objClass root)


runEdhProgram' :: Context -> SeqStmts -> IO (Either EvalError EdhValue)
runEdhProgram' _    []     = return $ Right EdhNil
runEdhProgram' !ctx !stmts = do
  !final <- newEmptyTMVarIO
  let
    !scope@(Scope _ this _) = contextScope ctx
    wrapper :: EdhProg (STM ())
    wrapper = do
      let evalStmts :: SeqStmts -> EdhProcExit -> EdhProg (STM ())
          evalStmts []       exit = exit (this, scope, nil)
          evalStmts [s     ] exit = evalStmt this s exit
          evalStmts (s : rs) exit = evalStmt this s (const $ evalStmts rs exit)
      evalStmts stmts $ \(_, _, !val) -> return $ putTMVar final val
  tryJust Just $ do
    driveEdhProg ctx wrapper
    atomically $ readTMVar final


createEdhWorld :: MonadIO m => m EdhWorld
createEdhWorld = liftIO $ do
  -- ultimate default methods/operators/values go into this
  worldEntity      <- newTVarIO Map.empty
  -- methods supporting reflected module manipulation go into this
  moduManiMethods  <- newTVarIO Map.empty
  -- methods supporting reflected scope manipulation go into this
  scopeManiMethods <- newTVarIO Map.empty
  rootSupers       <- newTVarIO []
  let
    !worldClass = Class
      { classContext   = []
      , classProcedure =
        ProcDecl { procedure'name = "<world>"
                 , procedure'args = WildReceiver
                 , procedure'body = StmtSrc (_genesisSourcePosition, VoidStmt)
                 }
      }
    !moduClassProc = ProcDecl
      { procedure'name = "<module>"
      , procedure'args = WildReceiver
      , procedure'body = StmtSrc (_genesisSourcePosition, VoidStmt)
      }
    !scopeClassProc = ProcDecl
      { procedure'name = "<scope>"
      , procedure'args = WildReceiver
      , procedure'body = StmtSrc (_genesisSourcePosition, VoidStmt)
      }
    !root = Object { objEntity = worldEntity
                   , objClass  = worldClass
                   , objSupers = rootSupers
                   }
  opPD  <- newTMVarIO Map.empty
  modus <- newTVarIO Map.empty
  return $ EdhWorld
    { worldRoot      = root
    , moduleClass    = Class
                         { classContext   = [ Scope moduManiMethods
                                                    root
                                                    moduClassProc
                                            ]
                         , classProcedure = moduClassProc
                         }
    , scopeClass     = Class
                         { classContext   = [ Scope scopeManiMethods
                                                    root
                                                    scopeClassProc
                                            ]
                         , classProcedure = scopeClassProc
                         }
    , worldOperators = opPD
    , worldModules   = modus
    }


declareEdhOperators :: EdhWorld -> Text -> [(OpSymbol, Precedence)] -> STM ()
declareEdhOperators world declLoc opps = do
  opPD <- takeTMVar wops
  catchSTM (declarePrecedence opPD)
    $ \(e :: SomeException) -> tryPutTMVar wops opPD >> throwSTM e
 where
  !wops = worldOperators world
  declarePrecedence :: OpPrecDict -> STM ()
  declarePrecedence opPD = do
    opPD' <-
      sequence
      $ Map.unionWithKey chkCompatible (return <$> opPD)
      $ Map.fromList
      $ (<$> opps)
      $ \(op, p) -> (op, return (p, declLoc))
    putTMVar wops opPD'
  chkCompatible
    :: OpSymbol
    -> STM (Precedence, Text)
    -> STM (Precedence, Text)
    -> STM (Precedence, Text)
  chkCompatible op prev newly = do
    (prevPrec, prevDeclLoc) <- prev
    (newPrec , newDeclLoc ) <- newly
    if prevPrec /= newPrec
      then throwSTM $ UsageError
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
      else return (prevPrec, prevDeclLoc)


installEdhAttrs :: Entity -> [(AttrKey, EdhValue)] -> STM ()
installEdhAttrs e as = modifyTVar' e $ \em -> Map.union ad em
  where ad = Map.fromList as

installEdhAttr :: Entity -> AttrKey -> EdhValue -> STM ()
installEdhAttr e k v = modifyTVar' e $ \em -> Map.insert k v em


_genesisSourcePosition :: SourcePos
_genesisSourcePosition = SourcePos { sourceName   = "<Genesis>"
                                   , sourceLine   = mkPos 1
                                   , sourceColumn = mkPos 1
                                   }
