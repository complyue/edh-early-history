{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Runtime
  ( createEdhWorld
  , declareEdhOperators
  , installEdhAttrs
  , installEdhAttr
  , runEdhProgram
  , runEdhProgram'
  , moduleContext
  , voidStatement
  , mkHostProc
  , mkHostOper
-- TODO cherrypick what artifacts to export as for user interface
  , module RT
  , module TX
  , module EV
  )
where

import           Prelude

import           System.IO
import           GHC.Conc                       ( unsafeIOToSTM )

import           Control.Exception
import           Control.Monad.Except

import           Control.Concurrent.STM

import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           System.Mem.Weak

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map
import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , (<|)
                                                )
import qualified Data.List.NonEmpty            as NE

import           Text.Megaparsec

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes  as RT
import           Language.Edh.Details.Tx       as TX
import           Language.Edh.Details.Evaluate as EV


runEdhProgram
  :: MonadIO m
  => EdhWorld
  -> Object
  -> SeqStmts
  -> m (Either EvalError EdhValue)
runEdhProgram !w !m !rs = liftIO $ runEdhProgram' ctx rs
  where !ctx = moduleContext w m


moduleContext :: EdhWorld -> Object -> Context
moduleContext !w !mo = Context { contextWorld    = w
                               , callStack       = moduScope <| rootScope
                               , generatorCaller = Nothing
                               , contextMatch    = true
                               , contextStmt     = voidStatement
                               }
 where
  !moduScope = Scope (objEntity mo)
                     mo
                     (NE.toList rootScope)
                     (classProcedure $ moduleClass w)
  !rootScope = (classLexiStack $ moduleClass w)


runEdhProgram' :: Context -> SeqStmts -> IO (Either EvalError EdhValue)
runEdhProgram' _    []     = return $ Right EdhNil
runEdhProgram' !ctx !stmts = do
  !final <- newEmptyTMVarIO
  let !(Scope _ !this _ _) = contextScope ctx
  tryJust Just $ do
    driveEdhProg ctx $ evalBlock this stmts $ \(_, _, !val) ->
      return $ putTMVar final val
    atomically $ readTMVar final


createEdhWorld :: MonadIO m => m EdhWorld
createEdhWorld = liftIO $ do
  -- ultimate default methods/operators/values go into this
  worldEntity      <- newTVarIO Map.empty
  -- methods supporting reflected scope manipulation go into this
  scopeManiMethods <- newTVarIO Map.empty
  rootSupers       <- newTVarIO []
  let
    !worldInitProc = ProcDecl { procedure'name = "<world>"
                              , procedure'args = WildReceiver
                              , procedure'body = voidStatement
                              }
    !worldScope = Scope worldEntity root [] worldInitProc
    !worldClass = Class { classLexiStack = worldScope :| []
                        , classProcedure = worldInitProc
                        }
    !root = Object { objEntity = worldEntity
                   , objClass  = worldClass
                   , objSupers = rootSupers
                   }
    !moduClassProc = ProcDecl { procedure'name = "<module>"
                              , procedure'args = WildReceiver
                              , procedure'body = voidStatement
                              }
    !scopeClassProc = ProcDecl { procedure'name = "<scope>"
                               , procedure'args = WildReceiver
                               , procedure'body = voidStatement
                               }
    !scopeClass = Class { classLexiStack = worldScope :| []
                        , classProcedure = scopeClassProc
                        }
  opPD    <- newTMVarIO Map.empty
  modus   <- newTVarIO Map.empty
  runtime <- newTMVarIO EdhRuntime { runtimeLogger   = defaultLogger
                                   , runtimeLogLevel = 20
                                   }
  return $ EdhWorld
    { worldRoot      = root
    , moduleClass    = Class { classLexiStack = worldScope :| []
                             , classProcedure = moduClassProc
                             }
    , scopeSuper     = Object { objEntity = scopeManiMethods
                              , objClass  = scopeClass
                              , objSupers = rootSupers
                              }
    , worldOperators = opPD
    , worldModules   = modus
    , worldRuntime   = runtime
    }
 where
  defaultLogger :: LogLevel -> Maybe String -> ArgsPack -> STM ()
  defaultLogger !level !srcLoc !pkargs = unsafeIOToSTM $ case pkargs of
    ArgsPack [!argVal] !kwargs | Map.null kwargs ->
      hPutStrLn stderr $ logPrefix ++ T.unpack (edhValueStr argVal)
    _ -> hPutStrLn stderr $ logPrefix ++ show pkargs
   where
    logPrefix :: String
    logPrefix =
      (case srcLoc of
          Nothing -> id
          Just sl -> (++ sl ++ "\n")
        )
        $ case level of
            _ | level >= 50 -> "🔥 "
            _ | level >= 40 -> "❗ "
            _ | level >= 30 -> "⚠️ "
            _ | level >= 20 -> "ℹ️ "
            _ | level >= 10 -> "🐞 "
            _               -> "😥 "

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
    if newPrec < 0 || newPrec >= 10
      then
        throwSTM
        $  UsageError
        $  "Invalidate precedence "
        <> T.pack (show newPrec)
        <> " (declared "
        <> T.pack (show newDeclLoc)
        <> ") for operator: "
        <> op
      else if prevPrec /= newPrec
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


mkHostProc :: Text -> EdhProcedure -> STM EdhValue
mkHostProc !d !p = do
  !s <- unsafeIOToSTM $ newCString $ T.unpack d
  let !hp = HostProcedure { hostProc'name = s, hostProc'proc = p }
  unsafeIOToSTM $ addFinalizer hp $ free s
  return $ EdhHostProc hp

mkHostOper :: EdhWorld -> OpSymbol -> EdhProcedure -> STM EdhValue
mkHostOper world opSym proc =
  Map.lookup opSym <$> (readTMVar $ worldOperators world) >>= \case
    Nothing ->
      throwSTM
        $  UsageError
        $  "No precedence declared in the world for operator: "
        <> opSym
    Just (prec, _) -> do
      !s <- unsafeIOToSTM $ newCString $ T.unpack opSym
      let !hp = HostProcedure s proc
      unsafeIOToSTM $ addFinalizer hp $ free s
      return $ EdhHostOper prec hp


installEdhAttrs :: Entity -> [(AttrKey, EdhValue)] -> STM ()
installEdhAttrs e as = modifyTVar' e $ \em -> Map.union ad em
  where ad = Map.fromList as

installEdhAttr :: Entity -> AttrKey -> EdhValue -> STM ()
installEdhAttr e k v = modifyTVar' e $ \em -> Map.insert k v em


voidStatement :: StmtSrc
voidStatement = StmtSrc
  ( SourcePos { sourceName   = "<Genesis>"
              , sourceLine   = mkPos 1
              , sourceColumn = mkPos 1
              }
  , VoidStmt
  )

