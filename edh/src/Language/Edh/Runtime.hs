
module Language.Edh.Runtime
  ( createEdhWorld
  , defaultEdhLogger
  , declareEdhOperators
  , installEdhAttrs
  , installEdhAttr
  , bootEdhModule
  , runEdhProgram
  , runEdhProgram'
  , mkHostProc
  , mkHostOper
-- TODO cherrypick what artifacts to export as for user interface
  , module CL
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

import           Control.Concurrent
import           Control.Concurrent.STM

import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           System.Mem.Weak

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map
import           Data.List.NonEmpty             ( NonEmpty(..) )

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.CoreLang as CL
import           Language.Edh.Details.RtTypes  as RT
import           Language.Edh.Details.Tx       as TX
import           Language.Edh.Details.Evaluate as EV


bootEdhModule
  :: MonadIO m => EdhWorld -> Text -> m (Either InterpretError Object)
bootEdhModule !world impSpec = liftIO $ tryJust edhKnownError $ do
  !final <- newEmptyTMVarIO
  runEdhProgram' ctx
    $ importEdhModule (SingleReceiver (RecvRestPkArgs "_")) impSpec
    $ \(_, _, !val) -> case val of
        EdhObject modu -> contEdhSTM $ putTMVar final modu
        _              -> error "bug: importEdhModule returns non-object?"
  atomically $ readTMVar final
 where
  !root = worldRoot world
  !ctx  = moduleContext world root


runEdhProgram
  :: MonadIO m
  => EdhWorld
  -> Object
  -> [StmtSrc]
  -> m (Either InterpretError EdhValue)
runEdhProgram !world !modu !stmts = liftIO $ tryJust edhKnownError $ do
  !final <- newEmptyTMVarIO
  runEdhProgram' ctx $ evalBlock modu stmts $ \(_, _, !val) ->
    contEdhSTM $ putTMVar final val
  atomically $ readTMVar final
  where !ctx = moduleContext world modu

runEdhProgram' :: MonadIO m => Context -> EdhProg (STM ()) -> m ()
runEdhProgram' !ctx !prog = liftIO $ driveEdhProgram ctx prog


defaultEdhLogger :: IO EdhLogger
defaultEdhLogger = do
  logQueue <- newTQueueIO
  let logPrinter :: IO ()
      logPrinter = do
        msg <- atomically $ readTQueue logQueue
        hPutStrLn stderr msg
        logPrinter
      logger :: EdhLogger
      logger !level !srcLoc !pkargs = case pkargs of
        ArgsPack [!argVal] !kwargs | Map.null kwargs ->
          writeTQueue logQueue $ logPrefix ++ T.unpack (edhValueStr argVal)
        _ -> writeTQueue logQueue $ logPrefix ++ show pkargs
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
  void $ forkIO logPrinter
  return logger


createEdhWorld :: MonadIO m => EdhLogger -> m EdhWorld
createEdhWorld !logger = liftIO $ do
  -- ultimate default methods/operators/values go into this
  rootEntity <- newTVarIO $ Map.fromList
    [ (AttrByName "__name__", EdhString "<root>")
    , (AttrByName "__file__", EdhString "<Genesis>")
    ]
  -- methods supporting reflected scope manipulation go into this
  scopeManiMethods <- newTVarIO Map.empty
  rootSupers       <- newTVarIO []
  let
    !moduClassProc = ProcDecl { procedure'name = "<module>"
                              , procedure'args = WildReceiver
                              , procedure'body = voidStatement
                              }
    !worldScope = Scope rootEntity root [] moduClassProc
    !moduClass  = Class { classLexiStack = worldScope :| []
                        , classProcedure = moduClassProc
                        }
    !root = Object { objEntity = rootEntity
                   , objClass  = moduClass
                   , objSupers = rootSupers
                   }
    !scopeClassProc = ProcDecl { procedure'name = "<scope>"
                               , procedure'args = WildReceiver
                               , procedure'body = voidStatement
                               }
    !scopeClass = Class { classLexiStack = worldScope :| []
                        , classProcedure = scopeClassProc
                        }
  opPD    <- newTMVarIO Map.empty
  modus   <- newTMVarIO Map.empty
  runtime <- newTMVarIO EdhRuntime { runtimeLogger   = logger
                                   , runtimeLogLevel = 20
                                   }
  return $ EdhWorld
    { worldRoot      = root
    , moduleClass    = moduClass
    , scopeSuper     = Object { objEntity = scopeManiMethods
                              , objClass  = scopeClass
                              , objSupers = rootSupers
                              }
    , worldOperators = opPD
    , worldModules   = modus
    , worldRuntime   = runtime
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


mkHostProc
  :: (HostProcedure -> EdhValue) -> Text -> EdhProcedure -> STM EdhValue
mkHostProc !vc !d !p = do
  !s <- unsafeIOToSTM $ newCString $ T.unpack d
  let !hp = HostProcedure { hostProc'name = s, hostProc'proc = p }
  unsafeIOToSTM $ addFinalizer hp $ free s
  return $ vc hp

mkHostOper :: EdhWorld -> OpSymbol -> EdhProcedure -> STM EdhValue
mkHostOper world opSym proc =
  Map.lookup opSym <$> readTMVar (worldOperators world) >>= \case
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
