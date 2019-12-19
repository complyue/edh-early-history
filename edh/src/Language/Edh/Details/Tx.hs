{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Details.Tx
  ( EdhProg(..)
  , runEdhProg
  , cleanupEdhProg
  , edhReadAttr
  , edhWriteAttr
  , throwEdh
  , runEdhTx

  -- , EdhTxState(..)
  -- , EdhTxOps(..)
  -- , EdhOpsPack
  -- , asyncEdh
  )
where

import           Prelude
-- import           Debug.Trace

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Concurrent

import           Data.IORef
import qualified Data.Map.Strict               as Map

import           Language.Edh.Details.RtTypes
import           Language.Edh.Details.Utils


throwEdh :: Exception e => e -> EdhProg a
throwEdh e = liftIO $ throwIO e

asyncEdh :: ThreadId -> IO () -> IO ()
asyncEdh masterThread f = void $ forkIO $ void $ handle onExc f
 where
  onExc :: SomeException -> IO ()
  onExc e = throwTo masterThread e >> throwIO ThreadKilled


-- | Transactional operations packed per entity basis
type EdhOpsPack = [(Entity, [(AttrKey, MVar EdhValue)])]
-- todo seek better data structure to manage state of a tx.
--      using list as is for pending ops is not optimal;
--      simply using strict 'Map Entity [(,)]' may be better,
--      but not trival as 'Entity' ('MVar' per se) lacks 'Ord' instance.
--      anyway, neither above is cache friendly.
_packTxOp :: Entity -> AttrKey -> MVar EdhValue -> EdhOpsPack -> EdhOpsPack
_packTxOp ent key var = packTxOp' []
 where
  packTxOp' :: EdhOpsPack -> EdhOpsPack -> EdhOpsPack
  packTxOp' prefix []                     = (ent, [(key, var)]) : prefix
  packTxOp' prefix (r@(ent', eos) : rest) = if ent' /= ent
    then packTxOp' (r : prefix) rest
    else (ent, (key, var) : eos) : prefix ++ rest


-- | All operations per a transaction
data EdhTxOps = EdhTxOps {
    _edh'tx'reads :: !EdhOpsPack
    , _edh'tx'writes :: !EdhOpsPack
  }

data EdhTxState = EdhTxState {
    _edh'tx'master :: !ThreadId
    -- | the op set still open for new ops to join
    , _edh'tx'open :: !(MVar (IORef EdhTxOps))
    -- | the op set submitted for execution
    , _edh'tx'exec :: !(MVar EdhTxOps)
  }

runEdhTx :: EdhProg a -> (a -> EdhProg ()) -> EdhProg ()
runEdhTx initOps collectResult = do
  txs@(EdhTxState !masterThread !openOps !execOps) <- ask
  liftIO $ do
    oops <- newIORef (EdhTxOps [] [])
    v    <- bracket_ (putMVar openOps oops) (takeMVar openOps)
      $ runReaderT (unEdhProg initOps) txs
    oops' <- readIORef oops
    asyncEdh masterThread $ do
      putMVar execOps oops'
      runReaderT (unEdhProg $ collectResult v) txs

edhReadAttr :: Entity -> AttrKey -> (EdhValue -> EdhProg ()) -> EdhProg ()
edhReadAttr ent key exit = do
  txs@(EdhTxState masterThread !openOps !_execOps) <- ask
  liftIO $ tryReadMVar openOps >>= \case
    Nothing -> readMVar ent >>= \em -> case Map.lookup key em of
      -- not in tx, fast snapshot read alone
      Nothing  -> error "bug in attr resolution"
      Just val -> runReaderT (unEdhProg $ exit val) txs
    Just oops -> do -- in tx, schedule the read to the open op set
      var <- newEmptyMVar
      atomicModifyIORef' oops $ \(EdhTxOps rpck wpck) ->
        (EdhTxOps (_packTxOp ent key var rpck) wpck, ())
      asyncEdh masterThread $ do
        val <- readMVar var
        runReaderT (unEdhProg $ exit val) txs

edhWriteAttr
  :: Entity -> AttrKey -> ((EdhValue -> EdhProg ()) -> EdhProg ()) -> EdhProg ()
edhWriteAttr ent key exit = do
  txs@(EdhTxState masterThread !openOps !_execOps) <- ask
  liftIO $ tryReadMVar openOps >>= \case
    Nothing -> -- not in tx, fast write alone
      let writeVal :: EdhValue -> EdhProg ()
          writeVal val = liftIO $ modifyMVar_ ent $ return . Map.insert key val
      in  runReaderT (unEdhProg $ exit writeVal) txs
    Just oops -> do -- in tx, schedule the write to the open op set
      var <- newEmptyMVar
      atomicModifyIORef' oops $ \(EdhTxOps rpck wpck) ->
        (EdhTxOps rpck (_packTxOp ent key var wpck), ())
      asyncEdh masterThread
        $ runReaderT (unEdhProg $ exit (liftIO . putMVar var)) txs


-- | The transactional monad of Edh
newtype EdhProg a = EdhProg { unEdhProg :: ReaderT EdhTxState IO a }
    deriving (Functor, Applicative, Monad,
        MonadReader EdhTxState,
        MonadIO, MonadFail)


runEdhProg :: MVar () -> EdhProg () -> IO ()
runEdhProg halt prog = do
  -- prepare transactional state
  masterThread <- myThreadId
  openOps      <- newEmptyMVar
  execOps      <- newEmptyMVar

  -- launch program
  runReaderT (unEdhProg prog) (EdhTxState masterThread openOps execOps)

  -- drive transaction executions from the master thread
  driveEdhTx masterThread halt execOps

cleanupEdhProg :: MVar () -> EdhProg ()
cleanupEdhProg halt = ask >>= \(EdhTxState _ _ !execOps) -> liftIO $ do
  -- make sure the program halt properly
  void $ tryPutMVar halt ()
  -- notify the driver from blocking wait to check halt
  void $ tryPutMVar execOps (EdhTxOps [] [])

driveEdhTx :: ThreadId -> MVar () -> MVar EdhTxOps -> IO ()
driveEdhTx masterThread halt execOps = do
  -- blocking wait next tx to come
  EdhTxOps !txrs !txws <- takeMVar execOps
  -- kickoff atomic reads/writes per entity
  launchTx txrs txws
  yield
  -- check halt 
  isEmptyMVar halt >>= \case
    -- shall halt
    False -> return ()
    -- loop another iteration
    True  -> driveEdhTx masterThread halt execOps

 where

  launchTx
    :: [(Entity, [(AttrKey, MVar EdhValue)])]
    -> [(Entity, [(AttrKey, MVar EdhValue)])]
    -> IO ()
  launchTx []                  [] = return ()
  launchTx ((ent, ers) : txrs) [] = do
    asyncEdh masterThread $ perEntity ent ers []
    launchTx txrs []
  launchTx [] ((ent, ews) : txws) = do
    asyncEdh masterThread $ perEntity ent [] ews
    launchTx [] txws
  launchTx ((ent, ers) : txrs) txws@(_ : _) = do
    case w of
      Nothing          -> asyncEdh masterThread $ perEntity ent ers []
      Just (_ent, ews) -> asyncEdh masterThread $ perEntity ent ers ews
    launchTx txrs txws'
    where (w, txws') = takeOutFromList ((== ent) . fst) txws

  perEntity
    :: Entity
    -> [(AttrKey, MVar EdhValue)]
    -> [(AttrKey, MVar EdhValue)]
    -> IO ()
  perEntity ent ers ews = modifyMVar_ ent $ \em -> do
    forM_ ers $ \(key, var) -> case Map.lookup key em of
      Nothing  -> error "bug in attr resolution"
      Just val -> putMVar var val
    flip Map.union em . Map.fromList <$> forM
      ews
      (\(key, var) -> (key, ) <$> readMVar var)

