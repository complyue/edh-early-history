{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Details.Tx
  ( runEdhProg
  , cleanupEdhProg
  , edhReadAttr
  , edhWriteAttr
  , throwEdh
  , runEdhTx

  -- , asyncEdh
  )
where

import           Prelude
-- import           Debug.Trace

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
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

