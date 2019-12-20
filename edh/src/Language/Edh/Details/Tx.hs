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
import           Control.Concurrent.STM
import           Control.Monad.STM

import           Data.IORef
import qualified Data.Map.Strict               as Map

import           Language.Edh.Control
import           Language.Edh.Details.RtTypes
import           Language.Edh.Details.Utils


throwEdh :: Exception e => e -> EdhProg a
throwEdh e = liftIO $ throwIO e

-- | Run an action in a separate thread, with all kinds of exceptions
-- re-thrown to master thread, unless it is itself getting killed.
asyncEdh :: ThreadId -> IO () -> IO ThreadId
asyncEdh masterThread f = forkIO $ catch f onExc
 where
  onExc :: SomeException -> IO ()
  onExc e = case asyncExceptionFromException e of
    Nothing -> throwTo masterThread e >> throwIO ThreadKilled
    Just ae -> case ae of
      ThreadKilled -> return ()
      -- TODO is this right ?
      _            -> throwTo masterThread e


runEdhTx :: EdhProg a -> (a -> EdhProg ()) -> EdhProg ()
runEdhTx initOps collectResult = do
  txs@(EdhTxState !masterThread !openOps !execOps) <- ask
  liftIO $ do
    oops <- newIORef (EdhTxOps [] [])
    v    <- bracket_ (putTMVar openOps oops) (takeTMVar openOps)
      $ runReaderT (unEdhProg initOps) txs
    oops' <- readIORef oops
    asyncEdh masterThread $ do
      putTMVar execOps oops'
      runReaderT (unEdhProg $ collectResult v) txs

edhReadAttr :: Entity -> AttrKey -> (EdhValue -> EdhProg ()) -> EdhProg ()
edhReadAttr ent key exit = do
  txs@(EdhTxState masterThread !openOps !_execOps) <- ask
  liftIO $ tryReadTMVar openOps >>= \case
    Nothing -> readTMVar ent >>= \em -> case Map.lookup key em of
      -- not in tx, fast snapshot read alone
      Nothing  -> error "bug in attr resolution"
      Just val -> runReaderT (unEdhProg $ exit val) txs
    Just oops -> do -- in tx, schedule the read to the open op set
      var <- newEmptyMVar
      atomicModifyIORef' oops $ \(EdhTxOps rpck wpck) ->
        (EdhTxOps (_packTxOp ent key var rpck) wpck, ())
      asyncEdh masterThread $ do
        val <- readTMVar var
        runReaderT (unEdhProg $ exit val) txs

edhWriteAttr
  :: Entity -> AttrKey -> ((EdhValue -> EdhProg ()) -> EdhProg ()) -> EdhProg ()
edhWriteAttr ent key exit = do
  txs@(EdhTxState masterThread !openOps !_execOps) <- ask
  liftIO $ tryReadTMVar openOps >>= \case
    Nothing -> -- not in tx, fast write alone
      let writeVal :: EdhValue -> EdhProg ()
          writeVal val = liftIO $ modifyTVar' ent $ return . Map.insert key val
      in  runReaderT (unEdhProg $ exit writeVal) txs
    Just oops -> do -- in tx, schedule the write to the open op set
      var <- newEmptyMVar
      atomicModifyIORef' oops $ \(EdhTxOps rpck wpck) ->
        (EdhTxOps rpck (_packTxOp ent key var wpck), ())
      asyncEdh masterThread
        $ runReaderT (unEdhProg $ exit (liftIO . putTMVar var)) txs


runEdhProg :: MVar () -> EdhProg () -> IO ()
runEdhProg halt prog = do
  -- prepare transactional state
  masterThread <- myThreadId
  openOps      <- newEmptyMVar
  execOps      <- newEmptyMVar
  let txs = (EdhTxState masterThread openOps execOps)

  -- launch the program
  runReaderT (unEdhProg prog) txs

  -- drive transaction executions from the master thread
  driveEdhTx txs halt

cleanupEdhProg :: MVar () -> EdhProg ()
cleanupEdhProg halt = ask >>= \(EdhTxState _ _ !execOps) -> liftIO $ do
  -- make sure the program halt properly
  void $ tryPutMVar halt ()
  -- notify the driver from blocking wait to check halt
  void $ tryPutMVar execOps (EdhTxOps [] [])

driveEdhTx :: EdhTxState -> MVar () -> IO ()
driveEdhTx txs@(EdhTxState masterThread openOps execOps) halt = do
  -- blocking wait next tx to come
  !txOps <- takeMVar execOps
  -- kickoff atomic reads/writes per entity
  crunchTx txs txOps
  yield
  -- check halt 
  isEmptyMVar halt >>= \case
    -- shall halt
    False -> isEmptyMVar execOps >>= \case
      False -> throwIO $ EvalError "Edh program halted with tx pending"
      True  -> return ()
    -- loop another iteration
    True -> driveEdhTx txs halt

 where
  crunchTx :: EdhTxState -> EdhTxOps -> IO ()
  crunchTx txs@(EdhTxState masterThread openOps execOps) txOps@(EdhTxOps txReads txWrites)
    = do
      -- drive ops from the program
      forM_ txReads $ \(ent, key, exit, var, tid) -> asyncEdh masterThread $ do
        val <- atomically $ readTMVar var
        runReaderT (unEdhProg $ exit val) txs
      forM_ txWrites $ \(ent, key, exit, var, tid) ->
        asyncEdh masterThread $ runReaderT (unEdhProg $ exit var) txs

      -- auto cycling on STM retry
      join $ atomically $ finishTx `orElse` return
        (resetAll >> crunchTx txs txOps)
   where
    finishTx :: STM (IO ())
    finishTx = do
      -- pump values within STM
      forM_ txReads $ \(ent, key, exit, var, tid) -> do
        em <- readTVar ent
        case Map.lookup key em of
          Nothing ->
            -- in Edh, an attribute can not be **deleted** from an entity once
            -- set, no op like `delete` in JavaScript or `del` in Python.
            error "bug in attr resolution"
          Just val -> putTMVar var val
      forM_ txWrites $ \(ent, key, exit, var, tid) -> do
        val <- readTMVar var
        -- TODO good idea to group writes by entity ?
        modifyTVar' ent $ \em -> Map.insert key val em

      -- return nop to break the loop on success
      return (return ())

    resetAll :: IO ()
    resetAll = do
      forM_ txReads $ \(ent, key, exit, var, tid) -> do
        readIORef tid >>= \case
          Nothing   -> return ()
          Just tid' -> killThread tid'
        void $ atomically (tryTakeTMVar var)
      forM_ txWrites $ \(ent, key, exit, var, tid) -> do
        readIORef tid >>= \case
          Nothing   -> return ()
          Just tid' -> killThread tid'
        void $ atomically (tryTakeTMVar var)


  -- updEntity
  --   :: EntityStore
  --   -> [(AttrKey, TMVar EdhValue)]
  --   -> [(AttrKey, TMVar EdhValue)]
  --   -> STM EntityStore
  -- updEntity em ers ews = do
  --   forM_ ers $ \(key, var) -> case Map.lookup key em of
  --     Nothing ->
  --     -- in Edh, an attribute can not be **deleted** from an entity once
  --     -- set, no op like `delete` in JavaScript or `del` in Python.
  --       error "bug in attr resolution"
  --     Just val -> putTMVar var val
  --   flip Map.union em . Map.fromList <$> forM
  --     ews
  --     (\(key, var) -> (key, ) <$> readTMVar var)


  -- launchTx
  --   :: [(Entity, [(AttrKey, TMVar EdhValue)])]
  --   -> [(Entity, [(AttrKey, TMVar EdhValue)])]
  --   -> IO ()
  -- launchTx []                  [] = return ()
  -- launchTx ((ent, ers) : txrs) [] = do
  --   asyncEdh masterThread $ perEntity ent ers []
  --   launchTx txrs []
  -- launchTx [] ((ent, ews) : txws) = do
  --   asyncEdh masterThread $ perEntity ent [] ews
  --   launchTx [] txws
  -- launchTx ((ent, ers) : txrs) txws@(_ : _) = do
  --   case w of
  --     Nothing          -> asyncEdh masterThread $ perEntity ent ers []
  --     Just (_ent, ews) -> asyncEdh masterThread $ perEntity ent ers ews
  --   launchTx txrs txws'
  --   where (w, txws') = takeOutFromList ((== ent) . fst) txws

  -- perEntity
  --   :: Entity
  --   -> [(AttrKey, TMVar EdhValue)]
  --   -> [(AttrKey, TMVar EdhValue)]
  --   -> IO ()
  -- perEntity ent ers ews = atomically $ do
  --   em  <- readTVar ent
  --   em' <- updEntity em ers ews
  --   writeTVar ent em'
