{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Details.Tx
  ( runEdhProg
  , cleanupEdhProg
  , edhReadAttr
  , edhWriteAttr
  , throwEdh
  , withEdhTx

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

import           Data.IORef
import qualified Data.Map.Strict               as Map

import           Language.Edh.Control
import           Language.Edh.Details.RtTypes


throwEdh :: Exception e => e -> EdhProg a
throwEdh !e = liftIO $ throwIO e

-- | Run an action in a separate thread, with all kinds of exceptions
-- re-thrown to master thread, unless it is itself getting killed.
asyncEdh :: ThreadId -> IO () -> IO ThreadId
asyncEdh !masterTh !f = forkIO $ catch f onExc
 where
  onExc :: SomeException -> IO ()
  onExc e = case asyncExceptionFromException e of
    Nothing -> throwTo masterTh e >> throwIO ThreadKilled
    Just ae -> case ae of
      ThreadKilled -> return ()
      -- TODO is this right ?
      _            -> throwTo masterTh e


withEdhTx :: EdhProg a -> (a -> EdhProg ()) -> EdhProg ()
withEdhTx !initOps !collectResult = do
  txs@(EdhTxState !masterTh !openOps !execOps) <- ask
  let ensureOpenTx :: IO (IO ())
      ensureOpenTx = modifyMVar openOps $ \case
        Nothing -> do
          oops' <- newIORef (EdhTxOps [] [])
          let submitTx = do
                modifyMVar_ openOps (const $ return Nothing)
                !oops'' <- readIORef oops'
                putMVar execOps oops''
          return (Just oops', submitTx)
        Just !oops' -> return (Just oops', return ())
  liftIO $ do
    v <- bracket ensureOpenTx id $ const $ runReaderT (unEdhProg initOps) txs
    void $ asyncEdh masterTh $ runReaderT (unEdhProg $ collectResult v) txs


edhReadAttr :: Entity -> AttrKey -> (EdhValue -> EdhProg ()) -> EdhProg ()
edhReadAttr !ent !key !exit = do
  txs@(EdhTxState _ !openOps !_execOps) <- ask
  liftIO $ readMVar openOps >>= \case
    -- not in tx, fast read through
    Nothing -> readTVarIO ent >>= \em -> case Map.lookup key em of
      Nothing  -> error "bug in attr resolution"
      Just val -> runReaderT (unEdhProg $ exit val) txs
    -- in tx, schedule the read to the open op set
    Just !oops -> liftIO $ do
      !var <- newEmptyTMVarIO
      !tid <- newIORef Nothing
      atomicModifyIORef' oops $ \(EdhTxOps rOps wOps) ->
        (EdhTxOps ((ent, key, exit, var, tid) : rOps) wOps, ())

edhWriteAttr
  :: Entity -> AttrKey -> ((EdhValue -> EdhProg ()) -> EdhProg ()) -> EdhProg ()
edhWriteAttr !ent !key !exit = do
  txs@(EdhTxState _ !openOps !_execOps) <- ask
  liftIO $ readMVar openOps >>= \case
    -- not in tx, fast write through
    Nothing -> do
      let writeThrough :: EdhValue -> EdhProg ()
          writeThrough val =
            liftIO $ atomically $ modifyTVar ent $ Map.insert key val
      runReaderT (unEdhProg $ exit writeThrough) txs
    -- in tx, schedule the write to the open op set
    Just !oops -> liftIO $ do
      !var <- newEmptyTMVarIO
      !tid <- newIORef Nothing
      atomicModifyIORef' oops $ \(EdhTxOps rOps wOps) ->
        (EdhTxOps rOps ((ent, key, exit, var, tid) : wOps), ())


cleanupEdhProg :: MVar () -> EdhProg ()
cleanupEdhProg !halt = ask >>= \(EdhTxState _ _ !execOps) -> liftIO $ do
  -- make sure halt is signaled
  void $ tryPutMVar halt ()
  -- notify the driver from blocking wait to check halt
  void $ tryPutMVar execOps (EdhTxOps [] [])

runEdhProg :: MVar () -> EdhProg () -> IO ()
runEdhProg !halt !prog = do
  -- prepare transactional state
  !masterTh <- myThreadId
  !openOps  <- newMVar Nothing
  !execOps  <- newEmptyMVar
  let !txs = EdhTxState masterTh openOps execOps
  -- launch the program
  runReaderT (unEdhProg prog) txs
  -- drive transaction executions from the master thread
  driveEdhProg txs
 where
  driveEdhProg :: EdhTxState -> IO ()
  driveEdhProg txs@(EdhTxState !_ !_ !execOps) = do
    -- blocking wait next tx to come
    !txOps <- takeMVar execOps
    -- drive this tx
    crunchTx txs txOps
    yield
    -- check halt 
    isEmptyMVar halt >>= \case
      -- shall halt
      False -> isEmptyMVar execOps >>= \case
        False -> throwIO $ EvalError "Edh program halted with tx pending"
        True  -> return ()
      -- loop another iteration
      True -> driveEdhProg txs
   where
    crunchTx :: EdhTxState -> EdhTxOps -> IO ()
    crunchTx (EdhTxState !masterTh !_ !_) txOps@(EdhTxOps !txReads !txWrites) =
      do
        -- drive ops from the program
        forM_ txReads $ \(!_ent, !_key, !exit, !var, !tid) ->
          atomicWriteIORef tid
            .   Just
            =<< (asyncEdh masterTh $ do
                  !val <- atomically $ readTMVar var
                  runReaderT (unEdhProg $ exit val) txs
                )
        forM_ txWrites $ \(!_ent, !_key, !exit, !var, !tid) ->
          atomicWriteIORef tid
            .   Just
            =<< (asyncEdh masterTh $ runReaderT
                  (unEdhProg $ exit (liftIO . atomically . putTMVar var))
                  txs
                )
        -- auto cycling on STM retry
        join $ atomically $ stmTx `orElse` return
          (resetAll >> crunchTx txs txOps)
     where
      stmTx :: STM (IO ())
      stmTx = do
        -- pump values within a single STM transaction
        forM_ txReads $ \(!ent, !key, !_exit, !var, !_tid) -> do
          -- TODO good idea to group reads by entity ?
          !em <- readTVar ent
          case Map.lookup key em of
            Nothing ->
              -- in Edh, an attribute can not be **deleted** from an entity once
              -- set, no op like `delete` in JavaScript or `del` in Python.
              error "bug in attr resolution"
            Just val -> putTMVar var val
        forM_ txWrites $ \(ent, key, _exit, var, _tid) -> do
          !val <- readTMVar var
          -- TODO good idea to group writes by entity ?
          modifyTVar' ent $ \em -> Map.insert key val em
        -- return nop to break the loop on success
        return (return ())
      resetAll :: IO ()
      resetAll = do
        forM_ txReads $ \(_ent, _key, _exit, var, tid) -> do
          atomicModifyIORef' tid (Nothing, ) >>= \case
            Nothing    -> return ()
            Just !tid' -> killThread tid'
          void $ atomically (tryTakeTMVar var)
        forM_ txWrites $ \(_ent, _key, _exit, var, tid) -> do
          atomicModifyIORef' tid (Nothing, ) >>= \case
            Nothing    -> return ()
            Just !tid' -> killThread tid'
          void $ atomically (tryTakeTMVar var)

