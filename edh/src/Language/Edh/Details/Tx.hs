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


withEdhTx :: EdhProg a -> (a -> EdhProg ()) -> EdhProg ()
withEdhTx initOps collectResult = do
  txs@(EdhTxState !masterThread !openOps !_execOps) <- ask
  let
    sureOpenTx :: IO (IORef EdhTxOps, IO ())
    sureOpenTx = modifyMVar openOps $ \case
      Nothing -> do
        oops' <- newIORef (EdhTxOps [] [])
        return
          (Just oops', (oops', modifyMVar_ openOps (const $ return Nothing)))
      Just oops' -> return (Just oops', (oops', return ()))
  liftIO $ do
    v <- bracket sureOpenTx snd $ const $ runReaderT (unEdhProg initOps) txs
    void $ asyncEdh masterThread $ runReaderT (unEdhProg $ collectResult v) txs


edhReadAttr :: Entity -> AttrKey -> (EdhValue -> EdhProg ()) -> EdhProg ()
edhReadAttr ent key exit = do
  txs@(EdhTxState _ !openOps !_execOps) <- ask
  liftIO $ readMVar openOps >>= \case
    -- not in tx, fast read through
    Nothing -> readTVarIO ent >>= \em -> case Map.lookup key em of
      Nothing  -> error "bug in attr resolution"
      Just val -> runReaderT (unEdhProg $ exit val) txs
    -- in tx, schedule the read to the open op set
    Just oops -> liftIO $ do
      var <- newEmptyTMVarIO
      tid <- newIORef Nothing
      atomicModifyIORef' oops $ \(EdhTxOps rOps wOps) ->
        (EdhTxOps ((ent, key, exit, var, tid) : rOps) wOps, ())

edhWriteAttr
  :: Entity -> AttrKey -> ((EdhValue -> EdhProg ()) -> EdhProg ()) -> EdhProg ()
edhWriteAttr ent key exit = do
  txs@(EdhTxState _ !openOps !_execOps) <- ask
  liftIO $ readMVar openOps >>= \case
    -- not in tx, fast write through
    Nothing -> do
      let writeThrough :: EdhValue -> EdhProg ()
          writeThrough val =
            liftIO $ atomically $ modifyTVar ent $ Map.insert key val
      runReaderT (unEdhProg $ exit writeThrough) txs
    -- in tx, schedule the write to the open op set
    Just oops -> liftIO $ do
      var <- newEmptyTMVarIO
      tid <- newIORef Nothing
      atomicModifyIORef' oops $ \(EdhTxOps rOps wOps) ->
        (EdhTxOps rOps ((ent, key, exit, var, tid) : wOps), ())


cleanupEdhProg :: MVar () -> EdhProg ()
cleanupEdhProg halt = ask >>= \(EdhTxState _ _ !execOps) -> liftIO $ do
  -- make sure halt is signaled
  void $ tryPutMVar halt ()
  -- notify the driver from blocking wait to check halt
  void $ tryPutMVar execOps (EdhTxOps [] [])

runEdhProg :: MVar () -> EdhProg () -> IO ()
runEdhProg halt prog = do
  -- prepare transactional state
  !masterThread <- myThreadId
  !openOps      <- newEmptyMVar
  !execOps      <- newEmptyMVar
  let !txs = (EdhTxState masterThread openOps execOps)
  -- launch the program
  runReaderT (unEdhProg prog) txs
  -- drive transaction executions from the master thread
  driveEdhProg txs
 where
  driveEdhProg :: EdhTxState -> IO ()
  driveEdhProg txs@(EdhTxState _ _ execOps) = do
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
    crunchTx (EdhTxState masterThread _ _) txOps@(EdhTxOps txReads txWrites) =
      do
        -- drive ops from the program
        forM_ txReads $ \(_ent, _key, exit, var, tid) ->
          atomicWriteIORef tid
            .   Just
            =<< (asyncEdh masterThread $ do
                  val <- atomically $ readTMVar var
                  runReaderT (unEdhProg $ exit val) txs
                )
        forM_ txWrites $ \(_ent, _key, exit, var, tid) ->
          atomicWriteIORef tid
            .   Just
            =<< (asyncEdh masterThread $ runReaderT
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
        forM_ txReads $ \(ent, key, _exit, var, _tid) -> do
          -- TODO good idea to group reads by entity ?
          em <- readTVar ent
          case Map.lookup key em of
            Nothing ->
              -- in Edh, an attribute can not be **deleted** from an entity once
              -- set, no op like `delete` in JavaScript or `del` in Python.
              error "bug in attr resolution"
            Just val -> putTMVar var val
        forM_ txWrites $ \(ent, key, _exit, var, _tid) -> do
          val <- readTMVar var
          -- TODO good idea to group writes by entity ?
          modifyTVar' ent $ \em -> Map.insert key val em
        -- return nop to break the loop on success
        return (return ())
      resetAll :: IO ()
      resetAll = do
        forM_ txReads $ \(_ent, _key, _exit, var, tid) -> do
          atomicModifyIORef' tid (Nothing, ) >>= \case
            Nothing   -> return ()
            Just tid' -> killThread tid'
          void $ atomically (tryTakeTMVar var)
        forM_ txWrites $ \(_ent, _key, _exit, var, tid) -> do
          atomicModifyIORef' tid (Nothing, ) >>= \case
            Nothing   -> return ()
            Just tid' -> killThread tid'
          void $ atomically (tryTakeTMVar var)

