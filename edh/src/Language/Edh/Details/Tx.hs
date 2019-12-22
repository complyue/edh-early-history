{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Details.Tx
  ( runEdhProg
  , readEdhAttr
  , writeEdhAttr
  , throwEdh
  , withEdhTx
  , withEdhTx'
  , asyncEdh
  )
where

import           Prelude
import           Debug.Trace

import           Control.Exception

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Concurrent
import           Control.Concurrent.STM


import           Data.IORef
import qualified Data.Map.Strict               as Map

import           Language.Edh.Details.RtTypes


throwEdh :: Exception e => e -> EdhProg a
throwEdh !e = liftIO $ throwIO e

-- | Run an Edh sub-program in a separate thread, with all kinds of
-- exceptions re-thrown to master thread, unless it is itself getting
-- killed.
asyncEdh :: EdhProg () -> EdhProg ()
asyncEdh f = do
  txs@(EdhTxState !masterTh _ _) <- ask
  void $ liftIO $ asyncEdh' masterTh $ runReaderT (unEdhProg f) txs

-- | Run an action in a separate thread, with all kinds of exceptions
-- re-thrown to master thread, unless it is itself getting killed.
asyncEdh' :: ThreadId -> IO () -> IO ThreadId
asyncEdh' !masterTh !f = forkIO $ catch f onExc
 where
  onExc :: SomeException -> IO ()
  onExc e = case asyncExceptionFromException e of
    Nothing -> throwTo masterTh e >> throwIO ThreadKilled
    Just ae -> case ae of
      ThreadKilled -> return ()
      -- TODO is this right ?
      _            -> throwTo masterTh e


withEdhTx :: EdhProg () -> EdhProg ()
withEdhTx !initOps = do
  txs@(EdhTxState !_ !openOps !execOps) <- ask
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
  liftIO $ bracket ensureOpenTx id $ const $ runReaderT (unEdhProg initOps) txs

withEdhTx' :: EdhProg a -> (a -> EdhProg ()) -> EdhProg ()
withEdhTx' !initOps !collectResult = do
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
    void $ asyncEdh' masterTh $ runReaderT (unEdhProg $ collectResult v) txs


readEdhAttr :: Entity -> AttrKey -> (EdhValue -> EdhProg ()) -> EdhProg ()
readEdhAttr !ent !key !exit = do
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

writeEdhAttr
  :: Entity -> AttrKey -> ((EdhValue -> EdhProg ()) -> EdhProg ()) -> EdhProg ()
writeEdhAttr !ent !key !exit = do
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


runEdhProg :: EdhProg () -> IO ()
runEdhProg !prog = do
  -- prepare transactional state
  !masterTh <- myThreadId
  !openOps  <- newMVar Nothing
  !execOps  <- newEmptyMVar
  let !txs = EdhTxState masterTh openOps execOps
  -- launch the program
  runReaderT (unEdhProg prog) txs
  -- check synchronous result
  liftIO $ isEmptyMVar execOps >>= \case
    True  -> return () -- program finished synchronously without tx submitted 
    False -> do -- there're transactions submitted, drive the executions
-- drive transaction executions from the master thread.
-- exceptions occurred in all threads started by this program will be re-thrown
-- asynchronously to this thread, causing the whole program to abort.
      driveEdhProg txs
 where
  driveEdhProg :: EdhTxState -> IO ()
  driveEdhProg txs@(EdhTxState _ _ !execOps) = do
    -- check next tx
    tryTakeMVar execOps >>= \case
      Nothing     -> return () -- no more tx to execute
      Just !txOps -> do
        -- drive this tx
        crunchTx 0 txs txOps
        -- take a breath
        yield
        -- start another iteration
        driveEdhProg txs
   where
    crunchTx :: Int -> EdhTxState -> EdhTxOps -> IO ()
    crunchTx retryCntr (EdhTxState !masterTh _ _) txOps@(EdhTxOps !txReads !txWrites)
      = do
        -- asynchronously launch all tx ops submitted by the program
        forM_ txReads $ \(!_ent, !_key, !exit, !var, !tid) ->
          atomicWriteIORef tid
            .   Just
            =<< (asyncEdh' masterTh $ do
                  !val <- atomically $ readTMVar var
                  runReaderT (unEdhProg $ exit val) txs
                )
        forM_ txWrites $ \(!_ent, !_key, !exit, !var, !tid) ->
          atomicWriteIORef tid
            .   Just
            =<< (asyncEdh' masterTh $ runReaderT
                  (unEdhProg $ exit (liftIO . atomically . putTMVar var))
                  txs
                )
        -- auto cycling on STM retry
        join $ atomically $ stmTx `orElse` return
          (resetAll >> crunchTx (retryCntr + 1) txs txOps)
     where
      -- pump values within a single STM transaction
      stmTx :: STM (IO ())
      stmTx = do
        -- grouped reads per entity
        readPerEnt [] txReads []
        -- grouped writes per entity
        writePerEnt [] txWrites []
        -- return nop to break the loop on success
        return (return ())

      readPerEnt
        :: [TxReadOp] -- ^ queue
        -> [TxReadOp] -- ^ candidates
        -> [TxReadOp] -- ^ backlog
        -> STM ()
      readPerEnt [] []          []      = return ()
      readPerEnt [] (op : rest) backlog = readPerEnt [op] rest backlog
      readPerEnt queue@((!te, _, _, _, _) : _) (op@(!ce, _, _, _, _) : rest) backlog
        = if ce == te
          then readPerEnt (op : queue) rest backlog
          else readPerEnt queue rest (op : backlog)
      readPerEnt queue@((!ent, _, _, _, _) : _) [] backlog = do
        !em <- readTVar ent
        forM_ queue $ \(_, !key, !_exit, !var, !_tid) -> do
          case Map.lookup key em of
            Nothing ->
              -- in Edh, an attribute can not be **deleted** from an entity once
              -- set, no op like `delete` in JavaScript or `del` in Python.
              error "bug in attr resolution"
            Just val -> putTMVar var val

        readPerEnt [] backlog []
      readPerEnt _ _ _ = error "bug"

      writePerEnt
        :: [TxWriteOp] -- ^ queue
        -> [TxWriteOp] -- ^ candidates
        -> [TxWriteOp] -- ^ backlog
        -> STM ()
      writePerEnt [] []          []      = return ()
      writePerEnt [] (op : rest) backlog = writePerEnt [op] rest backlog
      writePerEnt queue@((!te, _, _, _, _) : _) (op@(!ce, _, _, _, _) : rest) backlog
        = if ce == te
          then writePerEnt (op : queue) rest backlog
          else writePerEnt queue rest (op : backlog)
      writePerEnt queue@((!ent, _, _, _, _) : _) [] backlog = do
        !um <- Map.fromList <$> forM
          queue
          (\(_, key, _exit, var, _tid) -> do
            !val <- readTMVar var
            return (key, val)
          )
        modifyTVar' ent $ \em -> Map.union um em

        writePerEnt [] backlog []
      writePerEnt _ _ _ = error "bug"

      resetAll :: IO ()
      resetAll = do
        when (retryCntr >= 3)
          $ trace (" -*- stm retrying #" <> show retryCntr)
          $ return ()
        -- kill all launched threads, clear all vars
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

