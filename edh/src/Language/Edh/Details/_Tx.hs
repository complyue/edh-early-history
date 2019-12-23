{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Details.Tx where

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



-- | The monad for transactional running of an Edh program
newtype EdhProg a = EdhProg { unEdhProg :: ReaderT EdhTxState IO a }
  deriving (Functor, Applicative, Monad, MonadReader EdhTxState,
            MonadIO, MonadFail)

-- | The states of a transaction
data EdhTxState = EdhTxState {
    edh'tx'master :: !ThreadId
    -- | the stub for an open op set collecting more ops into current tx
    , edh'tx'open :: !(MVar (Maybe (IORef EdhTxOps)))
    -- | the stub an op set is submitted into for execution
    , edh'tx'exec :: !(MVar EdhTxOps)
  }

-- | All operations per a transaction
type EdhTxOps = [STM (EdhProg ())]
-- data EdhTxOps = EdhTxOps {
--     edh'tx'reads :: ! [TxReadOp]
--     , edh'tx'writes :: ![TxWriteOp]
--   }
-- type TxReadOp = (STM EdhValue, EdhValue -> EdhProg ())
-- type TxWriteOp = STM ()


-- | Type of a procedure in host language that can be called from Edh code.
--
-- Edh is not tracking whether such procedures are pure (i.e. side-effect
-- free) or impure (i.e. world-changing), though writing only pure functions
-- conforming to this procedual interface in the host language (Haskell)
-- should be considered idiomatic).
type EdhProcedure -- such a procedure servs as the callee
  =  Context -- ^ the caller's context
  -> ArgsSender -- ^ the manifestation of how the caller wills to send args
  -> Scope -- ^ the scope from which the callee is addressed off
  -> EdhProcExit -- ^ the CPS exit to return a value from this procedure
  -> EdhProg ()

-- | The type for an Edh procedure's return, in continuation passing style.
type EdhProcExit = STM (Scope, EdhValue) -> EdhProg ()



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
withEdhTx !subProg = do
  txs@(EdhTxState !_ !openOps !execOps) <- ask
  let ensureOpenTx :: IO (IO ())
      ensureOpenTx = modifyMVar openOps $ \case
        Nothing -> do
          oops' <- newIORef []
          let submitTx = do
                modifyMVar_ openOps (const $ return Nothing)
                !oops'' <- readIORef oops'
                putMVar execOps oops''
          return (Just oops', submitTx)
        Just !oops' -> return (Just oops', return ())
  liftIO $ bracket ensureOpenTx id $ const $ runReaderT (unEdhProg subProg) txs


-- doEdhOp :: EdhProg () -> STM ()
-- doEdhOp op = return $ do
--   (EdhTxState _ !openOps _) <- ask
--   (liftIO $ readMVar openOps) >>= \case
--     -- not in tx, immediate execution
--     Nothing    -> (liftIO $ atomically op) >>= id
--     -- in tx, schedule the op to the open op set
--     Just !oops -> liftIO $ atomicModifyIORef' oops $ \ops -> (op : ops, ())


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
        trace " ** got tx **" $ return ()
        -- drive this tx
        crunchTx 0 txOps
        -- take a breath
        yield
        trace " ** done tx **" $ return ()
        -- start another iteration
        driveEdhProg txs
   where
    crunchTx :: Int -> EdhTxOps -> IO ()
    crunchTx retryCntr txOps = do
      when (retryCntr > 0)
        $ trace (" ** stm retry #" <> show retryCntr)
        $ return ()

      atomically (stmTx `orElse` return Nothing) >>= \case
        Nothing -> -- stm retry 
          crunchTx (retryCntr + 1) txOps
        Just !subseqs -> -- stm success 
          runReaderT (unEdhProg $ forM_ subseqs id) txs
     where
      -- pump values within a single STM transaction
      stmTx :: STM (Maybe [EdhProg ()])
      stmTx = Just <$> forM txOps id
