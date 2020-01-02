
module Language.Edh.Details.Tx where

import           Prelude
import           Debug.Trace

import           Control.Monad
import           Control.Monad.Reader

import           Control.Concurrent
import           Control.Concurrent.STM

import           Language.Edh.Details.RtTypes


driveEdhProg :: Context -> EdhProg (STM ()) -> IO ()
driveEdhProg !ctx !prog = do
  -- prepare program state
  !masterTh  <- myThreadId
  !mainQueue <- newTQueueIO
  let !pgs   = EdhProgState masterTh mainQueue ctx False
      !scope = contextScope ctx
      !obj   = thisObject scope
  -- queue the program for bootstrap
  atomically $ writeTQueue mainQueue ((pgs, (obj, scope, nil)), const prog)
-- drive transaction executions from the master thread.
-- exceptions occurred in all threads started by this program will be re-thrown
-- asynchronously to this thread, causing the whole program to abort.
  driveProg mainQueue
 where
  driveProg :: TQueue EdhTxTask -> IO ()
  driveProg mainQueue = atomically (tryReadTQueue mainQueue) >>= \case
    Nothing      -> return () -- program finished 
    Just !txTask -> do
      -- run this task
      goSTM 0 txTask

      -- loop another iteration
      driveProg mainQueue
   where
    goSTM :: Int -> EdhTxTask -> IO ()
    goSTM !rtc txTask@((!pgs, !input), !task) = do
      when (rtc > 0) $ trace (" ** stm retry #" <> show rtc) $ return ()

      stmDone <-
        atomically
        $        (Just <$> join (runReaderT (task input) pgs))
        `orElse` return Nothing
      case stmDone of
        Nothing -> goSTM (rtc + 1) txTask
        Just () -> return ()

