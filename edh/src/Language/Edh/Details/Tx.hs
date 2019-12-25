{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Details.Tx where

import           Prelude
import           Debug.Trace

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader

import           Control.Concurrent
import           Control.Concurrent.STM

import           Language.Edh.Details.RtTypes


-- | Throw from an Edh program, be cautious NOT to have any monadic action
-- following such a throw, or it'll silently fail to work out.
throwEdh :: Exception e => e -> EdhProg (STM ())
throwEdh e = return $ throwSTM e


runEdhProg :: Context -> EdhProg (STM ()) -> IO ()
runEdhProg !ctx !prog = do
  -- prepare program state
  !masterTh  <- myThreadId
  !mainQueue <- newTQueueIO
  let !pgs   = EdhProgState masterTh mainQueue ctx False
      !scope = contextScope ctx
      !obj   = thisObject scope
  -- queue the program for bootstrap
  atomically $ writeTQueue mainQueue ((obj, scope, nil), const prog)
-- drive transaction executions from the master thread.
-- exceptions occurred in all threads started by this program will be re-thrown
-- asynchronously to this thread, causing the whole program to abort.
  driveEdhProg pgs
 where
  driveEdhProg :: EdhProgState -> IO ()
  driveEdhProg pgs@(EdhProgState _ !mainQueue _ _) =
    atomically (tryReadTQueue mainQueue) >>= \case
      Nothing      -> return () -- program finished 
      Just !txTask -> do
        -- run this task
        goSTM 0 txTask
        -- loop another iteration
        driveEdhProg pgs
   where
    goSTM :: Int -> EdhTxTask -> IO ()
    goSTM !rtc txTask@(!input, !task) = do
      when (rtc > 0) $ trace (" ** stm retry #" <> show rtc) $ return ()

      stmDone <-
        atomically
        $        (Just <$> join (runReaderT (task input) pgs))
        `orElse` return Nothing
      case stmDone of
        Nothing -> goSTM (rtc + 1) txTask
        Just () -> return ()

