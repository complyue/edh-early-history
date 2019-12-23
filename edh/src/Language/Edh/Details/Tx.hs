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


throwEdh :: Exception e => e -> EdhProg (STM ())
throwEdh e = return $ throwSTM e


runEdhProg :: Context -> EdhProg (STM ()) -> IO ()
runEdhProg !ctx !prog = do
  -- prepare program state
  !masterTh  <- myThreadId
  !mainQueue <- newTQueueIO
  let !pgs = EdhProgState masterTh mainQueue ctx False
  -- queue the program for bootstrap
  atomically $ writeTQueue mainQueue ((contextScope ctx, nil), const prog)
-- drive transaction executions from the master thread.
-- exceptions occurred in all threads started by this program will be re-thrown
-- asynchronously to this thread, causing the whole program to abort.
  driveEdhProg pgs
 where
  driveEdhProg :: EdhProgState -> IO ()
  driveEdhProg pgs@(EdhProgState _ !mainQueue _ _) =
    atomically (tryReadTQueue mainQueue) >>= \case
      Nothing   -> return () -- program finished 
      Just !txo -> do
        -- run this tx
        goSTM 0 txo
        -- loop another iteration
        driveEdhProg pgs
   where
    goSTM :: Int -> EdhTxOp -> IO ()
    goSTM !rtc txo@(!input, !op) = do
      when (rtc > 0) $ trace (" ** stm retry #" <> show rtc) $ return ()

      stmDone <-
        atomically
        $        (Just <$> join (runReaderT (op input) pgs))
        `orElse` return Nothing
      case stmDone of
        Nothing -> goSTM (rtc + 1) txo
        Just () -> return ()

