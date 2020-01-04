
module Language.Edh.Details.Tx where

import           Prelude
import           Debug.Trace

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader

import           Control.Concurrent
import           Control.Concurrent.STM

import           Language.Edh.Control
import           Language.Edh.Details.RtTypes


-- | Edh follows GHC's program termination criteria that the main thread
-- decides all. see:
--   https://hackage.haskell.org/package/base/docs/Control-Concurrent.html
driveEdhProgram :: Context -> EdhProg (STM ()) -> IO ()
driveEdhProgram !ctx !prog = do
  -- check async exception mask state
  getMaskingState >>= \case
    Unmasked -> return ()
    _        -> throwIO
      $ UsageError "Edh program should not run with async exceptions masked"

  -- prepare program environment
  !mainThId <- myThreadId
  let onDescendantExc :: SomeException -> IO ()
      onDescendantExc e = case asyncExceptionFromException e of
        Just (asyncExc :: SomeAsyncException) ->
          -- todo special handling here ?
          throwTo mainThId asyncExc
        _ -> throwTo mainThId e
  progHaltSig <- newBroadcastTChanIO
  !forkQueue  <- newTQueueIO
  let
    forkDescendants :: TChan () -> IO ()
    forkDescendants haltSig =
      atomically
          (        (Nothing <$ readTChan haltSig)
          `orElse` (Just <$> readTQueue forkQueue)
          )
        >>= \case
              Nothing -> -- program halted, done
                return ()
              Just ((!pgs, !input), !task) -> do
                -- got one to fork, prepare state for the descendant thread
                descQueue                 <- newTQueueIO
                (descHaltSig :: TChan ()) <- atomically $ dupTChan progHaltSig
                let !pgsDescendant = pgs { edh'task'queue = descQueue }
                    !descTaskSource =
                      (Nothing <$ readTChan descHaltSig)
                        `orElse` (Just <$> readTQueue descQueue)
                -- bootstrap on the descendant thread
                atomically
                  $ writeTQueue descQueue ((pgsDescendant, input), task)
                void
                  $ mask_
                  $ forkIOWithUnmask
                  $ \unmask -> catch (unmask $ driveEdhThread descTaskSource)
                                     onDescendantExc
                -- loop another iteration
                forkDescendants haltSig
  -- start forker thread
  void $ mask_ $ forkIOWithUnmask $ \unmask -> catch
    (unmask $ atomically (dupTChan progHaltSig) >>= forkDescendants)
    onDescendantExc

  -- prepare program state for main thread
  !mainQueue <- newTQueueIO
  let !scope = contextScope ctx
      !obj   = thisObject scope
      !pgs   = EdhProgState { edh'fork'queue = forkQueue
                            , edh'task'queue = mainQueue
                            , edh'in'tx      = False
                            , edh'context    = ctx
                            }
  -- broadcast the halt signal after the main thread done anyway
  flip finally (atomically $ writeTChan progHaltSig ()) $ do
    -- bootstrap the program on main thread
    atomically $ writeTQueue mainQueue ((pgs, (obj, scope, nil)), const prog)
    driveEdhThread $ tryReadTQueue mainQueue
 where
  driveEdhThread :: STM (Maybe EdhTxTask) -> IO ()
  driveEdhThread !taskSource = atomically taskSource >>= \case
    Nothing      -> return ()
    Just !txTask -> do
      -- run this task
      goSTM 0 txTask
      -- loop another iteration
      driveEdhThread taskSource
   where
    goSTM :: Int -> EdhTxTask -> IO ()
    goSTM !rtc txTask@((!pgs, !input), !task) = do
      when -- todo increase the threshold of reporting?
        (rtc > 0)
        do
          -- trace out the retries so the end users can be aware of them
          tid <- myThreadId
          trace (" ** " <> show tid <> " stm retry #" <> show rtc) $ return ()

      -- the weird formatting below comes from brittany,
      -- not the author's preference
      (        atomically
        $        (Just <$> join (runReaderT (task input) pgs))
        `orElse` return Nothing
        )
        >>= \case
              Nothing -> goSTM (rtc + 1) txTask
              Just () -> return ()

