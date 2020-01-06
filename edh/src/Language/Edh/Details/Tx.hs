
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
  !(progHaltSig :: TChan ()      ) <- newBroadcastTChanIO
  !(forkQueue :: TQueue EdhTxTask) <- newTQueueIO
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
              Just (EdhTxTask !pgs _ !input !task) -> do
                -- got one to fork, prepare state for the descendant thread
                !(descQueue :: TQueue EdhTxTask) <- newTQueueIO
                !(descHaltSig :: TChan ()) <- atomically $ dupTChan progHaltSig
                !reactors                        <- newTVarIO []
                !defers                          <- newTVarIO []
                let !pgsDescendant = pgs { edh'task'queue = descQueue
                                         , edh'reactors   = reactors
                                         , edh'defers     = defers
                    -- the forker should have checked not in tx, enforce here
                                         , edh'in'tx      = False
                                         }
                    !descTaskSource =
                      (Nothing <$ readTChan descHaltSig)
                        `orElse` (tryReadTQueue descQueue)
                -- bootstrap on the descendant thread
                atomically $ writeTQueue
                  descQueue
                  (EdhTxTask pgsDescendant False input task)
                void
                  $ mask_
                  $ forkIOWithUnmask
                  $ \unmask -> catch (unmask $ driveEdhThread descTaskSource)
                                     onDescendantExc
                -- loop another iteration
                forkDescendants haltSig
  -- start forker thread
  forkerHaltSig <- -- forker's sig reader chan must be dup'ed from the broadcast
    -- chan by the main thread, if by the forker thread, it can omit the signal
    -- thus block indefinitely, bcoz racing with main thread's finish
                   atomically $ dupTChan progHaltSig
  void $ mask_ $ forkIOWithUnmask $ \unmask ->
    catch (unmask $ forkDescendants forkerHaltSig) onDescendantExc
  -- broadcast the halt signal after the main thread done anyway
  flip finally (atomically $ writeTChan progHaltSig ()) $ do
    -- prepare program state for main thread
    !(mainQueue :: TQueue EdhTxTask) <- newTQueueIO
    !reactors                        <- newTVarIO []
    !defers                          <- newTVarIO []
    let !scope = contextScope ctx
        !this  = thisObject scope
        !pgs   = EdhProgState { edh'fork'queue = forkQueue
                              , edh'task'queue = mainQueue
                              , edh'reactors   = reactors
                              , edh'defers     = defers
                              , edh'in'tx      = False
                              , edh'context    = ctx
                              }
    -- bootstrap the program on main thread
    atomically $ writeTQueue
      mainQueue
      (EdhTxTask pgs False (this, scope, nil) (const prog))
    -- drive the program from main thread
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
    goSTM !rtc txTask@(EdhTxTask !pgs !wait !input !task) = if wait
      then -- let stm do the retry, for blocking read of a 'TChan' etc.
           atomically $ join (runReaderT (task input) pgs)
      else do -- blocking wait not expected, track stm retries explicitly
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

