
module Language.Edh.Details.Tx where

import           Prelude
import           Debug.Trace

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader

import           Control.Concurrent
import           Control.Concurrent.STM

import qualified Data.Map.Strict               as Map
import           Data.List.NonEmpty             ( (<|) )

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes
import           Language.Edh.Details.Evaluate


-- | Edh follows GHC's program termination criteria that the main thread
-- decides all. see:
--   https://hackage.haskell.org/package/base/docs/Control-Concurrent.html
driveEdhProgram :: Context -> EdhProg (STM ()) -> IO ()
driveEdhProgram !progCtx !prog = do
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
              Just (EdhTxTask !pgsFork _ !input !task) -> do
                -- got one to fork, prepare state for the descendant thread
                !(descQueue :: TQueue EdhTxTask) <- newTQueueIO
                !(descHaltSig :: TChan ()) <- atomically $ dupTChan progHaltSig
                !reactors                        <- newTVarIO []
                !defers                          <- newTVarIO []
                let !pgsDescendant = pgsFork { edh'task'queue = descQueue
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
    let !scopeAtBoot = contextScope progCtx
        !thisAtBoot  = thisObject scopeAtBoot
        !pgsAtBoot   = EdhProgState { edh'fork'queue = forkQueue
                                    , edh'task'queue = mainQueue
                                    , edh'reactors   = reactors
                                    , edh'defers     = defers
                                    , edh'in'tx      = False
                                    , edh'context    = progCtx
                                    }
    -- bootstrap the program on main thread
    atomically $ writeTQueue
      mainQueue
      (EdhTxTask pgsAtBoot False (thisAtBoot, scopeAtBoot, nil) (const prog))
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
  goSTM :: Int -> EdhTxTask -> IO ()
  goSTM !rtc txTask@(EdhTxTask !pgsThread !wait !input !task) = if wait
    then -- let stm do the retry, for blocking read of a 'TChan' etc.
         atomically stmJob
    else do -- blocking wait not expected, track stm retries explicitly
      when -- todo increase the threshold of reporting?
           (rtc > 0) $ do
        -- trace out the retries so the end users can be aware of them
        tid <- myThreadId
        trace (" 🌀 " <> show tid <> " stm retry #" <> show rtc) $ return ()

      atomically ((Just <$> stmJob) `orElse` return Nothing) >>= \case
              -- stm failed, do a tracked retry
        Nothing -> goSTM (rtc + 1) txTask
        -- stm done
        Just () -> return ()
   where
    stmJob :: STM ()
    stmJob = readTVar (edh'reactors pgsThread) >>= doReactors

    doReactors :: [(TChan EdhValue, Context, ArgsReceiver, StmtSrc)] -> STM ()
    doReactors [] = join (runReaderT (task input) pgsThread)
    doReactors ((!chan, reactorCtx, argsRcvr, stmt) : restReactors) =
      tryReadTChan chan >>= \case
        Nothing -> doReactors restReactors
        Just ev -> do
          let
            !pk = case ev of
              EdhArgsPack pk_ -> pk_
              _               -> ArgsPack [ev] Map.empty
            !reactorScope = contextScope reactorCtx
            !reactorThis  = thisObject reactorScope
            -- the reactor code must run in a transaction, or if it comprises
            -- of more cycles, it'll mess up with normal transaction tasks
            !pgsReactor =
              pgsThread { edh'context = reactorCtx, edh'in'tx = True }
          runEdhProg pgsReactor
            $ recvEdhArgs reactorCtx argsRcvr pk
            $ \(_, _, scopeObj) -> case scopeObj of
                EdhObject (Object rcvd'ent _ _) ->
                  local
                      (const pgsReactor
                        { edh'context =
                          reactorCtx
                            { callStack =
                              reactorScope { scopeEntity = rcvd'ent }
                                <| callStack reactorCtx
                            }
                        }
                      )
                    $ evalStmt reactorThis stmt
                    $ \(_, _, reactorRtn) -> case reactorRtn of
                        EdhBreak -> -- terminate this thread
                          return $ return ()
                        _ -> -- continue stm job
                          contEdhSTM $ doReactors restReactors
                _ -> error "bug"

