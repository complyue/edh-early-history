
module Language.Edh.Event where

import           Prelude

import           Control.Concurrent.STM
import           Control.Monad.STM

import           Language.Edh.Details.RtTypes


newEventSink :: STM EventSink
newEventSink = do
  mrv  <- newTVar nil
  chan <- newBroadcastTChan
  return EventSink { evs'mrv = mrv, evs'chan = chan }

mostRecentEvent :: MonadIO m => EventSink -> m EdhValue
mostRecentEvent _sink@(EventSink mrv _chan) = liftIO $ readTVarIO mrv

publishEvent :: EventSink -> EdhValue -> STM ()
publishEvent (EventSink mrv chan) val = do
  writeTVar mrv val
  writeTChan chan val

