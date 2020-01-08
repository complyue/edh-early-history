
-- | Edh Host Interface
--
-- With Haskell as the host language, Edh as the surface language,
-- this defines the interface for host code in Haskell to create
-- & control embedded Edh worlds, and to splice host (typically
-- side-effects free, i.e. pure, and fast-in-machine-speed)
-- functions, wrapped as host procedures, with procedures written
-- in Edh, those do arbitrary manipulations on arbitrary objects
-- in the world, well, less speedy as being interpreted.
module Language.Edh.EHI
  (
    -- * Language infrastructure
    -- ** Exceptions
    InterpretError(..)
    -- ** Logging interface
  , EdhLogger(..)
  , LogLevel
  , defaultEdhLogger
    -- ** Host arts making tools
  , mkSymbol
  , mkHostProc
  , mkHostOper
  , declareEdhOperators
  , installEdhAttrs
  , installEdhAttr
    -- ** Value system
  , edhTypeOf
  , edhValueStr
  , edhValueNull
    -- ** Object system
  , lookupEdhCtxAttr
  , lookupEdhObjAttr
  , resolveEdhCtxAttr
  , resolveEdhObjAttr
  , resolveEdhInstance
    -- ** Runtime system
  , EdhWorld(..)
  , EdhRuntime(..)
  , EdhProg(..)
  , EdhProgState(..)
  , EdhTxTask(..)
  , Context(..)
  , Scope(..)
  , runEdhProg
  , forkEdh
    -- ** Event processing
  , newEventSink
  , subscribeEvents
  , publishEvent
    -- ** Runtime error
  , getEdhErrorContext
  , throwEdh
  , throwEdhSTM
    -- ** CPS helpers
  , contEdhSTM
  , exitEdhSTM
  , exitEdhProc
  , waitEdhSTM
  , edhNop

    -- * Monadic API
  , EdhProgram
  , EdhSession
  , runEdh
  , runEdhWithoutBatteries
  , runEdhSession
  , evalEdh

    -- * IO API
  , createEdhWorld
  , installEdhBatteries
  , runEdhModule
  , evalEdhSource

    -- * Data types
    -- ** Host artifacts for the splice
  , HostProcedure(..)
  , EdhGenrCaller(..)
  , Class(..)
  , Method(..)
  , Operator(..)
  , GenrDef(..)
  , Interpreter(..)
    -- ** Event infrastructure
  , EventSink(..)
    -- ** End values
  , EdhValue(..)
  , nil
  , true
  , false
  , nan
  , inf
  , D.Decimal(..)
    -- ** Structured values
  , Symbol(..)
  , Entity(..)
  , AttrKey(..)
  , Dict(..)
  , ItemKey(..)
  , List(..)
  , ArgsPack(..)
  , Object(..)
    -- ** Reflectives
  , module AST
  )
where

import           Prelude

import           Control.Exception
import           Control.Monad.Reader

import           Data.Text                     as T

import qualified Data.Lossless.Decimal         as D

import           Language.Edh.Control
import           Language.Edh.Interpreter
import           Language.Edh.Batteries
import           Language.Edh.Runtime
import           Language.Edh.Event
import           Language.Edh.AST              as AST


evalEdh :: Text -> EdhSession EdhValue
evalEdh code = do
  (world, modu) <- ask
  liftIO $ evalEdhSource world modu code >>= \case
    Left  err -> throwIO err
    Right v   -> return v

runEdhSession
  :: ModuleId -> Text -> EdhSession a -> EdhProgram (Either InterpretError a)
runEdhSession moduId moduSource (ReaderT f) = do
  world <- ask
  runEdhModule world moduId moduSource >>= \case
    Left  err  -> return $ Left err
    Right modu -> liftIO $ tryJust Just $ f (world, modu)


runEdh :: MonadIO m => EdhLogger -> EdhProgram a -> m a
runEdh !logger (ReaderT f) = liftIO $ do
  world <- createEdhWorld logger
  installEdhBatteries world
  f world

runEdhWithoutBatteries :: MonadIO m => EdhLogger -> EdhProgram a -> m a
runEdhWithoutBatteries !logger (ReaderT f) =
  liftIO $ createEdhWorld logger >>= f


type EdhSession a = ReaderT (EdhWorld, Object) IO a

type EdhProgram a = ReaderT EdhWorld IO a

