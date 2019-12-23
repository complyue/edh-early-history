{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Edh Host Interface
--
-- With Haskell as the host language, Edh as the guest language,
-- this defines the interface for host code in Haskell to create
-- & control embed Edh worlds, and to splice host (typically
-- side-effects free, i.e. pure, and performant) functions in
-- Haskell, with object-orient, while highly mutable (thus
-- world-changing), but not-so-performant (as being interpreted)
-- procedures in Edh.
module Language.Edh.EHI
  (
    -- * Monadic API
    EdhInterpreter
  , EdhSession
    -- ** Interpreter entry point    
  , runEdh
  , runEdhWithoutBatteries
    -- ** Session entry point
  , runEdhSession
    -- ** The eval function
  , evalEdh

    -- ** Exceptions
  , InterpretError(..)

    -- * Pure API
  , createEdhWorld
  , installEdhBatteries
  , runEdhModule
  , evalEdhSource

    -- * Data types
    -- ** End values
  , EdhValue(..)
  , nil
  , true
  , false
  , nan
  , inf
  , D.Decimal(..)
    -- ** Reflection
  , EdhWorld
  , Symbol
  , Entity
  , AttrKey
  , Dict
  , ItemKey
  , Object
  , Class
  , Method
  , Module
  , Iterator
    -- ** World changing tools
  , declareEdhOperators
  , installEdhAttrs
  , installEdhAttr
  )
where

import           Prelude

import           Control.Exception
import           Control.Monad
import           Control.Monad.Fail
import           Control.Monad.Reader

import           Data.Text                     as T

import qualified Data.Lossless.Decimal         as D

import           Language.Edh.Control
import           Language.Edh.Interpreter
import           Language.Edh.Batteries
import           Language.Edh.Runtime
import           Language.Edh.AST


evalEdh :: Text -> EdhSession EdhValue
evalEdh code = do
  (world, modu) <- ask
  liftIO $ evalEdhSource world modu code >>= \case
    Left  err -> throwIO err
    Right v   -> return v

runEdhSession
  :: ModuleId
  -> Text
  -> EdhSession a
  -> EdhInterpreter (Either InterpretError a)
runEdhSession moduId moduSource (EdhSession (ReaderT f)) = do
  world <- ask
  runEdhModule world moduId moduSource >>= \case
    Left  err  -> return $ Left err
    Right modu -> liftIO $ tryJust Just $ f (world, modu)


runEdh :: MonadIO m => EdhInterpreter a -> m a
runEdh (EdhInterpreter (ReaderT f)) = liftIO $ do
  world <- createEdhWorld
  installEdhBatteries world
  f world

runEdhWithoutBatteries :: MonadIO m => EdhInterpreter a -> m a
runEdhWithoutBatteries (EdhInterpreter (ReaderT f)) =
  liftIO $ createEdhWorld >>= f


newtype EdhSession a = EdhSession { unEdhS :: ReaderT (EdhWorld,Module) IO a }
    deriving (Functor, Applicative, Monad,
        MonadReader (EdhWorld, Module),
        MonadIO, MonadFail)

newtype EdhInterpreter a = EdhInterpreter { unEdh :: ReaderT EdhWorld IO a }
    deriving (Functor, Applicative, Monad,
        MonadReader EdhWorld,
        MonadIO, MonadFail)

