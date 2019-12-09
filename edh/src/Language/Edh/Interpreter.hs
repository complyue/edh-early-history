
module Language.Edh.Interpreter where


import           Prelude

import           Control.Monad
import           Control.Monad.IO.Class

import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec                ( SourcePos(..)
                                                , mkPos
                                                )

import           Data.Lossless.Decimal         as D

import           Language.Edh.AST
import           Language.Edh.Parser
import           Language.Edh.Parser.Details
import           Language.Edh.Runtime
import           Language.Edh.Interpreter.Evaluate


-- | Create a self-contained object (its class appears defined within itself),
-- build the builtins and install to the object.
--
-- This is a starting point of an Edh world.
createEdhWorld :: MonadIO m => m Object
createEdhWorld = do
    e <- liftIO $ newIORef Map.empty
    let r = Object { objEntity = e, objCtor = c }
        c = Class
            { classOuterEntity = r
            , className        = "<builtins>"
            , classSourcePos   = SourcePos { sourceName   = "<Genesis>"
                                           , sourceLine   = mkPos 0
                                           , sourceColumn = mkPos 0
                                           }
            }
    return r


installEdhAttr :: MonadIO m => Object -> AttrKey -> EdhValue -> m ()
installEdhAttr o k v =
    liftIO $ void $ atomicModifyIORef' (objEntity o) $ \e0 ->
        return (Map.insert k v e0, ())

installEdhAttrs :: MonadIO m => Object -> [(AttrKey, EdhValue)] -> m ()
installEdhAttrs o as =
    liftIO $ void $ atomicModifyIORef' (objEntity o) $ \e0 ->
        return (Map.union ad e0, ())
    where ad = Map.fromList as


