
module Language.Edh.Interpreter where


import           Prelude

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict

import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

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
createEdhWorld :: MonadIO m => m EdhWorld
createEdhWorld = liftIO $ do
    e <- newIORef Map.empty
    let r = Object { objEntity = e, objCtor = c }
        c = Class
            { classOuterEntity = r
            , className        = "<root>"
            , classSourcePos   = SourcePos { sourceName   = "<Genesis>"
                                           , sourceLine   = mkPos 0
                                           , sourceColumn = mkPos 0
                                           }
            }
    opPD <- newIORef Map.empty
    return $ EdhWorld { rootObject = r, operatorDeclarations = opPD }


installEdhAttr :: MonadIO m => Object -> AttrKey -> EdhValue -> m ()
installEdhAttr o k v =
    liftIO $ void $ atomicModifyIORef' (objEntity o) $ \e0 ->
        return (Map.insert k v e0, ())

installEdhAttrs :: MonadIO m => Object -> [(AttrKey, EdhValue)] -> m ()
installEdhAttrs o as =
    liftIO $ void $ atomicModifyIORef' (objEntity o) $ \e0 ->
        return (Map.union ad e0, ())
    where ad = Map.fromList as


runEdhModule
    :: MonadIO m => EdhWorld -> ModuleId -> Text -> m (Either Text Module)
runEdhModule world moduId moduCode = liftIO $ do
    opPD <- readIORef $ operatorDeclarations world
    let (pr, opPD') = runState (runParserT parseModule moduId moduCode) opPD
    atomicModifyIORef' (operatorDeclarations world) $ \_ -> (opPD', ())
    case pr of
        Left  pe    -> return $ Left $ T.pack (show pe)
        Right stmts -> do
            entity <- newIORef Map.empty
            let modu = Module
                    { moduleObject = Object { objEntity = entity
                                            , objCtor   = objCtor root
                                            }
                    , modulePath   = moduId
                    }
            evalProgram modu stmts
            return $ Right modu
    where root = rootObject world

