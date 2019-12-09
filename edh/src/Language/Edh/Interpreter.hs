
module Language.Edh.Interpreter where


import           Prelude                       as Prelude

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
    return $ EdhWorld { worldRoot = r, worldOperators = opPD }


declareEdhOperators
    :: MonadIO m => EdhWorld -> Text -> [(OpSymbol, Precedence)] -> m ()
declareEdhOperators world declLoc opd = liftIO
    $ atomicModifyIORef' (worldOperators world) updatePrecedence
  where
    updatePrecedence :: OpPrecDict -> (OpPrecDict, ())
    updatePrecedence opPD =
        (flip (,) ())
            $ Map.unionWithKey chkCompatible opPD
            $ Map.fromList
            $ flip Prelude.map opd
            $ \(op, p) -> (op, (p, declLoc))

    chkCompatible
        :: OpSymbol
        -> (Precedence, Text)
        -> (Precedence, Text)
        -> (Precedence, Text)
    chkCompatible op (prevPrec, prevDeclLoc) (newPrec, _) =
        if prevPrec /= newPrec
            then error
                (  "invalid precedence change from "
                <> show prevPrec
                <> " to "
                <> show newPrec
                <> " for operator: "
                <> show op
                )
            else (prevPrec, prevDeclLoc)


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
    -- seralize parsing against 'worldOperators'
    pr <- atomicModifyIORef' (worldOperators world) $ \opPD ->
        let (pr, opPD') =
                    runState (runParserT parseModule moduId moduCode) opPD
        in  (opPD', pr)
    case pr of
        Left  pe    -> return $ Left $ T.pack (show pe)
        Right stmts -> do
            entity <- newIORef Map.empty
            let modu = Module
                    { moduleObject = Object
                                         { objEntity = entity
                                         , objCtor   = objCtor (worldRoot world)
                                         }
                    , modulePath   = moduId
                    }
            evalProgram modu stmts
            return $ Right modu

