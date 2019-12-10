
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
    let r = Object { objEntity = e, objClass = c, objSupers = [] }
        c = Class
            { classOuterEntity = r
            , className        = "<root>"
            , classSourcePos   = SourcePos { sourceName   = "<Genesis>"
                                           , sourceLine   = mkPos 0
                                           , sourceColumn = mkPos 0
                                           }
            , classProcedure   = []
            }
    opPD  <- newIORef Map.empty
    modus <- newIORef Map.empty
    return $ EdhWorld { worldRoot      = r
                      , worldOperators = opPD
                      , worldModules   = modus
                      }


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
    chkCompatible op (prevPrec, prevDeclLoc) (newPrec, newDeclLoc) =
        if prevPrec /= newPrec
            then error
                (  "precedence change from "
                <> show prevPrec
                <> " (declared "
                <> T.unpack prevDeclLoc
                <> ") to "
                <> show newPrec
                <> " (declared "
                <> T.unpack newDeclLoc
                <> ") for operator: "
                <> show op
                )
            else (prevPrec, prevDeclLoc)


putEdhAttr :: MonadIO m => Object -> AttrKey -> EdhValue -> m ()
putEdhAttr o k v = liftIO $ void $ atomicModifyIORef' (objEntity o) $ \e0 ->
    return (Map.insert k v e0, ())

putEdhAttrs :: MonadIO m => Object -> [(AttrKey, EdhValue)] -> m ()
putEdhAttrs o as = liftIO $ void $ atomicModifyIORef' (objEntity o) $ \e0 ->
    return (Map.union ad e0, ())
    where ad = Map.fromList as


runEdhModule
    :: MonadIO m => EdhWorld -> ModuleId -> Text -> m (Either Text Module)
runEdhModule world moduId moduSource = liftIO $ do
    -- serialize parsing against 'worldOperators'
    pr <- atomicModifyIORef' (worldOperators world) $ \opPD ->
        let (pr, opPD') =
                    runState (runParserT parseModule moduId moduSource) opPD
        in  (opPD', pr)
    case pr of
        Left  pe    -> return $ Left $ T.pack (show pe)
        Right stmts -> do
            entity <- newIORef Map.empty
            let modu = Module
                    { moduleObject = Object
                                         { objEntity = entity
                                         , objClass = objClass (worldRoot world)
                                         , objSupers = []
                                         }
                    , modulePath   = moduId
                    }
            evalProgram modu stmts
            return $ Right modu

