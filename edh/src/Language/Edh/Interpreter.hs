{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Language.Edh.Interpreter where


import           Prelude

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict

import           Data.IORef
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Parser
import           Language.Edh.Runtime
import           Language.Edh.Interpreter.Evaluate


createEdhWorld :: MonadIO m => m EdhWorld
createEdhWorld = liftIO $ do
    e <- newIORef Map.empty
    let srcPos = SourcePos { sourceName   = "<Genesis>"
                           , sourceLine   = mkPos 0
                           , sourceColumn = mkPos 0
                           }
        r = Object { objEntity = e, objClass = c, objSupers = [] }
        c = Class
            { classOuterEntity = e
            , className        = "<root>"
            , classSourcePos   = srcPos
            , classProcedure   = ProcDecl { procedure'args = WildReceiver
                                          , procedure'body = (srcPos, VoidStmt)
                                          }
            }
    opPD  <- newIORef Map.empty
    modus <- newIORef Map.empty
    return $ EdhWorld { worldRoot      = e
                      , worldOperators = opPD
                      , worldModules   = modus
                      }


declareEdhOperators
    :: MonadIO m => EdhWorld -> Text -> [(OpSymbol, Precedence)] -> m ()
declareEdhOperators world declLoc opps = liftIO
    $ atomicModifyIORef' (worldOperators world) declarePrecedence
  where
    declarePrecedence :: OpPrecDict -> (OpPrecDict, ())
    declarePrecedence opPD =
        flip (,) ()
            $ Map.unionWithKey chkCompatible opPD
            $ Map.fromList
            $ flip Prelude.map opps
            $ \(op, p) -> (op, (p, declLoc))
    chkCompatible
        :: OpSymbol
        -> (Precedence, Text)
        -> (Precedence, Text)
        -> (Precedence, Text)
    chkCompatible op (prevPrec, prevDeclLoc) (newPrec, newDeclLoc) =
        if prevPrec /= newPrec
            then throw $ EvalError
                (  "precedence change from "
                <> T.pack (show prevPrec)
                <> " (declared "
                <> prevDeclLoc
                <> ") to "
                <> T.pack (show newPrec)
                <> " (declared "
                <> T.pack (show newDeclLoc)
                <> ") for operator: "
                <> op
                )
            else (prevPrec, prevDeclLoc)


putEdhAttr :: MonadIO m => Entity -> AttrKey -> EdhValue -> m ()
putEdhAttr e k v =
    liftIO $ void $ atomicModifyIORef' e $ \e0 -> return (Map.insert k v e0, ())

putEdhAttrs :: MonadIO m => Entity -> [(AttrKey, EdhValue)] -> m ()
putEdhAttrs e as = liftIO $ void $ atomicModifyIORef' e $ \e0 ->
    return (Map.union ad e0, ())
    where ad = Map.fromList as


runEdhModule
    :: MonadIO m
    => EdhWorld
    -> ModuleId
    -> Text
    -> m (Either InterpretError Module)
runEdhModule world moduId moduSource = liftIO $ do
    -- serialize parsing against 'worldOperators'
    pr <- atomicModifyIORef' (worldOperators world) $ \opPD ->
        let (pr, opPD') =
                    runState (runParserT parseProgram moduId moduSource) opPD
        in  (opPD', pr)
    case pr of
        Left  err   -> return $ Left $ EdhParseError err
        Right stmts -> do
            entity <- newIORef Map.empty
            let modu = Module { moduleEntity = entity, moduleId = moduId }
            runEdhProgram world modu stmts >>= \case
                Left  err -> return $ Left $ EdhEvalError err
                Right _   -> return $ Right modu


evalEdhSource
    :: MonadIO m
    => EdhWorld
    -> Module
    -> Text
    -> m (Either InterpretError EdhValue)
evalEdhSource world modu code = liftIO $ do
    -- serialize parsing against 'worldOperators'
    pr <- atomicModifyIORef' (worldOperators world) $ \opPD ->
        let (pr, opPD') =
                    runState (runParserT parseProgram (moduleId modu) code) opPD
        in  (opPD', pr)
    case pr of
        Left  err   -> return $ Left $ EdhParseError err
        Right stmts -> runEdhProgram world modu stmts >>= \case
            Left  err -> return $ Left $ EdhEvalError err
            Right val -> return $ Right val

