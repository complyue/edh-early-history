{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Language.Edh.Interpreter where


import           Prelude

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
            let moduObj = Object { objEntity = entity
                                 , objClass  = moduleClass world
                                 , objSupers = []
                                 }
                modu = Module { moduleObject = moduObj, moduleId = moduId }
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

