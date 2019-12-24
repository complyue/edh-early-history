{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Interpreter where


import           Prelude

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Control.Concurrent.STM

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
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
runEdhModule world moduId moduSource =
  liftIO
    $
  -- serialize parsing against 'worldOperators'
      bracket (atomically $ takeTMVar wops) (atomically . tryPutTMVar wops)
    $ \opPD ->
        let (pr, opPD') =
                runState (runParserT parseProgram moduId moduSource) opPD
        in
          case pr of
            Left  !err   -> return $ Left $ EdhParseError err
            Right !stmts -> do
              -- release world lock as soon as parsing done successfuly
              atomically $ putTMVar wops opPD'

              -- prepare the module meta data
              let !moduIdAttrVal = EdhString $ T.pack moduId
              !entity <- newTVarIO $ Map.fromList
                [ (AttrByName "__name__", moduIdAttrVal)
                , (AttrByName "__file__", EdhString "<adhoc>")
                ]
              let !moduObj = Object { objEntity = entity
                                    , objClass  = moduleClass world
                                    , objSupers = []
                                    }
                  !modu = Module { moduleObject = moduObj, moduleId = moduId }

              -- run statements from the module
              runEdhProgram world modu stmts >>= \case
                Left  err -> return $ Left $ EdhEvalError err
                Right _   -> return $ Right modu
  where !wops = worldOperators world


evalEdhSource
  :: MonadIO m
  => EdhWorld
  -> Module
  -> Text
  -> m (Either InterpretError EdhValue)
evalEdhSource world modu code =
  liftIO
    $
  -- serialize parsing against 'worldOperators'
      bracket (atomically $ takeTMVar wops) (atomically . tryPutTMVar wops)
    $ \opPD ->
        let (pr, opPD') =
                runState (runParserT parseProgram (moduleId modu) code) opPD
        in  case pr of
              Left  !err   -> return $ Left $ EdhParseError err
              Right !stmts -> do
                -- release world lock as soon as parsing done successfuly
                atomically $ putTMVar wops opPD'

                runEdhProgram world modu stmts >>= \case
                  Left  err -> return $ Left $ EdhEvalError err
                  Right val -> return $ Right val
  where !wops = worldOperators world
