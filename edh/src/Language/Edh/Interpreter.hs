
module Language.Edh.Interpreter where


import           Prelude
-- import           Debug.Trace

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
  -> m (Either InterpretError Object)
runEdhModule world moduId moduSource =
  liftIO
    $
      -- serialize parsing against 'worldOperators'
      bracket (atomically $ takeTMVar wops) (atomically . tryPutTMVar wops)
    $ \opPD -> do
        -- parse module source
        let (pr, opPD') = runState
              (runParserT parseProgram (T.unpack moduId) moduSource)
              opPD
        case pr of
          Left  !err   -> return $ Left $ EdhParseError err
          Right !stmts -> do
            -- release world lock as soon as parsing done successfuly
            atomically $ putTMVar wops opPD'
            -- prepare the module meta data
            !moduEntity <- newTVarIO $ Map.fromList
              [ (AttrByName "__name__", EdhString moduId)
              , (AttrByName "__file__", EdhString "<adhoc>")
              ]
            !moduSupers <- newTVarIO []
            let !modu = Object { objEntity = moduEntity
                               , objClass  = moduleClass world
                               , objSupers = moduSupers
                               }
            -- run statements from the module
            runEdhProgram world modu stmts >>= \case
              Left  err -> return $ Left err
              Right _   -> return $ Right modu
  where !wops = worldOperators world


evalEdhSource
  :: MonadIO m
  => EdhWorld
  -> Object
  -> Text
  -> m (Either InterpretError EdhValue)
evalEdhSource world modu code = liftIO $ do
  mem <- readTVarIO (objEntity modu)
  let moduName = T.unpack $ case Map.lookup (AttrByName "__name__") mem of
        Just (EdhString name) -> name
        _                     -> "<adhoc>"
  -- serialize parsing against 'worldOperators'
  bracket (atomically $ takeTMVar wops) (atomically . tryPutTMVar wops)
    $ \opPD ->
        let (pr, opPD') = runState (runParserT parseProgram moduName code) opPD
        in  case pr of
              Left  !err   -> return $ Left $ EdhParseError err
              Right !stmts -> do

                -- trace
                --     ( (" ** parsed: \n" ++)
                --     $ unlines
                --     $ (<$> stmts) \(StmtSrc (sp, stmt)) ->
                --         sourcePosPretty sp ++ "\n  " ++ show stmt
                --     )
                --   $ return ()

                -- release world lock as soon as parsing done successfuly
                atomically $ putTMVar wops opPD'

                runEdhProgram world modu stmts >>= \case
                  Left  err -> return $ Left err
                  Right val -> return $ Right val
  where !wops = worldOperators world
