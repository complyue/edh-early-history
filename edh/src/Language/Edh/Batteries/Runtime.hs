{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Batteries.Runtime where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Data.Lossless.Decimal          ( Decimal(..) )

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | operator (<|)
loggingProc :: EdhProcedure
loggingProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit
  = do
    !pgs <- ask
    let !callerCtx@(Context !world _ _) = edh'context pgs
        !callerScope                    = contextScope callerCtx
    evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
      EdhDecimal (Decimal d e n) | d == 1 -> contEdhSTM $ do
        let logLevel = (fromIntegral n :: LogLevel) * 10 ^ e
        (EdhRuntime logger rtLogLevel) <- readTMVar $ worldRuntime world
        if logLevel < rtLogLevel
          then -- drop log msg without even eval it
               exitEdhSTM pgs exit (that, callerScope, nil)
          else -- eval & output
               runEdhProg pgs $ evalExpr that rhExpr $ \(_, _, rhVal) ->
            contEdhSTM $ case rhVal of
              EdhArgsPack pkargs -> do
                logger logLevel pkargs
                exitEdhSTM pgs exit (that, callerScope, nil)
              EdhTuple vals -> do
                logger logLevel $ ArgsPack vals Map.empty
                exitEdhSTM pgs exit (that, callerScope, nil)
              _ -> do
                logger logLevel $ ArgsPack [rhVal] Map.empty
                exitEdhSTM pgs exit (that, callerScope, nil)
      _ -> throwEdh EvalError $ "Invalid log level: " <> T.pack (show lhVal)
loggingProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)

