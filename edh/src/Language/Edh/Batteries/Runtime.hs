
module Language.Edh.Batteries.Runtime where

import           Prelude
-- import           Debug.Trace

import           GHC.Conc                       ( unsafeIOToSTM )

import           Control.Monad.Reader
import           Control.Concurrent
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Data.Lossless.Decimal          ( Decimal(..) )

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | operator (<|)
loggingProc :: EdhProcedure
loggingProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) !that _ !exit
  = do
    !pgs <- ask
    let !callerCtx@(Context !world _ _ _ (StmtSrc (srcPos, _))) =
          edh'context pgs
        !callerScope = contextScope callerCtx
    evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
      EdhDecimal (Decimal d e n) | d == 1 -> contEdhSTM $ do
        let logLevel = (fromIntegral n :: LogLevel) * 10 ^ e
        (EdhRuntime logger rtLogLevel) <- readTMVar $ worldRuntime world
        if logLevel < rtLogLevel
          then -- drop log msg without even eval it
               exitEdhSTM pgs exit (that, callerScope, nil)
          else runEdhProg pgs $ evalExpr that rhExpr $ \(_, _, rhVal) -> do
            let srcLoc = if rtLogLevel <= 20
                  then -- with source location info
                       Just $ sourcePosPretty srcPos
                  else -- no source location info
                       Nothing
            contEdhSTM $ case rhVal of
              EdhArgsPack pkargs -> do
                logger logLevel srcLoc pkargs
                exitEdhSTM pgs exit (that, callerScope, nil)
              EdhTuple vals -> do
                logger logLevel srcLoc $ ArgsPack vals Map.empty
                exitEdhSTM pgs exit (that, callerScope, nil)
              _ -> do
                logger logLevel srcLoc $ ArgsPack [rhVal] Map.empty
                exitEdhSTM pgs exit (that, callerScope, nil)
      _ -> throwEdh EvalError $ "Invalid log level: " <> T.pack (show lhVal)
loggingProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


keepNotify :: Int -> EdhGenrCaller -> STM ()
keepNotify !delayMicros !genr'caller@(!pgs', !iter'cb) = do
  unsafeIOToSTM $ threadDelay delayMicros
  -- TODO return timestamp now instead of nil
  runEdhProg pgs' $ iter'cb (this, scope, nil) $ \_ ->
    keepNotify delayMicros genr'caller
 where
  !scope = contextScope $ edh'context pgs'
  !this  = thisObject scope


-- | host generator runtime.everyMicros(n) - with fixed interval
rtEveryMicrosProc :: EdhProcedure
rtEveryMicrosProc !argsSender !that _ _ = ask >>= \pgs -> do
  case generatorCaller $ edh'context pgs of
    Nothing          -> throwEdh EvalError "Can only be called as generator"
    Just genr'caller -> case argsSender of
      PackSender [SendPosArg !nExpr] -> evalExpr that nExpr $ \case
        (_, _, EdhDecimal (Decimal d e n)) | d == 1 ->
          contEdhSTM $ keepNotify (fromIntegral n * 10 ^ e) genr'caller
        nVal -> throwEdh EvalError $ "Invalid argument: " <> T.pack (show nVal)
      _ ->
        throwEdh EvalError $ "Invalid argument: " <> T.pack (show argsSender)


-- | host generator runtime.everyMillis(n) - with fixed interval
rtEveryMillisProc :: EdhProcedure
rtEveryMillisProc !argsSender !that _ _ = ask >>= \pgs -> do
  case generatorCaller $ edh'context pgs of
    Nothing          -> throwEdh EvalError "Can only be called as generator"
    Just genr'caller -> case argsSender of
      PackSender [SendPosArg !nExpr] -> evalExpr that nExpr $ \case
        (_, _, EdhDecimal (Decimal d e n)) | d == 1 ->
          contEdhSTM $ keepNotify (fromIntegral n * 10 ^ (e + 3)) genr'caller
        nVal -> throwEdh EvalError $ "Invalid argument: " <> T.pack (show nVal)
      _ ->
        throwEdh EvalError $ "Invalid argument: " <> T.pack (show argsSender)


-- | host generator runtime.everySeconds(n) - with fixed interval
rtEverySecondsProc :: EdhProcedure
rtEverySecondsProc !argsSender !that _ _ = ask >>= \pgs -> do
  case generatorCaller $ edh'context pgs of
    Nothing          -> throwEdh EvalError "Can only be called as generator"
    Just genr'caller -> case argsSender of
      PackSender [SendPosArg !nExpr] -> evalExpr that nExpr $ \case
        (_, _, EdhDecimal (Decimal d e n)) | d == 1 ->
          contEdhSTM $ keepNotify (fromIntegral n * 10 ^ (e + 6)) genr'caller
        nVal -> throwEdh EvalError $ "Invalid argument: " <> T.pack (show nVal)
      _ ->
        throwEdh EvalError $ "Invalid argument: " <> T.pack (show argsSender)

