
module Language.Edh.Batteries.Math where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Text                     as T

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime

import           Data.Lossless.Decimal


-- | operator (+)
addProc :: EdhProcedure
addProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr that rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal rhNum ->
        exitEdhProc exit (that, scope, EdhDecimal $ lhNum + rhNum)
      _ ->
        throwEdh EvalError
          $  "Invalid right-hand value for (+) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh EvalError
        $  "Invalid left-hand value for (+) operation: "
        <> T.pack (show lhVal)
addProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (-)
subsProc :: EdhProcedure
subsProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit =
  do
    !pgs <- ask
    let !callerCtx = edh'context pgs
        !scope     = contextScope callerCtx
    evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
      EdhDecimal lhNum -> evalExpr that rhExpr $ \(_, _, rhVal) ->
        case rhVal of
          EdhDecimal rhNum ->
            exitEdhProc exit (that, scope, EdhDecimal $ lhNum - rhNum)
          _ ->
            throwEdh EvalError
              $  "Invalid right-hand value for (-) operation: "
              <> T.pack (show rhVal)
      _ ->
        throwEdh EvalError
          $  "Invalid left-hand value for (-) operation: "
          <> T.pack (show lhVal)
subsProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (*)
mulProc :: EdhProcedure
mulProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr that rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal rhNum ->
        exitEdhProc exit (that, scope, EdhDecimal $ lhNum * rhNum)
      _ ->
        throwEdh EvalError
          $  "Invalid right-hand value for (*) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh EvalError
        $  "Invalid left-hand value for (*) operation: "
        <> T.pack (show lhVal)
mulProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (/)
divProc :: EdhProcedure
divProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr that rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal rhNum ->
        exitEdhProc exit (that, scope, EdhDecimal $ lhNum / rhNum)
      _ ->
        throwEdh EvalError
          $  "Invalid right-hand value for (/) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh EvalError
        $  "Invalid left-hand value for (/) operation: "
        <> T.pack (show lhVal)
divProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (**)
powProc :: EdhProcedure
powProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr that rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal (Decimal rh'd rh'e rh'n) -> if rh'd /= 1
        then
          throwEdh EvalError
          $  "Invalid right-hand value for (**) operation: "
          <> T.pack (show rhVal)
        else exitEdhProc
          exit
          (that, scope, EdhDecimal $ lhNum ^^ (rh'n * 10 ^ rh'e))
      _ ->
        throwEdh EvalError
          $  "Invalid right-hand value for (**) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh EvalError
        $  "Invalid left-hand value for (**) operation: "
        <> T.pack (show lhVal)
powProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (~=)
valEqProc :: EdhProcedure
valEqProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit =
  do
    !pgs <- ask
    let
      !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
      cmpVals :: EdhValue -> EdhValue -> STM ()
      cmpVals lhVal rhVal = do
        if lhVal == rhVal
          then exitEdhSTM pgs exit (that, scope, true)
          else case lhVal of
            EdhList (List lhll) -> do
              case rhVal of
                EdhList (List rhll) -> do
                  lhl <- readTVar lhll
                  rhl <- readTVar rhll
                  exitEdhSTM pgs exit (that, scope, EdhBool $ lhl == rhl)
                _ -> exitEdhSTM pgs exit (that, scope, false)
            EdhDict (Dict lhd) -> do
              case rhVal of
                EdhDict (Dict rhd) -> do
                  lhm <- readTVar lhd
                  rhm <- readTVar rhd
                  exitEdhSTM pgs exit (that, scope, EdhBool $ lhm == rhm)
                _ -> exitEdhSTM pgs exit (that, scope, false)
            _ -> exitEdhSTM pgs exit (that, scope, false)
    evalExpr that lhExpr
      $ \(_, _, lhVal) -> evalExpr that rhExpr $ \(_, _, rhVal) ->
          if lhVal == rhVal
            then exitEdhProc exit (that, scope, true)
            else contEdhSTM $ cmpVals lhVal rhVal
valEqProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (==)
idEqProc :: EdhProcedure
idEqProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit =
  do
    !pgs <- ask
    let !callerCtx = edh'context pgs
        !scope     = contextScope callerCtx
    evalExpr that lhExpr $ \(_, _, lhVal) ->
      evalExpr that rhExpr $ \(_, _, rhVal) ->
        exitEdhProc exit (that, scope, EdhBool $ lhVal == rhVal)
idEqProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)

