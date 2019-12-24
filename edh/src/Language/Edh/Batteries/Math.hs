
module Language.Edh.Batteries.Math where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader

import qualified Data.Text                     as T

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | operator (+)
addProc :: EdhProcedure
addProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr lhExpr $ \(_, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr rhExpr $ \(_, rhVal) -> case rhVal of
      EdhDecimal rhNum -> exitEdhProc exit (scope, EdhDecimal $ lhNum + rhNum)
      _ ->
        throwEdh
          $  EvalError
          $  "Invalid right-hand value for (+) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh
        $  EvalError
        $  "Invalid left-hand value for (+) operation: "
        <> T.pack (show lhVal)
addProc !argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)


-- | operator (-)
subsProc :: EdhProcedure
subsProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr lhExpr $ \(_, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr rhExpr $ \(_, rhVal) -> case rhVal of
      EdhDecimal rhNum -> exitEdhProc exit (scope, EdhDecimal $ lhNum - rhNum)
      _ ->
        throwEdh
          $  EvalError
          $  "Invalid right-hand value for (-) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh
        $  EvalError
        $  "Invalid left-hand value for (-) operation: "
        <> T.pack (show lhVal)
subsProc !argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)


-- | operator (*)
mulProc :: EdhProcedure
mulProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr lhExpr $ \(_, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr rhExpr $ \(_, rhVal) -> case rhVal of
      EdhDecimal rhNum -> exitEdhProc exit (scope, EdhDecimal $ lhNum * rhNum)
      _ ->
        throwEdh
          $  EvalError
          $  "Invalid right-hand value for (*) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh
        $  EvalError
        $  "Invalid left-hand value for (*) operation: "
        <> T.pack (show lhVal)
mulProc !argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)


-- | operator (/)
divProc :: EdhProcedure
divProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr lhExpr $ \(_, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr rhExpr $ \(_, rhVal) -> case rhVal of
      EdhDecimal rhNum -> exitEdhProc exit (scope, EdhDecimal $ lhNum / rhNum)
      _ ->
        throwEdh
          $  EvalError
          $  "Invalid right-hand value for (/) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh
        $  EvalError
        $  "Invalid left-hand value for (/) operation: "
        <> T.pack (show lhVal)
divProc !argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)
