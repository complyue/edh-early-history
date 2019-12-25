
module Language.Edh.Batteries.Math where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader

import qualified Data.Text                     as T

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime

import           Data.Lossless.Decimal


-- | operator (+)
addProc :: EdhProcedure
addProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ _ !exit = do
  !pgs <- ask
  let !callerCtx               = edh'context pgs
      !scope@(Scope _ !this _) = contextScope callerCtx
  evalExpr lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal rhNum ->
        exitEdhProc exit (this, scope, EdhDecimal $ lhNum + rhNum)
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
subsProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ _ !exit = do
  !pgs <- ask
  let !callerCtx               = edh'context pgs
      !scope@(Scope _ !this _) = contextScope callerCtx
  evalExpr lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal rhNum ->
        exitEdhProc exit (this, scope, EdhDecimal $ lhNum - rhNum)
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
mulProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ _ !exit = do
  !pgs <- ask
  let !callerCtx               = edh'context pgs
      !scope@(Scope _ !this _) = contextScope callerCtx
  evalExpr lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal rhNum ->
        exitEdhProc exit (this, scope, EdhDecimal $ lhNum * rhNum)
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
divProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ _ !exit = do
  !pgs <- ask
  let !callerCtx               = edh'context pgs
      !scope@(Scope _ !this _) = contextScope callerCtx
  evalExpr lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal rhNum ->
        exitEdhProc exit (this, scope, EdhDecimal $ lhNum / rhNum)
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
powProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ _ !exit = do
  !pgs <- ask
  let !callerCtx               = edh'context pgs
      !scope@(Scope _ !this _) = contextScope callerCtx
  evalExpr lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal (Decimal rh'd rh'e rh'n) -> if rh'd /= 1
        then
          throwEdh EvalError
          $  "Invalid right-hand value for (**) operation: "
          <> T.pack (show rhVal)
        else exitEdhProc
          exit
          (this, scope, EdhDecimal $ lhNum ^^ (rh'n * 10 ^ rh'e))
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

