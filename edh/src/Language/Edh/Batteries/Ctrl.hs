{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Batteries.Ctrl where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader

import qualified Data.Text                     as T

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | operator (->)
branchProc :: EdhProcedure
branchProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit =
  do
    !pgs <- ask
    let !callerCtx@(Context _ _ _ !ctxMatch _) = edh'context pgs
        !callerScope                           = contextScope callerCtx
    case lhExpr of
      PrefixExpr Guard guardedExpr ->
        evalExpr that guardedExpr $ \(_, _, predValue) -> if predValue /= true
          then exitEdhProc exit (that, callerScope, EdhFallthrough)
          else evalExpr that rhExpr $ \(that', scope', rhVal) -> exitEdhProc
            exit
            ( that'
            , scope'
            , case rhVal of
              EdhFallthrough -> EdhFallthrough
              _              -> EdhCaseClose rhVal
            )
      _ -> evalExpr that lhExpr $ \(_, _, lhVal) -> if lhVal /= ctxMatch
        then exitEdhProc exit (that, callerScope, EdhFallthrough)
        else evalExpr that rhExpr $ \(that', scope', rhVal) -> exitEdhProc
          exit
          ( that'
          , scope'
          , case rhVal of
            EdhFallthrough -> EdhFallthrough
            _              -> EdhCaseClose rhVal
          )
branchProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)

