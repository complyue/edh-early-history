{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Batteries.Ctrl where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Data.Lossless.Decimal          ( Decimal(..) )

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | operator (->)
branchProc :: EdhProcedure
branchProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit =
  do
    !pgs <- ask
    let !callerCtx@(Context !world _ _ (StmtSrc (srcPos, _))) = edh'context pgs
        !callerScope = contextScope callerCtx
    evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
      _ -> throwEdh EvalError $ "Invalid  xxx : " <> T.pack (show lhVal)
branchProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)

