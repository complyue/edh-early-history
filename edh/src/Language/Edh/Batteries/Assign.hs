
module Language.Edh.Batteries.Assign where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader

import qualified Data.Text                     as T

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | operator (=)
assignProc :: EdhProcedure
assignProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  pgs <- ask
  let inTxAfter = edh'in'tx pgs
  -- execution of the assignment always in a tx for atomicity
  local (\pgs' -> pgs' { edh'in'tx = True }) $ evalExpr rhExpr $ assignEdhTarget
    inTxAfter
    lhExpr
    exit
assignProc !argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)

