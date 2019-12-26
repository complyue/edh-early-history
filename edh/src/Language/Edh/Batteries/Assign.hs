
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
assignProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit =
  do
    pgs <- ask
    -- execution of the assignment always in a tx for atomicity
    local (\pgs' -> pgs' { edh'in'tx = True })
      $ evalExpr that rhExpr
      $ assignEdhTarget pgs that lhExpr exit
assignProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)

