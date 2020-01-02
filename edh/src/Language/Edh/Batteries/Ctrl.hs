
module Language.Edh.Batteries.Ctrl where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader

import qualified Data.Text                     as T

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | operator (->) - the brancher, if its left-hand matches, early stop its
-- enclosing code block (commonly a case-of block, but other blocks as well),
-- with eval-ed result of its right-hand, unless the right-hand result is
-- `fallthrough`
branchProc :: EdhProcedure
branchProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) that _ !exit =
  do
    !pgs <- ask
    let !callerCtx@(Context _ _ _ !ctxMatch _) = edh'context pgs
        !callerScope                           = contextScope callerCtx
    case lhExpr of
      -- | recognize `_` as similar to the wildcard pattern match in Haskell,
      -- it always matches
      AttrExpr (DirectRef (NamedAttr "_")) ->
        evalExpr that rhExpr $ \(that', scope', rhVal) -> exitEdhProc
          exit
          ( that'
          , scope'
          , case rhVal of
            EdhFallthrough -> EdhFallthrough
            _              -> EdhCaseClose rhVal
          )
      -- | guarded condition does not match against target in context, but
      -- whether itself is true
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
      -- | match against the target in context
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

