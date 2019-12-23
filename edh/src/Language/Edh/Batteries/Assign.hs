
module Language.Edh.Batteries.Assign where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


assignProc :: EdhProcedure
assignProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  !pgs <- ask
  let txq                             = edh'main'queue pgs
      !inTx                           = edh'in'tx pgs
      !callerCtx                      = edh'context pgs
      !callerScope@(Scope !ent !this) = contextScope callerCtx
      !thisEnt                        = objEntity this
      !pgs'tx                         = pgs { edh'in'tx = True }
      finishAssign :: Entity -> AttrKey -> (Scope, EdhValue) -> EdhProg (STM ())
      finishAssign tgtEnt key (_, !val) = return $ do
        let result = (callerScope, val) -- assigment result as if come from the calling scope
        modifyTVar' tgtEnt $ \em -> Map.insert key val em
        if inTx
          then join $ runReaderT (exit result) pgs
          else writeTQueue txq (result, exit)
  case lhExpr of
    AttrExpr !addr -> case addr of
      DirectRef !addr' -> return $ resolveAddr callerScope addr' >>= \key ->
        join $ runReaderT (evalExpr rhExpr (finishAssign ent key)) pgs'tx
      IndirectRef !tgtExpr !addr' -> case tgtExpr of
        AttrExpr ThisRef ->
          return
            $   resolveAddr callerScope addr'
            >>= \key -> join $ runReaderT
                  (evalExpr rhExpr (finishAssign thisEnt key))
                  pgs'tx
        AttrExpr SupersRef ->
          throwEdh $ EvalError "Can not assign an attribute to supers"
        _ -> local (const pgs'tx) $ evalExpr tgtExpr $ \(_, !tgtVal) ->
          case tgtVal of
            EdhObject (Object !tgtEnt _ _) ->
              return
                $   resolveAddr callerScope addr'
                >>= \key -> join $ runReaderT
                      (evalExpr rhExpr (finishAssign tgtEnt key))
                      pgs'tx
            _ -> throwEdh $ EvalError $ "Invalid assignment target: " <> T.pack
              (show tgtVal)
      ThisRef   -> throwEdh $ EvalError "Can not assign to this"
      SupersRef -> throwEdh $ EvalError "Can not assign to supers"
    x ->
      throwEdh
        $  EvalError
        $  "Invalid left hand value for assignment: "
        <> T.pack (show x)
assignProc !argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)

