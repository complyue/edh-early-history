
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


_finishAssign
  :: Bool
  -> Entity
  -> AttrKey
  -> EdhProcExit
  -> (Scope, EdhValue)
  -> EdhProg (STM ())
_finishAssign inTx tgtEnt key exit (_, !val) = do
  !pgs <- ask
  let txq          = edh'main'queue pgs
      !callerCtx   = edh'context pgs
      !callerScope = contextScope callerCtx
  return $ do
    -- assigment result as if come from the calling scope
    let result = (callerScope, val)
    modifyTVar' tgtEnt $ \em -> Map.insert key val em
    if inTx
      then join $ runReaderT (exit result) pgs
      else writeTQueue txq (result, exit)


-- | operator (=)
assignProc :: EdhProcedure
assignProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  !pgs <- ask
  let !callerCtx                      = edh'context pgs
      !callerScope@(Scope !ent !this) = contextScope callerCtx
      !thisEnt                        = objEntity this
      -- execution of the assignment always in a tx for atomicity
      !inTx                           = edh'in'tx pgs
      !pgs'tx                         = pgs { edh'in'tx = True }
  case lhExpr of
    AttrExpr !addr -> case addr of
      DirectRef !addr' -> return $ resolveAddr callerScope addr' >>= \key ->
        join $ runReaderT (evalExpr rhExpr (_finishAssign inTx ent key exit))
                          pgs'tx
      IndirectRef !tgtExpr !addr' -> case tgtExpr of
        AttrExpr ThisRef -> return $ resolveAddr callerScope addr' >>= \key ->
          join $ runReaderT
            (evalExpr rhExpr (_finishAssign inTx thisEnt key exit))
            pgs'tx
        AttrExpr SupersRef ->
          throwEdh $ EvalError "Can not assign an attribute to supers"
        _ -> local (const pgs'tx) $ evalExpr tgtExpr $ \(_, !tgtVal) ->
          case tgtVal of
            EdhObject (Object !tgtEnt _ _) ->
              return $ resolveAddr callerScope addr' >>= \key ->
                join $ runReaderT
                  (evalExpr rhExpr (_finishAssign inTx tgtEnt key exit))
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


-- | operator (+=)
iaddProc :: EdhProcedure
iaddProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  !pgs <- ask
  let !callerCtx                      = edh'context pgs
      !callerScope@(Scope !ent !this) = contextScope callerCtx
      !thisEnt                        = objEntity this
      -- execution of the assignment always in a tx for atomicity
      !inTx                           = edh'in'tx pgs
      !pgs'tx                         = pgs { edh'in'tx = True }
  case lhExpr of
    AttrExpr !addr -> case addr of
      DirectRef !addr' -> return $ resolveAddr callerScope addr' >>= \key ->
        join $ runReaderT (evalExpr rhExpr (_finishAssign inTx ent key exit))
                          pgs'tx
      IndirectRef !tgtExpr !addr' -> case tgtExpr of
        AttrExpr ThisRef -> return $ resolveAddr callerScope addr' >>= \key ->
          join $ runReaderT
            (evalExpr rhExpr (_finishAssign inTx thisEnt key exit))
            pgs'tx
        AttrExpr SupersRef ->
          throwEdh $ EvalError "Can not assign an attribute to supers"
        _ -> local (const pgs'tx) $ evalExpr tgtExpr $ \(_, !tgtVal) ->
          case tgtVal of
            EdhObject (Object !tgtEnt _ _) ->
              return $ resolveAddr callerScope addr' >>= \key ->
                join $ runReaderT
                  (evalExpr rhExpr (_finishAssign inTx tgtEnt key exit))
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
iaddProc !argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)

