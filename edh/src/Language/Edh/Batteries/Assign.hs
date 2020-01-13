
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


-- | operator (=)
assignProc :: EdhProcedure
assignProc [SendPosArg !lhExpr, SendPosArg !rhExpr] !exit = do
  pgs <- ask
  case lhExpr of
    IndexExpr ixExpr tgtExpr ->
      evalExpr ixExpr $ \(OriginalValue !ixVal _ _) ->
      -- indexing assignment
        evalExpr rhExpr $ \(OriginalValue !rhVal _ _) ->
          evalExpr tgtExpr $ \(OriginalValue !tgtVal _ _) -> case tgtVal of

       -- indexing assign to a dict
            EdhDict (Dict d) -> contEdhSTM $ do
              ixKey <- case ixVal of
                EdhType    t -> return $ ItemByType t
                EdhString  s -> return $ ItemByStr s
                EdhSymbol  s -> return $ ItemBySym s
                EdhDecimal n -> return $ ItemByNum n
                EdhBool    b -> return $ ItemByBool b
                _ ->
                  throwEdhSTM pgs EvalError
                    $  "Invalid dict key: "
                    <> T.pack (show $ edhTypeOf ixVal)
                    <> ": "
                    <> T.pack (show ixVal)
              modifyTVar' d $ Map.insert ixKey rhVal
              exitEdhSTM pgs exit rhVal

            -- indexing assign to an object, by calling its ([=]) method with ixVal and rhVal
            -- as the args
            EdhObject obj ->
              contEdhSTM $ resolveEdhObjAttr obj (AttrByName "[=]") >>= \case
                Nothing ->
                  throwEdhSTM pgs EvalError $ "No ([=]) method from: " <> T.pack
                    (show obj)
                Just (OriginalValue (EdhMethod (Method mth'lexi'stack mth'proc)) _ mth'that)
                  -> runEdhProg pgs $ callEdhMethod
                    (ArgsPack [ixVal, rhVal] Map.empty)
                    mth'that
                    mth'lexi'stack
                    mth'proc
                    Nothing
                    exit
                Just (OriginalValue !badIndexer _ _) ->
                  throwEdhSTM pgs EvalError
                    $  "Malformed index method ([=]) on "
                    <> T.pack (show obj)
                    <> " - "
                    <> T.pack (show $ edhTypeOf badIndexer)
                    <> ": "
                    <> T.pack (show badIndexer)

            _ ->
              throwEdh EvalError
                $  "Don't know how to index assign "
                <> T.pack (show $ edhTypeOf tgtVal)
                <> ": "
                <> T.pack (show tgtVal)
                <> " with "
                <> T.pack (show $ edhTypeOf ixVal)
                <> ": "
                <> T.pack (show ixVal)

    _ -> -- non-indexing assignment
      -- execution of the assignment always in a tx for atomicity
      local (const pgs { edh'in'tx = True })
        $ evalExpr rhExpr
        $ \(OriginalValue !rhVal _ _) -> assignEdhTarget pgs lhExpr exit rhVal

assignProc !argsSender _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)

