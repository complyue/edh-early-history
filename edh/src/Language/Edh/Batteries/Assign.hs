
module Language.Edh.Batteries.Assign where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader

import           Control.Concurrent.STM

import qualified Data.Text                     as T

import qualified Data.Map.Strict               as Map
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( (<|) )

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | operator (=)
assignProc :: EdhProcedure
assignProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  pgs <- ask
  let !ctx@(Context _ !call'stack _ _ _) = edh'context pgs
      !scope                             = contextScope ctx
  case lhExpr of
    IndexExpr ixExpr tgtExpr -> evalExpr that ixExpr $ \(_, _, ixVal) ->
      -- indexing assignment
      evalExpr that rhExpr $ \(_, _, rhVal) -> evalExpr that tgtExpr $ \case

       -- indexing assign to a dict
        (that', scope', EdhDict (Dict d)) -> contEdhSTM $ do
          ixKey <- case ixVal of
            EdhType    t -> return $ ItemByType t
            EdhString  s -> return $ ItemByStr s
            EdhSymbol  s -> return $ ItemBySym s
            EdhDecimal n -> return $ ItemByNum n
            EdhBool    b -> return $ ItemByBool b
            _ ->
              throwEdhFromSTM pgs EvalError
                $  "Invalid dict key: "
                <> T.pack (show $ edhTypeOf ixVal)
                <> ": "
                <> T.pack (show ixVal)
          modifyTVar' d $ Map.insert ixKey rhVal
          exitEdhSTM pgs exit (that', scope', rhVal)

        -- indexing assign to an object, by calling its ([=]) method with ixVal and rhVal
        -- as the args
        (_, _, EdhObject obj) ->
          contEdhSTM $ lookupEdhObjAttr obj (AttrByName "[=]") >>= \case
            Nothing ->
              throwEdhFromSTM pgs EvalError $ "No ([=]) method from: " <> T.pack
                (show obj)
            Just (EdhMethod (Method mth'lexi'stack mthProc@(ProcDecl _ mth'args mth'body)))
              -> do
                (ixAttrName, valAttrName) <- indexingAssignMethodArg pgs
                                                                     mth'args
                ixMthEnt <-
                  newTVar
                    $ Map.fromList
                        [ (AttrByName ixAttrName , ixVal)
                        , (AttrByName valAttrName, rhVal)
                        ]
                runEdhProg
                    (pgs -- set method's scope
                      { edh'context =
                        (edh'context pgs)
                          { callStack =
                            (  Scope ixMthEnt
                                     obj
                                     (NE.toList mth'lexi'stack)
                                     mthProc
                            <| call'stack
                            )
                          }
                      }
                    )
                  $ evalStmt obj mth'body
                  $ \(that'', scope'', mthRtn) ->
                      -- restore previous context after method returned
                      local (const pgs) $ case mthRtn of
                        EdhContinue -> throwEdh EvalError "Unexpected continue"
                        -- allow the use of `break` to early stop a method 
                        -- procedure with nil result
                        EdhBreak    -> exitEdhProc exit (that, scope, nil)
                        -- explicit return
                        EdhReturn rtnVal ->
                          exitEdhProc exit (that'', scope'', rtnVal)
                        -- no explicit return, assuming it returns the last
                        -- value from procedure execution
                        _ -> exitEdhProc exit (that'', scope'', mthRtn)
            Just badIndexer ->
              throwEdhFromSTM pgs EvalError
                $  "Malformed index method ([=]) on "
                <> T.pack (show obj)
                <> " - "
                <> T.pack (show $ edhTypeOf badIndexer)
                <> ": "
                <> T.pack (show badIndexer)

        (_, _, tgtVal) ->
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
      local (\pgs' -> pgs' { edh'in'tx = True })
        $ evalExpr that rhExpr
        $ assignEdhTarget pgs that lhExpr exit
assignProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)

