
module Language.Edh.Batteries.Ctrl where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader

import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | utility error(*args,**kwargs) - eval error reporter
errorProc :: EdhProcedure
errorProc !argsSender !that _ _ =
  packEdhArgs that argsSender
    $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) -> if null kwargs
        then case args of
          [v] -> throwEdh EvalError $ edhValueStr v
          _   -> throwEdh EvalError $ edhValueStr $ EdhTuple args
        else
          let !kwDict =
                Map.fromAscList
                  $ (<$> Map.toAscList kwargs)
                  $ \(attrName, val) -> (ItemByStr attrName, val)
          in
            throwEdh EvalError
            $ T.pack
            $ showEdhDict
            $ Map.union kwDict
            $ Map.fromAscList
                [ (ItemByNum (fromIntegral i), t)
                | (i, t) <- zip [(0 :: Int) ..] args
                ]


-- | operator (->) - the brancher, if its left-hand matches, early stop its
-- enclosing code block (commonly a case-of block, but other blocks as well),
-- with eval-ed result of its right-hand, unless the right-hand result is
-- `fallthrough`
branchProc :: EdhProcedure
branchProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx@(  Context _ _ _ !ctxMatch _) = edh'context pgs
      !callerScope@(Scope !ent _ _ _         ) = contextScope callerCtx
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

    BlockExpr patternExpr -> case patternExpr of
      -- ^ a block expr left to (->) triggers pattern matching

      -- | {( x,y,z,... )} -- tuple pattern
      [StmtSrc (_, ExprStmt (TupleExpr vExprs))] -> contEdhSTM $ do
        attrNames <- sequence $ (<$> vExprs) $ \case
          (AttrExpr (DirectRef (NamedAttr vAttr))) -> return $ AttrByName vAttr
          vPattern ->
            throwEdhFromSTM pgs EvalError
              $  "Invalid element in tuple pattern: "
              <> T.pack (show vPattern)
        case ctxMatch of
          EdhTuple vs | length vs == length vExprs -> do
            modifyTVar' ent $ Map.union $ Map.fromList $ zip attrNames vs
            runEdhProg pgs $ evalExpr that rhExpr $ \(that', scope', rhVal) ->
              exitEdhProc
                exit
                ( that'
                , scope'
                , case rhVal of
                  EdhFallthrough -> EdhFallthrough
                  _              -> EdhCaseClose rhVal
                )
          _ -> exitEdhSTM pgs exit (that, callerScope, EdhFallthrough)

       -- | {( x:y:z:... )} -- pair pattern
      [StmtSrc (_, ExprStmt (ParenExpr pairPattern))] ->
        case matchPairPattern pairPattern ctxMatch [] of
          Nothing -> throwEdh EvalError $ "Invalid pair pattern: " <> T.pack
            (show pairPattern)
          Just [] -> -- valid pattern, no match
            exitEdhProc exit (that, callerScope, EdhFallthrough)
          Just mps -> -- pattern matched
                      contEdhSTM $ do
            modifyTVar' ent $ Map.union (Map.fromList mps)
            runEdhProg pgs $ evalExpr that rhExpr $ \(that', scope', rhVal) ->
              exitEdhProc
                exit
                ( that'
                , scope'
                , case rhVal of
                  EdhFallthrough -> EdhFallthrough
                  _              -> EdhCaseClose rhVal
                )

      -- {{ class:obj }} -- instance pattern
      [StmtSrc (_, ExprStmt (DictExpr [InfixExpr ":" (AttrExpr (DirectRef (NamedAttr className))) (AttrExpr (DirectRef (NamedAttr objAttr)))]))]
        -> -- brittany insists on putting together the long line above, any workaround?
          -- TODO impl. this 
           throwEdh EvalError "Class pattern not impl. yet"

      -- TODO more kinds of match patterns to support ?
      --      e.g. list pattern, with rest-items repacking etc.
      _ -> throwEdh EvalError $ "Invalid match pattern: " <> T.pack
        (show patternExpr)


    -- | guarded condition, ignore match target in context, just check if the
    -- condition itself is true
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

    -- | value-wise matching against the target in context
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


-- | `Nothing` means invalid pattern, `[]` means no match, non-empty list is
-- the aligned values along with attr names as matched
matchPairPattern
  :: Expr -> EdhValue -> [(AttrKey, EdhValue)] -> Maybe [(AttrKey, EdhValue)]
matchPairPattern p v matches = case p of
  InfixExpr ":" leftExpr (AttrExpr (DirectRef (NamedAttr vAttr))) -> case v of
    EdhPair leftVal val ->
      let matches' = ((AttrByName vAttr, val) : matches)
      in  case leftExpr of
            (AttrExpr (DirectRef (NamedAttr leftAttr))) -> case leftVal of
              EdhPair _ _ -> Just []
              _           -> Just ((AttrByName leftAttr, leftVal) : matches')
            InfixExpr ":" _ _ -> matchPairPattern leftExpr leftVal matches'
            _                 -> Nothing
    _ -> Just []
  _ -> Nothing
