
module Language.Edh.Batteries.Math where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime

import           Data.Lossless.Decimal


-- | operator (+)
addProc :: EdhProcedure
addProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr that rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal rhNum ->
        exitEdhProc exit (that, scope, EdhDecimal $ lhNum + rhNum)
      _ ->
        throwEdh EvalError
          $  "Invalid right-hand value for (+) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh EvalError
        $  "Invalid left-hand value for (+) operation: "
        <> T.pack (show lhVal)
addProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (-)
subsProc :: EdhProcedure
subsProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr that rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal rhNum ->
        exitEdhProc exit (that, scope, EdhDecimal $ lhNum - rhNum)
      _ ->
        throwEdh EvalError
          $  "Invalid right-hand value for (-) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh EvalError
        $  "Invalid left-hand value for (-) operation: "
        <> T.pack (show lhVal)
subsProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (*)
mulProc :: EdhProcedure
mulProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr that rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal rhNum ->
        exitEdhProc exit (that, scope, EdhDecimal $ lhNum * rhNum)
      _ ->
        throwEdh EvalError
          $  "Invalid right-hand value for (*) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh EvalError
        $  "Invalid left-hand value for (*) operation: "
        <> T.pack (show lhVal)
mulProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (/)
divProc :: EdhProcedure
divProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr that rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal rhNum ->
        exitEdhProc exit (that, scope, EdhDecimal $ lhNum / rhNum)
      _ ->
        throwEdh EvalError
          $  "Invalid right-hand value for (/) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh EvalError
        $  "Invalid left-hand value for (/) operation: "
        <> T.pack (show lhVal)
divProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (**)
powProc :: EdhProcedure
powProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhDecimal lhNum -> evalExpr that rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhDecimal (Decimal rh'd rh'e rh'n) -> if rh'd /= 1
        then
          throwEdh EvalError
          $  "Invalid right-hand value for (**) operation: "
          <> T.pack (show rhVal)
        else exitEdhProc
          exit
          (that, scope, EdhDecimal $ lhNum ^^ (rh'n * 10 ^ rh'e))
      _ ->
        throwEdh EvalError
          $  "Invalid right-hand value for (**) operation: "
          <> T.pack (show rhVal)
    _ ->
      throwEdh EvalError
        $  "Invalid left-hand value for (**) operation: "
        <> T.pack (show lhVal)
powProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (&&)
logicalAndProc :: EdhProcedure
logicalAndProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhBool lhBool -> evalExpr that rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhBool rhBool ->
        exitEdhProc exit (that, scope, EdhBool $ lhBool && rhBool)
      _ -> throwEdh EvalError $ "Invalid right-hand value type: " <> T.pack
        (show $ edhTypeOf rhVal)
    _ -> throwEdh EvalError $ "Invalid left-hand value type: " <> T.pack
      (show $ edhTypeOf lhVal)
logicalAndProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)

-- | operator (||)
logicalOrProc :: EdhProcedure
logicalOrProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) -> case lhVal of
    EdhBool lhBool -> evalExpr that rhExpr $ \(_, _, rhVal) -> case rhVal of
      EdhBool rhBool ->
        exitEdhProc exit (that, scope, EdhBool $ lhBool || rhBool)
      _ -> throwEdh EvalError $ "Invalid right-hand value type: " <> T.pack
        (show $ edhTypeOf rhVal)
    _ -> throwEdh EvalError $ "Invalid left-hand value type: " <> T.pack
      (show $ edhTypeOf lhVal)
logicalOrProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (~=)
valEqProc :: EdhProcedure
valEqProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let
    !callerCtx = edh'context pgs
    !scope     = contextScope callerCtx
    cmp2List :: [EdhValue] -> [EdhValue] -> STM Bool
    cmp2List []               []               = return True
    cmp2List (_ : _)          []               = return False
    cmp2List []               (_     : _     ) = return False
    cmp2List (lhVal : lhRest) (rhVal : rhRest) = cmp2Val lhVal rhVal >>= \case
      False -> return False
      True  -> cmp2List lhRest rhRest
    cmp2Map :: [(ItemKey, EdhValue)] -> [(ItemKey, EdhValue)] -> STM Bool
    cmp2Map []      []      = return True
    cmp2Map (_ : _) []      = return False
    cmp2Map []      (_ : _) = return False
    cmp2Map ((lhKey, lhVal) : lhRest) ((rhKey, rhVal) : rhRest) =
      if lhKey /= rhKey
        then return False
        else cmp2Val lhVal rhVal >>= \case
          False -> return False
          True  -> cmp2Map lhRest rhRest
    cmp2Val :: EdhValue -> EdhValue -> STM Bool
    cmp2Val lhVal rhVal = if lhVal == rhVal
      then return True
      else case lhVal of
        EdhList (List lhll) -> case rhVal of
          EdhList (List rhll) -> do
            lhl <- readTVar lhll
            rhl <- readTVar rhll
            cmp2List lhl rhl
          _ -> return False
        EdhDict (Dict lhd) -> case rhVal of
          EdhDict (Dict rhd) -> do
            lhm <- readTVar lhd
            rhm <- readTVar rhd
            cmp2Map (Map.toAscList lhm) (Map.toAscList rhm)
          _ -> return False
        _ -> return False
  evalExpr that lhExpr $ \(_, _, lhVal) ->
    evalExpr that rhExpr $ \(_, _, rhVal) -> if lhVal == rhVal
      then exitEdhProc exit (that, scope, true)
      else contEdhSTM $ cmp2Val lhVal rhVal >>= \conclusion ->
        exitEdhSTM pgs exit (that, scope, EdhBool conclusion)
valEqProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (==)
idEqProc :: EdhProcedure
idEqProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) ->
    evalExpr that rhExpr $ \(_, _, rhVal) ->
      exitEdhProc exit (that, scope, EdhBool $ lhVal == rhVal)
idEqProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


-- | operator (>)
isGtProc :: EdhProcedure
isGtProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) ->
    evalExpr that rhExpr $ \(_, _, rhVal) ->
      contEdhSTM $ doEdhComparison pgs exit that scope lhVal rhVal $ \case
        GT -> True
        _  -> False
isGtProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)

-- | operator (>=)
isGeProc :: EdhProcedure
isGeProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) ->
    evalExpr that rhExpr $ \(_, _, rhVal) ->
      contEdhSTM $ doEdhComparison pgs exit that scope lhVal rhVal $ \case
        GT -> True
        EQ -> True
        _  -> False
isGeProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)

-- | operator (<)
isLtProc :: EdhProcedure
isLtProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) ->
    evalExpr that rhExpr $ \(_, _, rhVal) ->
      contEdhSTM $ doEdhComparison pgs exit that scope lhVal rhVal $ \case
        LT -> True
        _  -> False
isLtProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)

-- | operator (<=)
isLeProc :: EdhProcedure
isLeProc [SendPosArg !lhExpr, SendPosArg !rhExpr] that _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  evalExpr that lhExpr $ \(_, _, lhVal) ->
    evalExpr that rhExpr $ \(_, _, rhVal) ->
      contEdhSTM $ doEdhComparison pgs exit that scope lhVal rhVal $ \case
        LT -> True
        EQ -> True
        _  -> False
isLeProc !argsSender _ _ _ =
  throwEdh EvalError $ "Unexpected operator args: " <> T.pack (show argsSender)


doEdhComparison
  :: EdhProgState
  -> EdhProcExit
  -> Object
  -> Scope
  -> EdhValue
  -> EdhValue
  -> (Ordering -> Bool)
  -> STM ()
doEdhComparison pgs exit that scope lhVal rhVal cm =
  compareEdhValue lhVal rhVal >>= \case
    Nothing ->
      throwEdhSTM pgs EvalError
        $  "Not comparable: "
        <> T.pack (show $ edhTypeOf lhVal)
        <> " vs "
        <> T.pack (show $ edhTypeOf rhVal)
    Just ord -> exitEdhSTM pgs exit (that, scope, EdhBool $ cm ord)

compareEdhValue :: EdhValue -> EdhValue -> STM (Maybe Ordering)
compareEdhValue lhVal rhVal = case lhVal of
  EdhDecimal lhNum -> case rhVal of
    EdhDecimal rhNum -> return $ Just $ compare lhNum rhNum
    _                -> return Nothing
  EdhString lhStr -> case rhVal of
    EdhString rhStr -> return $ Just $ compare lhStr rhStr
    _               -> return Nothing
  EdhBool lhCnd -> case rhVal of
    EdhBool rhCnd -> return $ Just $ compare lhCnd rhCnd
    _             -> return Nothing
  _ -> return Nothing

