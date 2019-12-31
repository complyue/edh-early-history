
module Language.Edh.Batteries.Reflect where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | utility constructor(*args,**kwargs)
ctorProc :: EdhProcedure
ctorProc !argsSender !that _ !exit = do
  !pgs <- ask
  let callerCtx                      = edh'context pgs
      callerScope@(Scope _ this _ _) = contextScope callerCtx
  packEdhArgs that argsSender
    $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) ->
        let !argsCls = edhClassOf <$> args
        in  if null kwargs
              then case argsCls of
                [] ->
                  exitEdhProc exit (that, callerScope, EdhClass $ objClass this)
                [t] -> exitEdhProc exit (that, callerScope, t)
                _   -> exitEdhProc exit (that, callerScope, EdhTuple argsCls)
              else exitEdhProc
                exit
                ( that
                , callerScope
                , EdhArgsPack $ ArgsPack argsCls $ Map.map edhClassOf kwargs
                )
 where
  edhClassOf :: EdhValue -> EdhValue
  edhClassOf (EdhObject o) = EdhClass $ objClass o
  edhClassOf _             = nil

-- | utility supers(*args,**kwargs)
supersProc :: EdhProcedure
supersProc !argsSender that _ !exit = do
  !pgs <- ask
  let !callerCtx   = edh'context pgs
      !callerScope = contextScope callerCtx
      !argCnt      = case argsSender of
        SingleSender _       -> 1
        PackSender   senders -> length senders
  if argCnt < 1
    then contEdhSTM $ do
      supers <- map EdhObject <$> (readTVar $ objSupers that)
      exitEdhSTM pgs exit (that, callerScope, EdhTuple supers)
    else
      packEdhArgs that argsSender
        $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) -> if null kwargs
            then case args of
              [v] -> contEdhSTM $ do
                supers <- supersOf v
                exitEdhSTM pgs exit (that, callerScope, supers)
              _ -> contEdhSTM $ do
                argsSupers <- sequence $ supersOf <$> args
                exitEdhSTM pgs exit (that, callerScope, EdhTuple argsSupers)
            else contEdhSTM $ do
              argsSupers   <- sequence $ supersOf <$> args
              kwargsSupers <- sequence $ Map.map supersOf kwargs
              exitEdhSTM
                pgs
                exit
                ( that
                , callerScope
                , EdhArgsPack $ ArgsPack argsSupers kwargsSupers
                )
 where
  supersOf :: EdhValue -> STM EdhValue
  supersOf v = case v of
    EdhObject o ->
      map EdhObject <$> readTVar (objSupers o) >>= return . EdhTuple
    _ -> return nil


-- | utility scope()
-- obtain current scope as reflected object
scopeObtainProc :: EdhProcedure
scopeObtainProc _ !that _ !exit = do
  !pgs <- ask
  let (Context !world !call'stack _ _) = edh'context pgs
      !scope                           = NE.head call'stack
  contEdhSTM $ do
    wrapperObj <- mkScopeWrapper world scope
    exitEdhSTM pgs
               exit
               (that, contextScope $ edh'context pgs, EdhObject wrapperObj)


-- | utility scope.attrs()
-- get attribute types in the scope
scopeAttrsProc :: EdhProcedure
scopeAttrsProc _ !that _ !exit = do
  !pgs <- ask
  contEdhSTM $ do
    supers <- readTVar $ objSupers that
    case supers of
      [_, ent'wrapper, _wrapped'this] -> do
        em <- readTVar (objEntity ent'wrapper)
        ad <-
          newTVar
          $ Map.fromAscList
          $ [ (itemKeyOf ak, v) | (ak, v) <- Map.toAscList em ]
        exitEdhSTM pgs
                   exit
                   (that, contextScope (edh'context pgs), EdhDict $ Dict ad)
      _ -> error "bug <scope> supers wrong"
 where
  itemKeyOf :: AttrKey -> ItemKey
  itemKeyOf (AttrByName name) = ItemByStr name
  itemKeyOf (AttrBySym  sym ) = ItemBySym sym


-- | utility scope.traceback()
-- get stack trace from the wrapped scope
scopeTraceBackProc :: EdhProcedure
scopeTraceBackProc _ !that _ !exit = do
  !pgs <- ask
  let callerCtx = edh'context pgs
      scopesShown =
        show
              -- the world scope at bottom of any lexical stack has empty
              -- lexical stack itself, and is of no interest
          <$> (NE.takeWhile (not . null . lexiStack) $ classLexiStack $ objClass
                that
              )
  exitEdhProc
    exit
    ( that
    , contextScope callerCtx
    , EdhString $ T.pack $ unlines $ reverse scopesShown
    )


-- | utility scope.stack()
-- get lexical context from the wrapped scope
scopeStackProc :: EdhProcedure
scopeStackProc _ !that _ !exit = do
  !pgs <- ask
  let callerCtx@(Context !world _ _ _) = edh'context pgs
  contEdhSTM $ do
    wrappedObjs <-
      sequence
      $   mkScopeWrapper world
      -- the world scope at bottom of any lexical stack has empty
      -- lexical stack itself, and is of no interest
      <$> (NE.takeWhile (not . null . lexiStack) $ classLexiStack $ objClass
            that
          )
    exitEdhSTM
      pgs
      exit
      (that, contextScope callerCtx, EdhTuple $ EdhObject <$> wrappedObjs)


-- | utility scope.eval(expr1, expr2, kw3=expr3, kw4=expr4, ...)
-- evaluate expressions in this scope
scopeEvalProc :: EdhProcedure
scopeEvalProc !argsSender !that _ !exit = do
  !pgs <- ask
  let
    callerCtx@(Context !world _ _ _) = edh'context pgs
    callerScope                      = contextScope $ callerCtx
    evalThePack
      :: [EdhValue]
      -> Map.Map AttrName EdhValue
      -> [EdhValue]
      -> Map.Map AttrName EdhValue
      -> EdhProg (STM ())
    evalThePack !argsValues !kwargsValues [] !kwargsExprs
      | Map.null kwargsExprs =
      -- restore original program state and return the eval-ed values
                               local (const pgs) $ exitEdhProc
        exit
        ( that
        , callerScope
        , case argsValues of
          [val] | null kwargsValues -> val
          _ -> EdhArgsPack $ ArgsPack (reverse argsValues) kwargsValues
        )
    evalThePack !argsValues !kwargsValues [] !kwargsExprs = do
      let (!oneExpr, !kwargsExprs') = Map.splitAt 1 kwargsExprs
          (!kw     , !kwExpr      ) = Map.elemAt 0 oneExpr
      case kwExpr of
        EdhExpr !expr -> evalExpr that expr $ \(_, _, !val) -> evalThePack
          argsValues
          (Map.insert kw val kwargsValues)
          []
          kwargsExprs'
        v -> throwEdh EvalError $ "Not an expr: " <> T.pack (show v)
    evalThePack !argsValues !kwargsValues (!argExpr : argsExprs') !kwargsExprs
      = case argExpr of
        EdhExpr expr -> evalExpr that expr $ \(_, _, !val) ->
          evalThePack (val : argsValues) kwargsValues argsExprs' kwargsExprs
        v -> throwEdh EvalError $ "Not an expr: " <> T.pack (show v)
  packEdhArgs that argsSender
    $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) ->
        if null kwargs && null args
          then exitEdhProc exit (that, callerScope, nil)
          else
            contEdhSTM
            $
              -- eval all exprs with the original lexical scope as call stack
              runEdhProg pgs
                { edh'context = Context
                                  { contextWorld = world
                                  , callStack = (classLexiStack $ objClass that)
                                  , contextMatch = true
                                  , contextStmt  = voidStatement
                                  }
                }
            $ evalThePack [] Map.empty args kwargs


-- | utility makeOp(lhExpr, opSym, rhExpr)
makeOpProc :: EdhProcedure
makeOpProc argsSender that _ !exit = do
  !pgs <- ask
  let !callerCtx   = edh'context pgs
      !callerScope = contextScope callerCtx
  packEdhArgs that argsSender
    $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) -> if (not $ null kwargs)
        then throwEdh EvalError "No kwargs accepted by makeOp"
        else case args of
          [(EdhExpr lhe), (EdhString op), (EdhExpr rhe)] ->
            exitEdhProc exit (that, callerScope, EdhExpr $ InfixExpr op lhe rhe)
          _ -> throwEdh EvalError $ "Invalid arguments to makeOp: " <> T.pack
            (show args)


-- | utility expr(*args,**kwargs)
makeExprProc :: EdhProcedure
makeExprProc !argsSender !that scope !exit = case argsSender of
  SingleSender (SendPosArg !argExpr) -> exit (that, scope, EdhExpr argExpr)
  PackSender   []                    -> exit (that, scope, nil)
  PackSender   [SendPosArg !argExpr] -> exit (that, scope, EdhExpr argExpr)
  PackSender   argSenders            -> makeExprProc' argSenders exit
  SingleSender argSender             -> makeExprProc' [argSender] exit
 where
  makeExprProc' :: [ArgSender] -> EdhProcExit -> EdhProg (STM ())
  makeExprProc' [] !exit' =
    exit' (that, scope, EdhArgsPack $ ArgsPack [] Map.empty)
  makeExprProc' (!x : xs) !exit' = case x of
    UnpackPosArgs _ -> throwEdh EvalError "not possible to unpack to expr"
    UnpackKwArgs _ -> throwEdh EvalError "not possible to unpack to expr"
    UnpackPkArgs _ -> throwEdh EvalError "not possible to unpack to expr"
    SendPosArg !argExpr -> makeExprProc' xs $ \(_, _, !pk) -> case pk of
      (EdhArgsPack (ArgsPack !posArgs !kwArgs)) ->
        exit'
          ( that
          , scope
          , EdhArgsPack $ ArgsPack (EdhExpr argExpr : posArgs) kwArgs
          )
      _ -> error "bug"
    SendKwArg !kw !argExpr -> makeExprProc' xs $ \(_, _, !pk) -> case pk of
      (EdhArgsPack (ArgsPack !posArgs !kwArgs)) -> exit'
        ( that
        , scope
        , EdhArgsPack $ ArgsPack posArgs $ Map.insert kw
                                                      (EdhExpr argExpr)
                                                      kwArgs
        )
      _ -> error "bug"

