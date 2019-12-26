{-# LANGUAGE TupleSections #-}

module Language.Edh.Details.Evaluate where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map
import           Data.List.NonEmpty             ( (<|) )
import qualified Data.List.NonEmpty            as NE


import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes
import           Language.Edh.Details.Tx
import           Language.Edh.Details.Utils


evalStmt :: Object -> StmtSrc -> EdhProcExit -> EdhProg (STM ())
evalStmt that ss@(StmtSrc (_, !stmt)) !exit =
  local (\pgs -> pgs { edh'context = (edh'context pgs) { contextStmt = ss } })
    $ evalStmt' that stmt exit


evalBlock :: Object -> [StmtSrc] -> EdhProcExit -> EdhProg (STM ())
evalBlock that [] !exit = do
  !pgs <- ask
  let !ctx                     = edh'context pgs
      !scope@(Scope _ !this _) = contextScope ctx
  exitEdhProc exit (this, scope, nil)
evalBlock that [!ss] !exit = evalStmt that ss $ \result ->
  exitEdhProc exit $ case result of
    (!this', !scope', EdhCaseClose !val) -> (this', scope', val)
    _ -> result
evalBlock that (ss : rest) !exit = evalStmt that ss $ \result -> case result of
  (!this', !scope', EdhCaseClose !val) -> exitEdhProc exit (this', scope', val)
  brk@(_, _, EdhBreak) -> exitEdhProc exit brk
  ctn@(_, _, EdhContinue) -> exitEdhProc exit ctn
  rtn@(_, _, EdhReturn _) -> exitEdhProc exit rtn
  yld@(_, _, EdhYield _) -> exitEdhProc exit yld
  (_, _, EdhFallthrough) -> evalBlock that rest exit
  _ -> evalBlock that rest exit


evalExprs :: Object -> [Expr] -> EdhProcExit -> EdhProg (STM ())
-- here 'EdhTuple' is used for intermediate tag,
-- not returning real tuple values as in Edh.
evalExprs _ [] exit = do
  !pgs <- ask
  let !ctx                     = edh'context pgs
      !scope@(Scope _ !this _) = contextScope ctx
  exit (this, scope, EdhTuple [])
evalExprs that (x : xs) exit = evalExpr that x $ \(this', scope', val) ->
  evalExprs that xs $ \(_, _, tv) -> case tv of
    EdhTuple l -> exitEdhProc exit (this', scope', EdhTuple (val : l))
    _          -> error "bug"


evalStmt' :: Object -> Stmt -> EdhProcExit -> EdhProg (STM ())
evalStmt' that stmt exit = do
  !pgs <- ask
  let !ctx@(  Context _    !stack _) = edh'context pgs
      !scope@(Scope   !ent !this  _) = contextScope ctx
  case stmt of

    ExprStmt expr -> evalExpr that expr exit

    LetStmt argsRcvr argsSndr ->
      -- ensure args sending and receiving happens within a same tx
      -- for atomicity of the let statement
      local (\pgs' -> pgs' { edh'in'tx = True })
        $ packEdhArgs that argsSndr
        $ \(_, _, pkv) -> case pkv of
            EdhArgsPack pk -> recvEdhArgs argsRcvr pk $ \(_, _, scopeObj) ->
              case scopeObj of
                EdhObject (Object rcvd'ent rcvd'cls [])
                  | rcvd'cls == (scopeClass $ contextWorld ctx) -> return $ do
                    -- overwrite current scope entity with attributes from
                    -- the received entity
                    um <- readTVar rcvd'ent
                    modifyTVar' ent $ \em -> Map.union um em
                    -- let statement evaluates to nil always
                    join $ runReaderT (exitEdhProc exit (this, scope, nil)) pgs
                _ -> error "bug"
            _ -> error "bug"

    BreakStmt       -> exitEdhProc exit (this, scope, EdhBreak)
    ContinueStmt    -> exitEdhProc exit (this, scope, EdhContinue)
    FallthroughStmt -> exitEdhProc exit (this, scope, EdhFallthrough)

    YieldStmt expr  -> evalExpr that expr $ \(this', scope', !val) ->
      exitEdhProc exit (this', scope', EdhYield val)
    ReturnStmt expr -> evalExpr that expr $ \(this', scope', !val) ->
      exitEdhProc exit (this', scope', EdhReturn val)

    ClassStmt pd@(ProcDecl name _ _) -> return $ do
      let
        !cls = EdhClass
          $ Class { classContext = NE.toList stack, classProcedure = pd }
      modifyTVar' ent $ \em -> Map.insert (AttrByName name) cls em
      join $ runReaderT (exitEdhProc exit (this, scope, cls)) pgs

    MethodStmt pd@(ProcDecl name _ _) -> return $ do
      let mth = EdhMethod $ Method { methodProcedure = pd }
      modifyTVar' ent $ \em -> Map.insert (AttrByName name) mth em
      join $ runReaderT (exitEdhProc exit (this, scope, mth)) pgs

    GeneratorStmt pd@(ProcDecl name _ _) -> return $ do
      let gdf = EdhGenrDef $ GenrDef { generatorProcedure = pd }
      modifyTVar' ent $ \em -> Map.insert (AttrByName name) gdf em
      join $ runReaderT (exitEdhProc exit (this, scope, gdf)) pgs

    ImportStmt _argsRcvr srcExpr -> case srcExpr of
      LitExpr (StringLiteral moduPath) -> exitEdhProc
        exit
        (this, scope, EdhString $ "wana import " <> moduPath <> ".edh huh?")
      expr ->
        throwEdh EvalError $ "don't know how to import " <> T.pack (show expr)

    VoidStmt -> exitEdhProc exit (this, scope, nil)

    -- TODO comment out this once `case stmt` get total
    _ -> throwEdh EvalError $ "Eval not yet impl for: " <> T.pack (show stmt)


evalExpr :: Object -> Expr -> EdhProcExit -> EdhProg (STM ())
evalExpr that expr exit = do
  !pgs <- ask
  let !ctx@(  Context !world !stack _) = edh'context pgs
      !scope@(Scope   _      !this  _) = contextScope ctx
  case expr of
    LitExpr lit -> case lit of
      DecLiteral    v -> exitEdhProc exit (this, scope, EdhDecimal v)
      StringLiteral v -> exitEdhProc exit (this, scope, EdhString v)
      BoolLiteral   v -> exitEdhProc exit (this, scope, EdhBool v)
      NilLiteral      -> exitEdhProc exit (this, scope, nil)
      TypeLiteral v   -> exitEdhProc exit (this, scope, EdhType v)
      -- TODO impl this
      SinkCtor        -> throwEdh EvalError "sink ctor not impl. yet"

    PrefixExpr prefix expr' -> case prefix of
      PrefixPlus  -> evalExpr that expr' exit
      PrefixMinus -> evalExpr that expr' $ \case
        (this', scope', EdhDecimal v) ->
          exitEdhProc exit (this', scope', EdhDecimal (-v))
        (_, _, v) ->
          throwEdh EvalError $ "Can not negate: " <> T.pack (show v) <> " ❌"
      Not -> evalExpr that expr' $ \case
        (this', scope', EdhBool v) ->
          exitEdhProc exit (this', scope', EdhBool $ not v)
        (_, _, v) ->
          throwEdh EvalError
            $  "Expect bool but got: "
            <> T.pack (show v)
            <> " ❌"

      -- TODO this should probably create Thunk instead, but mind to
      --      cooperate with the branch operator (->), find a way to
      --      tell it that this is guarded value, don't compare with
      --      thunk target value. but how ?
      Guard  -> evalExpr that expr' exit

      AtoIso -> local (\s -> s { edh'in'tx = True }) $ evalExpr that expr' exit

      -- TODO impl these
      Go     -> throwEdh EvalError "goroutine starter not impl. yet"
      Defer  -> throwEdh EvalError "defer scheduler not impl. yet"

    IfExpr cond cseq alt -> evalExpr that cond $ \case
      (_, _, EdhBool True ) -> evalStmt that cseq exit
      (_, _, EdhBool False) -> case alt of
        Just elseClause -> evalStmt that elseClause exit
        _               -> exitEdhProc exit (this, scope, nil)
      (_, _, v) ->
        -- we are so strongly typed
        throwEdh EvalError $ "Not a boolean value: " <> T.pack (show v) <> " ❌"

    DictExpr xs -> -- make sure dict k:v pairs are evaluated in same tx
      local (\s -> s { edh'in'tx = True }) $ evalExprs that xs $ \(_, _, tv) ->
        case tv of
          EdhTuple l -> return $ do
            dpl <- forM l $ \case
              EdhPair kVal vVal -> (, vVal) <$> case kVal of
                EdhType    k -> return $ ItemByType k
                EdhString  k -> return $ ItemByStr k
                EdhSymbol  k -> return $ ItemBySym k
                EdhDecimal k -> return $ ItemByNum k
                EdhBool    k -> return $ ItemByBool k
                k ->
                  throwEdhFromSTM pgs EvalError
                    $  "Invalid dict key: "
                    <> T.pack (show k)
                    <> " ❌"
              pv ->
                throwEdhFromSTM pgs EvalError
                  $  "Invalid dict entry: "
                  <> T.pack (show pv)
                  <> " ❌"
            ds <- newTVar $ Map.fromList dpl
            join $ runReaderT
              (exitEdhProc exit (this, scope, EdhDict (Dict ds)))
              pgs
          _ -> error "bug"

    ListExpr xs -> -- make sure list values are evaluated in same tx
      local (\s -> s { edh'in'tx = True }) $ evalExprs that xs $ \(_, _, tv) ->
        case tv of
          EdhTuple l -> return $ do
            ll <- List <$> newTVar l
            join $ runReaderT (exitEdhProc exit (this, scope, EdhList ll)) pgs
          _ -> error "bug"

    TupleExpr xs -> -- make sure tuple values are evaluated in same tx
      local (\s -> s { edh'in'tx = True }) $ evalExprs that xs $ \(_, _, tv) ->
        case tv of
          EdhTuple l -> exitEdhProc exit (this, scope, EdhTuple l)
          _          -> error "bug"

    ParenExpr x     -> evalExpr that x exit

    -- TODO this should check for Thunk, and implement
    --      break/fallthrough semantics
    BlockExpr stmts -> evalBlock that stmts exit

    -- TODO impl this
    -- ForExpr ar iter todo -> undefined

    AttrExpr  addr  -> case addr of
      ThisRef          -> exitEdhProc exit (this, scope, EdhObject this)
      ThatRef          -> exitEdhProc exit (that, scope, EdhObject that)
      DirectRef !addr' -> return $ do
        !key <- resolveAddr pgs addr'
        resolveEdhCtxAttr scope key >>= \case
          Nothing -> throwEdhFromSTM pgs EvalError $ "Not in scope: " <> T.pack
            (show addr')
          Just scope'@(Scope !ent' _obj _sp) -> do
            em <- readTVar ent'
            case Map.lookup key em of
              Nothing -> throwEdhFromSTM pgs EvalError "attr resolving bug"
              Just val ->
                join $ runReaderT (exitEdhProc exit (this, scope', val)) pgs
      IndirectRef !tgtExpr !addr' -> evalExpr that tgtExpr $ \case
        (_, _, EdhObject !obj) -> return $ do
          !key <- resolveAddr pgs addr'
          resolveEdhObjAttr obj key >>= \case
            Nothing ->
              throwEdhFromSTM pgs EvalError
                $  "No such attribute "
                <> T.pack (show key)
                <> " from "
                <> T.pack (show obj)
            Just scope'@(Scope !ent' _obj' _) -> do
              em <- readTVar ent'
              case Map.lookup key em of
                Nothing -> throwEdhFromSTM pgs EvalError "attr resolving bug"
                Just val ->
                  join $ runReaderT (exitEdhProc exit (obj, scope', val)) pgs
        (_, _, v) -> throwEdh EvalError $ "Not an object: " <> T.pack (show v)


    -- IndexExpr ixExpr tgtExpr ->

    CallExpr procExpr argsSndr -> evalExpr that procExpr $ \case

      (this', scope', EdhHostProc (HostProcedure _name proc)) ->
        proc argsSndr this' scope' exit


      (_cls'this, _cls'scope, EdhClass cls@(Class _ clsProc@(ProcDecl _class'name proc'args proc'body)))
        ->
          -- ensure args sending and receiving happens within a same tx for atomicity of
          -- the call making
           local (\pgs' -> pgs' { edh'in'tx = True })
          $ packEdhArgs that argsSndr
          $ \(_, _, pkv) -> case pkv of
              EdhArgsPack pk ->
                recvEdhArgs proc'args pk $ \(_, _, scopeObj) -> case scopeObj of
                  EdhObject (Object rcvd'ent rcvd'cls [])
                    | rcvd'cls == (scopeClass world) -> do
                      let !newThis = Object rcvd'ent cls []
                      -- use this as the scope entity of instance ctor execution
                      local
                          (\pgs' -> pgs'
                            -- set new object's scope
                            { edh'context =
                              (edh'context pgs')
                                { contextStack = (Scope rcvd'ent newThis clsProc
                                                 <| stack
                                                 )
                                }
                              -- restore original tx state after args received
                            , edh'in'tx   = edh'in'tx pgs
                            }
                          )
                        $ evalStmt that proc'body
                        $ \(_, _, ctorRtn) ->
                          -- restore previous context after ctor returned 
                            local (const pgs) $ case ctorRtn of
                          -- allow a class procedure to explicitly return other
                          -- value than newly constructed `this` object
                          -- it can still `return this to early stop the ctor proc
                          -- this is magically an advanced feature
                              EdhReturn rtnVal ->
                                exitEdhProc exit (this, scope, rtnVal)
                            -- no explicit return from class procedure, return the
                            -- newly constructed this object, throw away the last
                            -- value from the procedure execution
                              _ -> exitEdhProc
                                exit
                                (this, scope, EdhObject newThis)
                  _ -> error "bug"
              _ -> error "bug"


      (tgt'this, (Scope _ mth'this _), EdhMethod (Method mthProc@(ProcDecl _mth'name mth'args mth'body)))
        ->
          -- ensure args sending and receiving happens within a same tx
          -- for atomicity of the call making
           local (\pgs' -> pgs' { edh'in'tx = True })
          $ packEdhArgs that argsSndr
          $ \(_, _, pkv) -> case pkv of
              EdhArgsPack pk -> recvEdhArgs mth'args pk $ \(_, _, scopeObj) ->
                case scopeObj of
                  EdhObject (Object rcvd'ent rcvd'cls [])
                    | rcvd'cls == (scopeClass world)
                    ->
                      -- use direct containing object of the method in its
                      -- procedure execution
                       local
                        (\pgs' -> pgs'
                          -- set method's scope
                          { edh'context =
                            (edh'context pgs')
                              { contextStack = (Scope rcvd'ent mth'this mthProc
                                               <| stack
                                               )
                              }
                            -- restore original tx state after args received
                          , edh'in'tx   = edh'in'tx pgs
                          }
                        )
                        -- use the resolution target object as that in execution of 
                        -- the method procedure
                      $ evalStmt tgt'this mth'body
                      $ \(_, _, mthRtn) ->
                          -- restore previous context after method returned
                                           local (const pgs) $ case mthRtn of
                          EdhReturn rtnVal ->
                            exitEdhProc exit (this, scope, rtnVal)
                          -- no explicit return, assuming it returns the last
                          -- value from procedure execution
                          _ -> exitEdhProc exit (this, scope, mthRtn)
                  _ -> error "bug"
              _ -> error "bug"


      -- TODO impl. this
      -- EdhGenrDef genrDef ->

      (_, _, val) ->
        throwEdh EvalError
          $  "Can not call: "
          <> T.pack (show val)
          <> " ❌ expressed with: "
          <> T.pack (show procExpr)


    InfixExpr !opSym !lhExpr !rhExpr ->
      return $ resolveEdhCtxAttr scope (AttrByName opSym) >>= \case
        Nothing ->
          throwEdhFromSTM pgs EvalError
            $  "Operator ("
            <> T.pack (show opSym)
            <> ") not in scope"
        Just scope'@(Scope !ent' _obj _sp) -> do
          em <- readTVar ent'
          case Map.lookup (AttrByName opSym) em of
            Nothing -> error "attr resolving bug"
            Just (EdhHostProc (HostProcedure !_name !proc)) ->
              join $ runReaderT
                (proc (PackSender [SendPosArg lhExpr, SendPosArg rhExpr])
                      this
                      scope'
                      exit
                )
                pgs
            -- TODO handle operator procedures etc.
            Just val ->
              throwEdhFromSTM pgs EvalError $ "Not callable: " <> T.pack
                (show val)


    _ -> throwEdh EvalError $ "Eval not yet impl for: " <> T.pack (show expr)


-- | assign an evaluated value to a target expression
--
-- Note the calling procedure should declare in-tx state in evaluating the
-- right-handle value as well as running this, so the evaluation of the
-- right-hand value as well as the writting to the target entity are done
-- within the same tx, thus for atomicity of the whole assignment.
assignEdhTarget
  :: EdhProgState
  -> Object
  -> Expr
  -> EdhProcExit
  -> (Object, Scope, EdhValue)
  -> EdhProg (STM ())
assignEdhTarget pgsAfter that lhExpr exit (_, _, rhVal) = do
  !pgs <- ask
  let !callerCtx@(  Context _    _     _  ) = edh'context pgs
      !callerScope@(Scope   !ent !this _sp) = contextScope callerCtx
      finishAssign :: Object -> Entity -> AttrKey -> STM ()
      finishAssign tgtObj tgtEnt key = do
        modifyTVar' tgtEnt $ \em -> Map.insert key rhVal em
        -- restore program state after assignment
        join $ runReaderT (exitEdhProc exit (tgtObj, callerScope, rhVal))
                          pgsAfter
  case lhExpr of
    AttrExpr !addr -> case addr of
      DirectRef !addr' ->
        return $ resolveAddr pgs addr' >>= \key -> finishAssign this ent key
      IndirectRef !tgtExpr !addr' -> case tgtExpr of
        AttrExpr ThisRef -> return $ resolveAddr pgs addr' >>= \key ->
          finishAssign this (objEntity this) key
        AttrExpr ThatRef -> return $ resolveAddr pgs addr' >>= \key ->
          finishAssign that (objEntity that) key
        _ -> evalExpr that tgtExpr $ \(!_, !_, !tgtVal) -> case tgtVal of
          EdhObject tgtObj@(Object !tgtEnt _ _) ->
            return $ resolveAddr pgs addr' >>= \key ->
              finishAssign tgtObj tgtEnt key
          _ -> throwEdh EvalError $ "Invalid assignment target: " <> T.pack
            (show tgtVal)
      ThisRef -> throwEdh EvalError "Can not assign to this"
      ThatRef -> throwEdh EvalError "Can not assign to that"
    x ->
      throwEdh EvalError $ "Invalid left hand value for assignment: " <> T.pack
        (show x)


-- The Edh call convention is so called call-by-repacking, i.e. a new pack of
-- arguments are evaluated & packed at the calling site, then passed to the
-- callee site, where arguments in the pack are received into the callee's
-- entity, the receiving may include more packing into attributes manifested
-- for rest-args.

-- This is semantically much the same as Python's call convention, regarding
-- positional and keyword argument matching, and additionally supports:
--  * wildcard receiver - receive all keyword arguments into the entity
--  * retargeting - don't receive the argument into the entity, but assign
--    to an attribute of another object, typically `this` object in scope
--  * argument renaming - match the name as sent, receive to a  differently
--     named attribute of the entity. while renaming a positional argument
--     is doable but meaningless, you'd just use the later name
--- * rest-args repacking, in forms of:
---     *args
---     **kwargs
---     ***pkargs


recvEdhArgs :: ArgsReceiver -> ArgsPack -> EdhProcExit -> EdhProg (STM ())
recvEdhArgs !argsRcvr pck@(ArgsPack !posArgs !kwArgs) !exit = do
  !pgs <- ask
  let
    !callerCtx = edh'context pgs
    !callerScope@(Scope !_callerEnt !callerThis _) = contextScope callerCtx
    !world     = contextWorld callerCtx
    recvFromPack
      :: (ArgsPack, EntityStore) -> ArgReceiver -> STM (ArgsPack, EntityStore)
    recvFromPack (pk@(ArgsPack posArgs' kwArgs'), em) argRcvr = case argRcvr of
      RecvRestPosArgs restPosArgAttr -> return
        ( ArgsPack [] kwArgs'
        , Map.insert (AttrByName restPosArgAttr)
                     (EdhArgsPack $ ArgsPack posArgs' Map.empty)
                     em
        )
      RecvRestKwArgs restKwArgAttr -> return
        ( ArgsPack posArgs' Map.empty
        , Map.insert (AttrByName restKwArgAttr)
                     (EdhArgsPack $ ArgsPack [] kwArgs')
                     em
        )
      RecvRestPkArgs restPkArgAttr -> return
        ( ArgsPack [] Map.empty
        , Map.insert (AttrByName restPkArgAttr) (EdhArgsPack pk) em
        )
      RecvArg argName argTgtAddr argDefault -> do
        (argVal, posArgs'', kwArgs'') <- resolveArgValue argName argDefault
        case argTgtAddr of
          Nothing ->
            return
              ( ArgsPack posArgs'' kwArgs''
              , Map.insert (AttrByName argName) argVal em
              )
          Just (DirectRef addr) -> case addr of
            NamedAttr attrName -> -- simple rename
              return
                ( ArgsPack posArgs'' kwArgs''
                , Map.insert (AttrByName attrName) argVal em
                )
            SymbolicAttr _symName -> -- todo support this ?
                                     throwEdhFromSTM
              pgs
              EvalError
              "arg renaming to symbolic attr not supported"
          Just addr@(IndirectRef _ _) -> do
            join $ runReaderT
              (assignEdhTarget pgs
                               callerThis
                               (AttrExpr addr)
                               (const $ return $ return ())
                               (callerThis, callerScope, argVal)
              )
              (pgs { edh'in'tx = True })
            return (ArgsPack posArgs'' kwArgs'', em)
          tgt ->
            throwEdhFromSTM pgs EvalError
              $  "Invalid argument retarget: "
              <> T.pack (show tgt)
     where
      resolveArgValue
        :: AttrName
        -> Maybe Expr
        -> STM (EdhValue, [EdhValue], Map.Map AttrName EdhValue)
      resolveArgValue argName argDefault = do
        let (inKwArgs, kwArgs'') = takeOutFromMap argName kwArgs'
        case inKwArgs of
          Just argVal -> return (argVal, posArgs', kwArgs'')
          _           -> case posArgs' of
            (posArg : posArgs'') -> return (posArg, posArgs'', kwArgs'')
            []                   -> case argDefault of
              Just defaultExpr -> do
                defaultVar <- newEmptyTMVar
                join $ runReaderT
                  (evalExpr
                    callerThis
                    defaultExpr
                    (\(_, _, val) -> return (putTMVar defaultVar val))
                  )
                  (pgs { edh'in'tx = True })
                defaultVal <- readTMVar defaultVar
                return (defaultVal, posArgs', kwArgs'')
              _ ->
                throwEdhFromSTM pgs EvalError $ "Missing argument: " <> argName
    woResidual :: ArgsPack -> EntityStore -> STM Entity
    woResidual (ArgsPack !posResidual !kwResidual) em
      | not (null posResidual)
      = throwEdhFromSTM pgs EvalError
        $  "Extraneous "
        <> T.pack (show $ length posResidual)
        <> " positional argument(s)"
      | not (Map.null kwResidual)
      = throwEdhFromSTM pgs EvalError
        $  "Extraneous keyword arguments: "
        <> T.unwords (Map.keys kwResidual)
      | otherwise
      = newTVar em
    doReturn :: Entity -> STM ()
    doReturn ent = join $ runReaderT
      (exitEdhProc
        exit
        (callerThis, callerScope, EdhObject $ Object ent (scopeClass world) [])
      )
      pgs -- execute outer code wrt what tx state originally is
  -- execution of the args receiving always in a tx for atomicity
  local (\pgs' -> pgs' { edh'in'tx = True }) $ case argsRcvr of
    PackReceiver argRcvrs -> return $ do
      (pck', em) <- foldM recvFromPack (pck, Map.empty) argRcvrs
      ent        <- woResidual pck' em
      doReturn ent
    SingleReceiver argRcvr -> return $ do
      (pck', em) <- recvFromPack (pck, Map.empty) argRcvr
      ent        <- woResidual pck' em
      doReturn ent
    WildReceiver -> return $ if null posArgs
      then do
        ent <- newTVar $ Map.mapKeys AttrByName kwArgs
        doReturn ent
      else
        throwEdhFromSTM pgs EvalError
        $  "Unexpected "
        <> T.pack (show $ length posArgs)
        <> " positional argument(s) to wild receiver"


packEdhArgs :: Object -> ArgsSender -> EdhProcExit -> EdhProg (STM ())
-- make sure values in a pack are evaluated in same tx
packEdhArgs that argsSender exit =
  local (\s -> s { edh'in'tx = True }) $ case argsSender of
    PackSender   argSenders -> packEdhArgs' that argSenders exit
    SingleSender argSender  -> packEdhArgs' that [argSender] exit

packEdhArgs' :: Object -> [ArgSender] -> EdhProcExit -> EdhProg (STM ())
packEdhArgs' that [] exit = do
  !pgs <- ask
  let !ctx                     = edh'context pgs
      !scope@(Scope _ !this _) = contextScope ctx
  exit (this, scope, EdhArgsPack $ ArgsPack [] Map.empty)
packEdhArgs' that (x : xs) exit = do
  !pgs <- ask
  let !ctx                     = edh'context pgs
      !scope@(Scope _ !this _) = contextScope ctx
      dictKey2Kw :: ItemKey -> STM AttrName
      dictKey2Kw = \case
        ItemByStr name -> return name
        k ->
          throwEdhFromSTM pgs EvalError
            $  "Invalid argument keyword from dict key: "
            <> T.pack (show k)
  case x of
    UnpackPosArgs listExpr -> evalExpr that listExpr $ \case
      (_, _, EdhList (List l)) -> packEdhArgs' that xs $ \(_, _, pk) ->
        case pk of
          EdhArgsPack (ArgsPack !posArgs !kwArgs) -> return $ do
            ll <- readTVar l
            join $ runReaderT
              (exitEdhProc
                exit
                (this, scope, EdhArgsPack (ArgsPack (posArgs ++ ll) kwArgs))
              )
              pgs
          _ -> error "bug"
      (_, _, v) ->
        throwEdh EvalError $ "Can not unpack args from: " <> T.pack (show v)
    UnpackKwArgs dictExpr -> evalExpr that dictExpr $ \case
      (_, _, EdhDict (Dict ds)) -> packEdhArgs' that xs $ \(_, _, pk) ->
        case pk of
          EdhArgsPack (ArgsPack !posArgs !kwArgs) -> return $ do
            dm  <- readTVar ds
            kvl <- forM (Map.toAscList dm) $ \(k, v) -> (, v) <$> dictKey2Kw k
            join $ runReaderT
              (exitEdhProc
                exit
                ( this
                , scope
                , EdhArgsPack
                  (ArgsPack posArgs $ Map.union kwArgs $ Map.fromAscList kvl)
                )
              )
              pgs
          _ -> error "bug"
      (_, _, v) ->
        throwEdh EvalError $ "Can not unpack kwargs from: " <> T.pack (show v)
    SendPosArg argExpr -> evalExpr that argExpr $ \(_, _, val) ->
      packEdhArgs' that xs $ \(_, _, pk) -> case pk of
        EdhArgsPack (ArgsPack !posArgs !kwArgs) -> exitEdhProc
          exit
          (this, scope, EdhArgsPack (ArgsPack (val : posArgs) kwArgs))
        _ -> error "bug"
    SendKwArg kw argExpr -> evalExpr that argExpr $ \(_, _, val) ->
      packEdhArgs' that xs $ \(_, _, pk) -> case pk of
        EdhArgsPack (ArgsPack !posArgs !kwArgs) -> exitEdhProc
          exit
          ( this
          , scope
          , EdhArgsPack
            (ArgsPack posArgs $ Map.alter
              (\case -- make sure latest value with same kw take effect
                Nothing       -> Just val
                Just laterVal -> Just laterVal
              )
              kw
              kwArgs
            )
          )
        _ -> error "bug"


-- | resolve an attribute addressor, either alphanumeric named or symbolic
resolveAddr :: EdhProgState -> AttrAddressor -> STM AttrKey
resolveAddr _ (NamedAttr !attrName) = return (AttrByName attrName)
resolveAddr !pgs (SymbolicAttr !symName) =
  let scope = contextScope $ edh'context pgs
  in  resolveEdhCtxAttr scope (AttrByName symName) >>= \case
        Just scope' -> do
          em <- readTVar (scopeEntity scope')
          case Map.lookup (AttrByName symName) em of
            Just (EdhSymbol !symVal) -> return (AttrBySym symVal)
            Just v ->
              throwEdhFromSTM pgs EvalError
                $  "Not a symbol: "
                <> T.pack (show v)
                <> " as "
                <> symName
                <> " from "
                <> T.pack (show $ thisObject scope') -- TODO this correct ?
            Nothing -> error "bug in ctx attr resolving"
        Nothing ->
          throwEdhFromSTM pgs EvalError
            $  "No symbol named "
            <> T.pack (show symName)
            <> " available"


-- Edh attribute resolution

resolveEdhCtxAttr :: Scope -> AttrKey -> STM (Maybe Scope)
resolveEdhCtxAttr scope@(Scope !ent !this _sp) !addr = readTVar ent >>= \em ->
  if Map.member addr em
    then -- directly present on current scope
         return (Just scope)
    else -- skip searching `this` object here, so within a method procedure you
         -- have to be explicit if intend to address attribute off `this` object,
         -- by writing `this.xxx`
         resolveLexicalAttr (classContext $ objClass this) addr

resolveLexicalAttr :: [Scope] -> AttrKey -> STM (Maybe Scope)
resolveLexicalAttr [] _ = return Nothing
resolveLexicalAttr (scope@(Scope !ent !obj _sp) : outerEntities) addr =
  readTVar ent >>= \em -> if Map.member addr em
    then -- directly present on current scope
         return (Just scope)
    else -- go for the interesting attribute from inheritance hierarchy
         -- of this context object, so a module can `extends` some objects
         -- too, in addition to the `import` mechanism
      (if ent == objEntity obj
          -- go directly to supers as entity is just searched
          then readTVar (objSupers obj) >>= resolveEdhSuperAttr addr
          -- context scope is different entity from this context object,
          -- start next from this object
          else resolveEdhObjAttr obj addr
        )
        >>= \case
              Just scope'from'object -> return $ Just scope'from'object
              -- go one level outer of the lexical stack
              Nothing                -> resolveLexicalAttr outerEntities addr

resolveEdhObjAttr :: Object -> AttrKey -> STM (Maybe Scope)
resolveEdhObjAttr !this !addr = readTVar thisEnt >>= \em ->
  if Map.member addr em
    then return (Just $ Scope thisEnt this clsProc)
    else readTVar (objSupers this) >>= resolveEdhSuperAttr addr
 where
  !thisEnt = objEntity this
  clsProc  = classProcedure (objClass this)

resolveEdhSuperAttr :: AttrKey -> [Object] -> STM (Maybe Scope)
resolveEdhSuperAttr _ [] = return Nothing
resolveEdhSuperAttr !addr (super : restSupers) =
  resolveEdhObjAttr super addr >>= \case
    Just scope -> return $ Just scope
    Nothing    -> resolveEdhSuperAttr addr restSupers

