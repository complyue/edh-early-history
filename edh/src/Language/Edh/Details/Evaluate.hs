
module Language.Edh.Details.Evaluate where

import           Prelude
-- import           Debug.Trace

import           GHC.Conc                       ( unsafeIOToSTM )

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Concurrent.STM

import           Data.Maybe
import qualified Data.ByteString               as B
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import qualified Data.Map.Strict               as Map
import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , (<|)
                                                )
import qualified Data.List.NonEmpty            as NE

import           Text.Megaparsec

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Parser
import           Language.Edh.Event
import           Language.Edh.Details.RtTypes
import           Language.Edh.Details.CoreLang
import           Language.Edh.Details.PkgMan
import           Language.Edh.Details.Utils


deParen :: Expr -> Expr
deParen x = case x of
  ParenExpr x' -> deParen x'
  _            -> x

deBlock :: StmtSrc -> [StmtSrc]
deBlock stmt = case stmt of
  (StmtSrc (_, ExprStmt (BlockExpr stmts'))) -> stmts'
  _ -> [stmt]


evalStmt :: Object -> StmtSrc -> EdhProcExit -> EdhProg (STM ())
evalStmt that ss@(StmtSrc (_, !stmt)) !exit =
  local (\pgs -> pgs { edh'context = (edh'context pgs) { contextStmt = ss } })
    $ evalStmt' that stmt exit


evalBlock :: Object -> [StmtSrc] -> EdhProcExit -> EdhProg (STM ())
evalBlock that [] !exit = do
  !pgs <- ask
  let !ctx   = edh'context pgs
      !scope = contextScope ctx
  -- empty block evals to nil
  exitEdhProc exit (that, scope, nil)
evalBlock that [!ss] !exit = evalStmt that ss $ \result -> case result of
  -- last branch does match
  (!this', !scope', EdhCaseClose !val) -> exitEdhProc exit (this', scope', val)
  -- explicit `fallthrough` at end of this block, cascade to outer block
  fall@(_, _, EdhFallthrough) -> exitEdhProc exit fall
  -- ctrl to be propagated outwards
  brk@(_, _, EdhBreak) -> exitEdhProc exit brk
  ctn@(_, _, EdhContinue) -> exitEdhProc exit ctn
  rtn@(_, _, EdhReturn _) -> exitEdhProc exit rtn
  -- yield should have been handled by 'evalExpr'
  (_, _, EdhYield _) -> throwEdh EvalError "bug yield reached block"
  -- last stmt no special branching result, propagate as is
  _ -> exitEdhProc exit result
evalBlock that (ss : rest) !exit = evalStmt that ss $ \result -> case result of
  -- this branch matches without fallthrough, done this block
  (!this', !scope', EdhCaseClose !val) -> exitEdhProc exit (this', scope', val)
  -- should fallthrough to next branch (or stmt)
  (_, _, EdhFallthrough) -> evalBlock that rest exit
  -- ctrl to be propagated outwards
  brk@(_, _, EdhBreak) -> exitEdhProc exit brk
  ctn@(_, _, EdhContinue) -> exitEdhProc exit ctn
  rtn@(_, _, EdhReturn _) -> exitEdhProc exit rtn
  -- yield should have been handled by 'evalExpr'
  (_, _, EdhYield _) -> throwEdh EvalError "bug yield reached block"
  -- no special branching result, continue this block
  _ -> evalBlock that rest exit


evalExprs :: Object -> [Expr] -> EdhProcExit -> EdhProg (STM ())
-- here 'EdhTuple' is used for intermediate tag,
-- not returning real tuple values as in Edh.
evalExprs _ [] !exit = do
  !pgs <- ask
  let !ctx                       = edh'context pgs
      !scope@(Scope _ !this _ _) = contextScope ctx
  exit (this, scope, EdhTuple [])
evalExprs !that (x : xs) !exit = evalExpr that x $ \(this', scope', val) ->
  evalExprs that xs $ \(_, _, tv) -> case tv of
    EdhTuple l -> exitEdhProc exit (this', scope', EdhTuple (val : l))
    _          -> error "bug"


evalStmt' :: Object -> Stmt -> EdhProcExit -> EdhProg (STM ())
evalStmt' that stmt exit = do
  !pgs <- ask
  let !ctx@(Context !world !call'stack _ _ (StmtSrc (srcPos, _))) =
        edh'context pgs
      !scope@(Scope !ent !this _ _) = contextScope ctx
  case stmt of

    ExprStmt expr             -> evalExpr that expr exit

    LetStmt argsRcvr argsSndr -> do
      -- ensure args sending and receiving happens within a same tx
      -- for atomicity of the let statement
      local (const pgs { edh'in'tx = True })
        $ packEdhArgs that argsSndr
        $ \(_, _, pkv) -> case pkv of
            EdhArgsPack pk ->
              recvEdhArgs ctx argsRcvr pk $ \(_, _, scopeObj) ->
                case scopeObj of
                  EdhObject (Object rcvd'ent rcvd'cls _)
                    | rcvd'cls == (objClass $ scopeSuper world) -> contEdhSTM
                    $  do
                      -- overwrite current scope entity with attributes from
                      -- the received entity
                         um <- readTVar rcvd'ent
                         modifyTVar' ent $ \em -> Map.union um em
                         -- let statement evaluates to nil always
                         exitEdhSTM pgs exit (this, scope, nil)
                  _ -> error "bug"
            _ -> error "bug"

    BreakStmt       -> exitEdhProc exit (this, scope, EdhBreak)
    ContinueStmt    -> exitEdhProc exit (this, scope, EdhContinue)
    FallthroughStmt -> exitEdhProc exit (this, scope, EdhFallthrough)

    ReturnStmt expr -> evalExpr that expr $ \(this', scope', !val) ->
      exitEdhProc exit (this', scope', EdhReturn val)

    AtoIsoStmt expr ->
      local (\pgs' -> pgs' { edh'in'tx = True })
        $ evalExpr that expr
        -- restore tx state after target expr evaluated
        $ \result -> local (const pgs) $ exitEdhProc exit result

    GoStmt expr -> forkEdh exit $ evalExpr that expr edhNop

    ReactorStmt sinkExpr argsRcvr reactionStmt ->
      evalExpr that sinkExpr $ \case
        (_, _, EdhSink sink) -> contEdhSTM $ do
          reactorChan <- dupTChan $ evs'chan sink
          modifyTVar' (edh'reactors pgs)
                      ((reactorChan, ctx, argsRcvr, reactionStmt) :)
        (_, _, val) ->
          throwEdh EvalError
            $  "Can only reacting to an (event) sink, not a "
            <> T.pack (show $ edhTypeOf val)
            <> ": "
            <> T.pack (show val)

    DeferStmt expr -> contEdhSTM $ modifyTVar' (edh'defers pgs) ((ctx, expr) :)


    -- TODO impl. this
    -- TryStmt trunkStmt catchesList finallyStmt -> undefined
    -- ThrowStmt excExpr                         -> undefined

    WhileStmt cndExpr bodyStmt -> do
      let !stmts = deBlock bodyStmt
          doWhile :: EdhProg (STM ())
          doWhile = do
            evalExpr that cndExpr $ \case
              (_, _, EdhBool True) ->
                evalBlock that stmts $ \doneResult@(_, _, doneVal) ->
                  case doneVal of
                    -- | early stop of procedure
                    EdhReturn _    -> exitEdhProc exit doneResult
                    -- | break while loop
                    EdhBreak       -> exitEdhProc exit (that, scope, nil)
                    -- | treat as break here, TODO judge this decision
                    EdhFallthrough -> exitEdhProc exit (that, scope, nil)
                    -- | treat as continue here, TODO judge this decision
                    EdhCaseClose _ -> exitEdhProc exit doneResult
                    -- continue while loop
                    _              -> doWhile
              (_, _, EdhBool False) -> exitEdhProc exit (that, scope, nil)
              (_, _, EdhNil       ) -> exitEdhProc exit (that, scope, nil)
              (_, _, cndVal) ->
                throwEdh EvalError
                  $  "Invalid condition value for while: "
                  <> T.pack (show $ edhTypeOf cndVal)
                  <> ": "
                  <> T.pack (show cndVal)
      doWhile

    ExtendsStmt superExpr -> evalExpr that superExpr $ \case
      (_, _, EdhObject superObj) -> contEdhSTM $ do
        modifyTVar' (objSupers this) (superObj :)
        exitEdhSTM pgs exit (that, scope, nil)
      (_, _, superVal) ->
        throwEdh EvalError
          $  "Can only extends an object, not "
          <> T.pack (show $ edhTypeOf superVal)
          <> ": "
          <> T.pack (show superVal)

    ClassStmt pd@(ProcDecl name _ _) -> contEdhSTM $ do
      let
        !cls =
          EdhClass $ Class { classLexiStack = call'stack, classProcedure = pd }
      when (name /= "_") $ modifyTVar' ent $ Map.insert (AttrByName name) cls
      exitEdhSTM pgs exit (this, scope, cls)

    MethodStmt pd@(ProcDecl name _ _) -> contEdhSTM $ do
      let
        mth = EdhMethod
          $ Method { methodLexiStack = call'stack, methodProcedure = pd }
      when (name /= "_") $ modifyTVar' ent $ Map.insert (AttrByName name) mth
      exitEdhSTM pgs exit (this, scope, mth)

    GeneratorStmt pd@(ProcDecl name _ _) -> contEdhSTM $ do
      let gdf = EdhGenrDef $ GenrDef { generatorLexiStack = call'stack
                                     , generatorProcedure = pd
                                     }
      when (name /= "_") $ modifyTVar' ent $ Map.insert (AttrByName name) gdf
      exitEdhSTM pgs exit (this, scope, gdf)

    InterpreterStmt pd@(ProcDecl name _ _) -> contEdhSTM $ do
      let mth = EdhInterpreter $ Interpreter
            { interpreterLexiStack = call'stack
            , interpreterProcedure = pd
            }
      when (name /= "_") $ modifyTVar' ent $ Map.insert (AttrByName name) mth
      exitEdhSTM pgs exit (this, scope, mth)

    OpDeclStmt opSym opPrec opProc -> contEdhSTM $ do
      let op = EdhOperator $ Operator { operatorLexiStack   = call'stack
                                      , operatorProcedure   = opProc
                                      , operatorPredecessor = Nothing
                                      , operatorPrecedence  = opPrec
                                      }
      modifyTVar' ent $ \em -> Map.insert (AttrByName opSym) op em
      exitEdhSTM pgs exit (this, scope, op)

    OpOvrdStmt opSym opProc opPrec -> contEdhSTM $ do
      let findPredecessor :: STM (Maybe EdhValue)
          findPredecessor = do
            lookupEdhCtxAttr scope (AttrByName opSym) >>= \case
              Nothing -> -- do
                -- (EdhRuntime logger _) <- readTMVar $ worldRuntime world
                -- logger 30 (Just $ sourcePosPretty srcPos)
                --   $ ArgsPack
                --       [EdhString "overriding an unavailable operator"]
                --       Map.empty
                return Nothing
              Just hostOper@(EdhHostOper _ _) -> return $ Just hostOper
              Just surfOper@(EdhOperator _  ) -> return $ Just surfOper
              Just opVal                      -> do
                (EdhRuntime logger _) <- readTMVar $ worldRuntime world
                logger 30 (Just $ sourcePosPretty srcPos) $ ArgsPack
                  [ EdhString
                    $  "overriding an invalid operator "
                    <> T.pack (show $ edhTypeOf opVal)
                    <> ": "
                    <> T.pack (show opVal)
                  ]
                  Map.empty
                return Nothing
      predecessor <- findPredecessor
      let op = EdhOperator $ Operator { operatorLexiStack   = call'stack
                                      , operatorProcedure   = opProc
                                      , operatorPredecessor = predecessor
                                      , operatorPrecedence  = opPrec
                                      }
      modifyTVar' ent $ \em -> Map.insert (AttrByName opSym) op em
      exitEdhSTM pgs exit (this, scope, op)

    ImportStmt argsRcvr srcExpr -> case srcExpr of
      LitExpr (StringLiteral importSpec) ->
        -- import from specified path
        importEdhModule argsRcvr importSpec exit
      _ -> evalExpr that srcExpr $ \(_, _, srcVal) -> case srcVal of
        EdhObject (Object !fromEnt _ _) ->
          -- import from an object
          importFromEntity argsRcvr fromEnt exit
        _ ->
          -- todo support more sources of import ?
          throwEdh EvalError
            $  "Don't know how to import from "
            <> T.pack (show $ edhTypeOf srcVal)
            <> ": "
            <> T.pack (show srcVal)

    VoidStmt -> exitEdhProc exit (this, scope, nil)

    _ -> throwEdh EvalError $ "Eval not yet impl for: " <> T.pack (show stmt)


importFromEntity :: ArgsReceiver -> Entity -> EdhProcExit -> EdhProg (STM ())
importFromEntity !argsRcvr !fromEnt !exit = do
  pgs <- ask
  let !ctx@(  Context !world _ _ _ _) = edh'context pgs
      !scope@(Scope !ent !this _ _  ) = contextScope ctx
  contEdhSTM $ do
    emImp <- readTVar fromEnt
    let !artsPk = ArgsPack [] $ Map.fromAscList $ catMaybes
          [ (case k of
-- only attributes with a name not started with `_` are importable,
-- and all symbol values are not importable however named
              AttrByName attrName | not (T.isPrefixOf "_" attrName) -> case v of
                EdhSymbol _ -> Nothing
                _           -> Just (attrName, v)
-- symbolic attributes are effective stripped off, this is desirable so that
-- symbolic attributes are not importable, thus private to a module/object
              _ -> Nothing
            )
          | (k, v) <- Map.toAscList emImp
          ]
    runEdhProg pgs $ recvEdhArgs ctx argsRcvr artsPk $ \(_, _, scopeObj) ->
      case scopeObj of
        EdhObject (Object rcvd'ent rcvd'cls _)
          | rcvd'cls == (objClass $ scopeSuper world) -> contEdhSTM $ do
            rcvd'em <- readTVar rcvd'ent
            modifyTVar' ent $ Map.union rcvd'em
            exitEdhSTM pgs exit (this, scope, nil)
        _ -> error "bug"

importEdhModule :: ArgsReceiver -> Text -> EdhProcExit -> EdhProg (STM ())
importEdhModule !argsRcvr !impSpec !exit = do
  pgs <- ask
  let !ctx   = edh'context pgs
      !world = contextWorld ctx
      !scope = contextScope ctx
  if edh'in'tx pgs
    then throwEdh EvalError "You don't import from within a transaction"
    else contEdhSTM $ lookupEdhCtxAttr scope (AttrByName "__file__") >>= \case
      Just (EdhString fromModuPath) -> do
        (nomPath, moduFile) <- locateEdhModule
          pgs
          (edhPkgPathFrom $ T.unpack fromModuPath)
          (T.unpack impSpec)
        let !moduId = T.pack nomPath
        moduMap <- takeTMVar (worldModules world)
        case Map.lookup moduId moduMap of
          Just !moduSlot -> do
            -- put back immediately
            putTMVar (worldModules world) moduMap
            -- blocking wait the target module loaded, then do import
            readTMVar moduSlot >>= \case
              -- TODO GHC should be able to detect cyclic imports as 
              --      deadlock, find ways to report that more friendly
              EdhObject modu ->
                runEdhProg pgs $ importFromEntity argsRcvr (objEntity modu) exit
              importError -> -- the first importer failed loading it,
                -- replicate the error in this thread
                throwEdhSTM pgs EvalError $ edhValueStr importError
          Nothing -> do  -- we are the first importer
            -- allocate an empty slot
            moduSlot <- newEmptyTMVar
            -- put it for global visibility
            putTMVar (worldModules world) $ Map.insert moduId moduSlot moduMap
            catchSTM
                (loadModule pgs moduSlot moduId moduFile $ \result ->
                  case result of
                    (_, _, EdhObject modu) ->
                      -- do the import after module successfully loaded
                      importFromEntity argsRcvr (objEntity modu) exit
                    _ -> error "bug"
                )
              $ \(e :: SomeException) -> do
                  -- cleanup on loading error
                  let errStr = T.pack (show e)
                  void $ tryPutTMVar moduSlot $ EdhString errStr
                  moduMap' <- takeTMVar (worldModules world)
                  case Map.lookup moduId moduMap' of
                    Just moduSlot' -> if moduSlot' == moduSlot
                      then putTMVar (worldModules world)
                        $ Map.delete moduId moduMap'
                      else putTMVar (worldModules world) moduMap'
                    _ -> putTMVar (worldModules world) moduMap'
                  throwSTM e
      _ -> error "bug: no valid `__file__` in context"

loadModule
  :: EdhProgState
  -> TMVar EdhValue
  -> ModuleId
  -> FilePath
  -> EdhProcExit
  -> STM ()
loadModule pgs moduSlot moduId moduFile exit = if edh'in'tx pgs
  then throwEdhSTM pgs
                   EvalError
                   "You don't load a module from within a transaction"
  else do
    unsafeIOToSTM (streamDecodeUtf8With lenientDecode <$> B.readFile moduFile)
      >>= \case
            Some moduSource _ _ -> do
              let !ctx                       = edh'context pgs
                  !world                     = contextWorld ctx
                  !scope@(Scope _ !this _ _) = contextScope ctx
                  !wops                      = worldOperators world
              -- serialize parsing against 'worldOperators'
              opPD <- takeTMVar wops
              flip
                  catchSTM
                  (\(e :: SomeException) -> tryPutTMVar wops opPD >> throwSTM e)
                $ do
                    -- parse module source
                    let (pr, opPD') = runState
                          (runParserT parseProgram (T.unpack moduId) moduSource)
                          opPD
                    case pr of
                      Left  !err   -> throwSTM $ EdhParseError err
                      Right !stmts -> do
                        -- release world lock as soon as parsing done successfuly
                        putTMVar wops opPD'
                        -- prepare the module meta data
                        !moduEntity <- newTVar $ Map.fromList
                          [ (AttrByName "__name__", EdhString moduId)
                          , (AttrByName "__file__", EdhString $ T.pack moduFile)
                          ]
                        !moduSupers <- newTVar []
                        let !modu = Object { objEntity = moduEntity
                                           , objClass  = moduleClass world
                                           , objSupers = moduSupers
                                           }
                        -- run statements from the module with its own context
                        runEdhProg pgs { edh'context = moduleContext world modu
                                       }
                          $ evalBlock modu stmts
                          $ \_ -> contEdhSTM $ do
                              -- arm the successfully loaded module
                              putTMVar moduSlot $ EdhObject modu
                              -- switch back to module importer's scope and continue
                              exitEdhSTM pgs exit (this, scope, EdhObject modu)

moduleContext :: EdhWorld -> Object -> Context
moduleContext !w !mo = Context { contextWorld    = w
                               , callStack       = moduScope <| rootScope
                               , generatorCaller = Nothing
                               , contextMatch    = true
                               , contextStmt     = voidStatement
                               }
 where
  !moduScope = Scope (objEntity mo)
                     mo
                     (NE.toList rootScope)
                     (classProcedure $ moduleClass w)
  !rootScope = (classLexiStack $ moduleClass w)

voidStatement :: StmtSrc
voidStatement = StmtSrc
  ( SourcePos { sourceName   = "<Genesis>"
              , sourceLine   = mkPos 1
              , sourceColumn = mkPos 1
              }
  , VoidStmt
  )


indexingMethodArg :: EdhProgState -> ArgsReceiver -> STM AttrName
indexingMethodArg _ (SingleReceiver (RecvArg ixAttrName Nothing Nothing)) =
  return ixAttrName
indexingMethodArg _ (PackReceiver [(RecvArg ixAttrName Nothing Nothing)]) =
  return ixAttrName
indexingMethodArg pgs _ =
  throwEdhSTM pgs EvalError "Invalid ([]) method signature"

indexingAssignMethodArg
  :: EdhProgState -> ArgsReceiver -> STM (AttrName, AttrName)
indexingAssignMethodArg _ (PackReceiver [(RecvArg ixAttrName Nothing Nothing), (RecvArg valAttrName Nothing Nothing)])
  = return (ixAttrName, valAttrName)
indexingAssignMethodArg pgs _ =
  throwEdhSTM pgs EvalError "Invalid ([=]) method signature"


evalExpr :: Object -> Expr -> EdhProcExit -> EdhProg (STM ())
evalExpr that expr exit = do
  !pgs <- ask
  let !ctx@(Context !world !call'stack genr'caller _ (StmtSrc (srcPos, _))) =
        edh'context pgs
      !scope@(Scope _ !this _ _) = contextScope ctx
  case expr of
    LitExpr lit -> case lit of
      DecLiteral    v -> exitEdhProc exit (this, scope, EdhDecimal v)
      StringLiteral v -> exitEdhProc exit (this, scope, EdhString v)
      BoolLiteral   v -> exitEdhProc exit (this, scope, EdhBool v)
      NilLiteral      -> exitEdhProc exit (this, scope, nil)
      TypeLiteral v   -> exitEdhProc exit (this, scope, EdhType v)

      SinkCtor        -> contEdhSTM $ do
        es <- newEventSink
        exitEdhSTM pgs exit (this, scope, EdhSink es)

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
      Guard -> contEdhSTM $ do
        (EdhRuntime logger _) <- readTMVar $ worldRuntime world
        logger
          30
          (Just $ sourcePosPretty srcPos)
          (ArgsPack [EdhString $ "Standalone guard treated as plain value."]
                    Map.empty
          )
        runEdhProg pgs $ evalExpr that expr' exit

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
          EdhTuple l -> contEdhSTM $ do
            dpl <- forM l $ \case
              EdhPair kVal vVal -> (, vVal) <$> case kVal of
                EdhType    k -> return $ ItemByType k
                EdhString  k -> return $ ItemByStr k
                EdhSymbol  k -> return $ ItemBySym k
                EdhDecimal k -> return $ ItemByNum k
                EdhBool    k -> return $ ItemByBool k
                k ->
                  throwEdhSTM pgs EvalError
                    $  "Invalid dict key: "
                    <> T.pack (show k)
                    <> " ❌"
              pv ->
                throwEdhSTM pgs EvalError
                  $  "Invalid dict entry: "
                  <> T.pack (show pv)
                  <> " ❌"
            ds <- newTVar $ Map.fromList dpl
            exitEdhSTM pgs exit (this, scope, EdhDict (Dict ds))
          _ -> error "bug"

    ListExpr xs -> -- make sure list values are evaluated in same tx
      local (\s -> s { edh'in'tx = True }) $ evalExprs that xs $ \(_, _, tv) ->
        case tv of
          EdhTuple l -> contEdhSTM $ do
            ll <- List <$> newTVar l
            exitEdhSTM pgs exit (this, scope, EdhList ll)
          _ -> error "bug"

    TupleExpr xs -> -- make sure tuple values are evaluated in same tx
      local (\s -> s { edh'in'tx = True }) $ evalExprs that xs $ \(_, _, tv) ->
        case tv of
          EdhTuple l -> exitEdhProc exit (this, scope, EdhTuple l)
          _          -> error "bug"

    ParenExpr x -> evalExpr that x exit

    BlockExpr stmts ->
      -- eval the block with `true` being the 'contextMatch'
      local
          (\pgs' ->
            pgs' { edh'context = (edh'context pgs') { contextMatch = true } }
          )
        $ evalBlock that stmts
        $ \blkResult -> -- restore program state after block done
                        local (const pgs) $ exitEdhProc exit blkResult

    CaseExpr tgtExpr branchesStmt ->
      evalExpr that tgtExpr $ \(_, _, tgtVal) ->
        -- eval the block with the case target being the 'contextMatch'
        local
            (\pgs' -> pgs'
              { edh'context = (edh'context pgs') { contextMatch = tgtVal }
              }
            )
          $ evalBlock that (deBlock branchesStmt)
            -- restore program state after block done
          $ \blkResult -> local (const pgs) $ exitEdhProc exit blkResult


    -- yield stmt evals to the value of caller's `do` expression
    YieldExpr yieldExpr -> evalExpr that yieldExpr $ \yieldResult ->
      case genr'caller of
        Nothing -> throwEdh EvalError "Unexpected yield"
        Just (pgs', yieldTo) ->
          contEdhSTM $ runEdhProg pgs' $ yieldTo yieldResult $ \doResult ->
            exitEdhSTM pgs exit doResult

    ForExpr argsRcvr iterExpr doExpr ->
      runForLoop that argsRcvr iterExpr doExpr (const $ return ()) exit

    AttrExpr addr -> case addr of
      ThisRef          -> exitEdhProc exit (this, scope, EdhObject this)
      ThatRef          -> exitEdhProc exit (that, scope, EdhObject that)
      DirectRef !addr' -> contEdhSTM $ do
        !key <- resolveAddr pgs addr'
        resolveEdhCtxAttr scope key >>= \case
          Nothing ->
            throwEdhSTM pgs EvalError $ "Not in scope: " <> T.pack (show addr')
          Just scope'@(Scope !ent' _ _ _) -> do
            em <- readTVar ent'
            case Map.lookup key em of
              Nothing  -> error "attr resolving bug"
              Just val -> exitEdhSTM pgs exit (this, scope', val)
      IndirectRef !tgtExpr !addr' -> contEdhSTM $ do
        key <- resolveAddr pgs addr'
        runEdhProg pgs $ evalExpr that tgtExpr $ \case
          (_, _, EdhObject !obj) ->
            return $ resolveEdhObjAttr obj key >>= \case
              Nothing ->
                throwEdhSTM pgs EvalError
                  $  "No such attribute "
                  <> T.pack (show key)
                  <> " from "
                  <> T.pack (show obj)
              Just scope'@(Scope !ent' _ _ _) -> do
                em <- readTVar ent'
                case Map.lookup key em of
                  Nothing  -> error "attr resolving bug"
                  Just val -> exitEdhSTM pgs exit (obj, scope', val)
          (_, _, v) ->
            throwEdh EvalError $ "Not an object: " <> T.pack (show v)


    IndexExpr ixExpr tgtExpr -> evalExpr that ixExpr $ \(_, _, ixVal) ->
      evalExpr that tgtExpr $ \case

        -- indexing a dict
        (that', scope', EdhDict (Dict d)) -> contEdhSTM $ do
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
          ds <- readTVar d
          case Map.lookup ixKey ds of
            Nothing  -> exitEdhSTM pgs exit (that', scope', nil)
            Just val -> exitEdhSTM pgs exit (that', scope', val)

        -- indexing an object, by calling its ([]) method with ixVal as the single arg
        (_, _, EdhObject obj) ->
          contEdhSTM $ lookupEdhObjAttr obj (AttrByName "[]") >>= \case
            Nothing ->
              throwEdhSTM pgs EvalError $ "No ([]) method from: " <> T.pack
                (show obj)
            Just (EdhMethod (Method mth'lexi'stack mthProc@(ProcDecl _ mth'args mth'body)))
              -> do
                ixAttrName <- indexingMethodArg pgs mth'args
                ixMthEnt   <- newTVar
                  $ Map.fromList [(AttrByName ixAttrName, ixVal)]
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
              throwEdhSTM pgs EvalError
                $  "Malformed index method ([]) on "
                <> T.pack (show obj)
                <> " - "
                <> T.pack (show $ edhTypeOf badIndexer)
                <> ": "
                <> T.pack (show badIndexer)

        (_, _, tgtVal) ->
          throwEdh EvalError
            $  "Don't know how to index "
            <> T.pack (show $ edhTypeOf tgtVal)
            <> ": "
            <> T.pack (show tgtVal)
            <> " with "
            <> T.pack (show $ edhTypeOf ixVal)
            <> ": "
            <> T.pack (show ixVal)


    CallExpr procExpr argsSndr -> evalExpr that procExpr $ \case

      -- calling a host procedure
      (that', hp'home, EdhHostProc (HostProcedure _ proc)) ->
        proc argsSndr that' hp'home exit

      -- calling a host generator
      (_, _, EdhHostGenr _) ->
        throwEdh EvalError "Can only call a host generator by for-from-do"


      -- calling a class (constructor) procedure
      (that', _, EdhClass cls@(Class cls'lexi'stack clsProc@(ProcDecl _ proc'args proc'body)))
        -> do
          -- ensure args sending and receiving happens within a same tx for atomicity of
          -- the call making
          let !calleeCtx = ctx { callStack = cls'lexi'stack }
          local (const pgs { edh'in'tx = True })
            $ packEdhArgs that argsSndr
            $ \(_, _, pkv) -> case pkv of
                EdhArgsPack pk ->
                  recvEdhArgs calleeCtx proc'args pk $ \(_, _, scopeObj) ->
                    case scopeObj of
                      EdhObject (Object rcvd'ent rcvd'cls _)
                        | rcvd'cls == (objClass $ scopeSuper world) -> contEdhSTM
                        $ do
                            newThis <- viewAsEdhObject rcvd'ent cls []
                            runEdhProg pgs
                             -- use new this as the scope entity of instance ctor execution
                                { edh'context =
                                  ctx
                                    { callStack =
                                      (  Scope rcvd'ent
                                               newThis
                                               (NE.toList cls'lexi'stack)
                                               clsProc
                                      <| call'stack
                                      )
                                    }
                                 -- restore original tx state after args received
                                , edh'in'tx   = edh'in'tx pgs
                                }
                              $ evalStmt that' proc'body
                              $ \(that'', scope'', ctorRtn) ->
                               -- restore previous context after ctor returned 
                                  local (const pgs) $ case ctorRtn of
                               -- allow a class procedure to explicitly return other
                               -- value than newly constructed `this` object
                               -- it can still `return this to early stop the ctor proc
                               -- this is magically an advanced feature
                                    EdhReturn rtnVal -> exitEdhProc
                                      exit
                                      (that'', scope'', rtnVal)
                                    EdhContinue ->
                                      throwEdh EvalError "Unexpected continue"
                               -- allow the use of `break` to early stop a constructor 
                               -- procedure with nil result
                                    EdhBreak ->
                                      exitEdhProc exit (that'', scope'', nil)
                               -- no explicit return from class procedure, return the
                               -- newly constructed this object, throw away the last
                               -- value from the procedure execution
                                    _ -> exitEdhProc
                                      exit
                                      (this, scope, EdhObject newThis)
                      _ -> error "bug"
                _ -> error "bug"


      -- calling a method procedure
      (that', (Scope _ mth'this _ _), EdhMethod (Method mth'lexi'stack mthProc@(ProcDecl _ mth'args mth'body)))
        -> do
          -- ensure args sending and receiving happens within a same tx
          -- for atomicity of the call making
          let !calleeCtx = ctx { callStack = mth'lexi'stack }
          local (const pgs { edh'in'tx = True })
            $ packEdhArgs that argsSndr
            $ \(_, _, pkv) -> case pkv of
                EdhArgsPack pk ->
                  recvEdhArgs calleeCtx mth'args pk $ \(_, _, scopeObj) ->
                    case scopeObj of
                      EdhObject (Object rcvd'ent rcvd'cls _)
                        | rcvd'cls == (objClass $ scopeSuper world)
                        ->
                          -- use direct containing object of the method as `this` in its
                          -- procedure execution
                           local
                            (const pgs
                              -- set method's scope
                              { edh'context =
                                ctx
                                  { callStack =
                                    (  Scope rcvd'ent
                                             mth'this
                                             (NE.toList mth'lexi'stack)
                                             mthProc
                                    <| call'stack
                                    )
                                  }
                                -- restore original tx state after args received
                              , edh'in'tx   = edh'in'tx pgs
                              }
                            )
                            -- use the resolution target object as `that` in execution of 
                            -- the method procedure
                          $ evalStmt that' mth'body
                          $ \(that'', scope'', mthRtn) ->
                              -- restore previous context after method returned
                              local (const pgs) $ case mthRtn of
                                EdhContinue ->
                                  throwEdh EvalError "Unexpected continue"
                                -- allow the use of `break` to early stop a method 
                                -- procedure with nil result
                                EdhBreak ->
                                  exitEdhProc exit (that', scope, nil)
                                -- explicit return
                                EdhReturn rtnVal ->
                                  exitEdhProc exit (that'', scope'', rtnVal)
                                -- no explicit return, assuming it returns the last
                                -- value from procedure execution
                                _ -> exitEdhProc exit (that'', scope'', mthRtn)
                      _ -> error "bug"
                _ -> error "bug"

      (_, _, EdhGenrDef _) ->
        throwEdh EvalError "Can only call a generator method by for-from-do"

      -- calling an interpreter procedure
      (that', (Scope _ mth'this _ _), EdhInterpreter (Interpreter mth'lexi'stack mthProc@(ProcDecl _ mth'args mth'body)))
        -> do
          let !calleeCtx = ctx { callStack = mth'lexi'stack }
          packEdhExprs that' argsSndr $ \(_, _, pk) -> case pk of
            EdhArgsPack (ArgsPack args kwargs) -> contEdhSTM $ do
              scopeWrapper <- mkScopeWrapper world scope
              runEdhProg pgs
                $ recvEdhArgs
                    calleeCtx
                    mth'args
                    (ArgsPack (EdhObject scopeWrapper : args) kwargs)
                $ \(_, _, scopeObj) -> case scopeObj of
                    EdhObject (Object rcvd'ent rcvd'cls _)
                      | rcvd'cls == (objClass $ scopeSuper world)
                      -> local
                          (const pgs
                            { edh'context =
                              ctx
                                { callStack = Scope rcvd'ent
                                                    mth'this
                                                    (NE.toList mth'lexi'stack)
                                                    mthProc
                                                <| call'stack
                                }
                            }
                          )
                        $ evalStmt that mth'body
                          -- pop call stack after interpreter proc returned
                        $ \(that'', scope'', mthRtn) ->
                            local (const pgs) $ exitEdhProc
                              exit
                              ( that''
                              , scope''
                              , case mthRtn of
                                -- explicit return
                                EdhReturn rtnVal -> rtnVal
                                -- no explicit return, assuming it returns the last
                                -- value from procedure execution
                                _                -> mthRtn
                              )
                    _ -> error "bug"
            _ -> error "bug"

      (_, _, val) ->
        throwEdh EvalError
          $  "Can not call: "
          <> T.pack (show val)
          <> " ❌ expressed with: "
          <> T.pack (show procExpr)


    InfixExpr !opSym !lhExpr !rhExpr ->
      return $ resolveEdhCtxAttr scope (AttrByName opSym) >>= \case
        Nothing ->
          throwEdhSTM pgs EvalError
            $  "Operator ("
            <> T.pack (show opSym)
            <> ") not in scope"
        Just scope'@(Scope !ent' _ _ _) -> do
          em <- readTVar ent'
          case Map.lookup (AttrByName opSym) em of
            Nothing -> error "attr resolving bug"
            -- run an operator implemented in Haskell
            Just (EdhHostOper _ (HostProcedure _ !proc)) ->
              -- with current this object as the operator's that object
              runEdhProg pgs
                $ proc [SendPosArg lhExpr, SendPosArg rhExpr] this scope' exit
            -- run an operator implemented in Edh
            Just (EdhOperator (Operator op'lexi'stack opProc@(ProcDecl _ op'args op'body) op'pred _))
              -> case op'args of
                -- 2 pos-args - simple lh/rh value receiving operator
                (PackReceiver [RecvArg lhName Nothing Nothing, RecvArg rhName Nothing Nothing])
                  -> runEdhProg pgs $ evalExpr that lhExpr $ \(_, _, lhVal) ->
                    evalExpr that rhExpr $ \(_, _, rhVal) -> contEdhSTM $ do
                      opEnt <-
                        newTVar
                        $  Map.fromList
                        $  [ (AttrByName lhName, lhVal)
                           , (AttrByName rhName, rhVal)
                           ]
                        ++ case op'pred of
-- put the overridden (predecessor) operator in the overriding operator's scope entity
                             Nothing       -> []
                             Just predProc -> [(AttrByName opSym, predProc)]
-- push operator procedure's scope to call stack
                      runEdhProg pgs
                          { edh'context =
                            ctx
                              { callStack =
                                (  Scope opEnt
                                         this -- todo should use op's lexi this instead ?
                                         (NE.toList op'lexi'stack)
                                         opProc
                                <| call'stack
                                )
                              }
                          }
                        $ evalStmt that op'body
-- pop call stack after operator proc returned
                        $ \(_, _, opRtn) -> local (const pgs) $ exitEdhProc
                            exit
                            ( that
                            , scope
                            , case opRtn of
                              -- explicit return
                              EdhReturn rtnVal -> rtnVal
                              -- no explicit return, assuming it returns the last
                              -- value from procedure execution
                              _                -> opRtn
                            )

                -- 3 pos-args - caller scope + lh/rh expr receiving operator
                (PackReceiver [RecvArg scopeName Nothing Nothing, RecvArg lhName Nothing Nothing, RecvArg rhName Nothing Nothing])
                  -> do
                    scopeWrapper <- mkScopeWrapper world scope
                    opEnt        <-
                      newTVar
                      $  Map.fromList
                      $  [ (AttrByName scopeName, EdhObject scopeWrapper)
                         , (AttrByName lhName   , EdhExpr lhExpr)
                         , (AttrByName rhName   , EdhExpr rhExpr)
                         ]
                      ++ case op'pred of
-- put the overridden (predecessor) operator in the overriding operator's scope entity
                           Nothing       -> []
                           Just predProc -> [(AttrByName opSym, predProc)]
-- push operator procedure's scope to call stack
                    runEdhProg pgs
                        { edh'context =
                          ctx
                            { callStack = Scope opEnt
                                                this -- todo should use op's lexi this instead ?
                                                (NE.toList op'lexi'stack)
                                                opProc
                                            <| call'stack
                            }
                        }
                      $ evalStmt that op'body
-- pop call stack after operator proc returned
                      $ \(_, _, opRtn) -> local (const pgs) $ exitEdhProc
                          exit
                          ( that
                          , scope
                          , case opRtn of
                            -- explicit return
                            EdhReturn rtnVal -> rtnVal
                            -- no explicit return, assuming it returns the last
                            -- value from procedure execution
                            _                -> opRtn
                          )

                _ ->
                  throwEdhSTM pgs EvalError
                    $  "Invalid operator signature: "
                    <> T.pack (show op'args)
            Just val ->
              throwEdhSTM pgs EvalError
                $  "Not callable: "
                <> T.pack (show val)
                <> " expressed with: "
                <> T.pack (show expr)


    -- _ -> throwEdh EvalError $ "Eval not yet impl for: " <> T.pack (show expr)


runForLoop
  :: Object
  -> ArgsReceiver
  -> Expr
  -> Expr
  -> ((Object, Scope, EdhValue) -> STM ())
  -> EdhProcExit
  -> EdhProg (STM ())
runForLoop !that !argsRcvr !iterExpr !doExpr !iterCollector !exit = do
  !pgs <- ask
  let !ctx@(  Context !world !call'stack _ _ _) = edh'context pgs
      !scope@(Scope !ent _ _ _                ) = contextScope ctx

      -- receive one yielded value from the generator, the 'exit' here is to
      -- continue the generator execution, result passed to the 'exit' here is
      -- the eval'ed value of the `yield` expression from the generator's 
      -- perspective
      recvYield
        :: (Object, Scope, EdhValue)
        -> ((Object, Scope, EdhValue) -> STM ())
        -> EdhProg (STM ())
      recvYield (_, _, yielded'val) exit' = case yielded'val of
        EdhContinue -> throwEdh EvalError "Unexpected continue from generator"
        EdhBreak    -> throwEdh EvalError "Unexpected break from generator"
        _ ->
          recvEdhArgs
              ctx
              argsRcvr
              (case yielded'val of
                EdhArgsPack pk' -> pk'
                _               -> ArgsPack [yielded'val] Map.empty
              )
            $ \(_, _, scopeObj') -> case scopeObj' of
                EdhObject (Object rcvd'ent' rcvd'cls' _)
                  | rcvd'cls' == (objClass $ scopeSuper world) -> contEdhSTM
                  $  do
                       recvYield'em <- readTVar rcvd'ent'
                       modifyTVar' ent $ Map.union recvYield'em
                       runEdhProg pgs $ evalExpr that doExpr $ \case
                         ctn@(_, _, EdhContinue) ->
                     -- propagate the continue to generator
                           contEdhSTM $ exit' ctn
                         (that'', scope'', EdhBreak) ->
                     -- early stop the for-from-do with nil result
                           exitEdhProc exit (that'', scope'', nil)
                         rtn@(_, _, EdhReturn _) ->
                     -- early return from for-from-do
                           exitEdhProc exit rtn
                         doResult ->
                     -- normal result from do, send to generator
                                     contEdhSTM $ do
                           iterCollector doResult
                           exit' doResult
                _ -> error "bug"

  case deParen iterExpr of
    -- calling a generator procedure
    CallExpr procExpr argsSndr -> evalExpr that procExpr $ \case

      -- a generator written in Haskell
      (that', scope', EdhHostGenr (HostProcedure _ hp)) ->
        local
            (\pgs' -> pgs'
              { edh'context =
                (edh'context pgs') { generatorCaller = Just (pgs, recvYield) }
              }
            )
          $ hp argsSndr that' scope'
          $ \result -> local (const pgs) $ exitEdhProc exit result

      -- a generator written in Edh
      (that', (Scope _ gnr'this _ _), EdhGenrDef (GenrDef gnr'lexi'stack gnrProc@(ProcDecl _ gnr'args gnr'body)))
        -> do
         -- ensure args sending and receiving happens within a same tx
         -- for atomicity of the call making
          let !calleeCtx = ctx { callStack = gnr'lexi'stack }
          local (const pgs { edh'in'tx = True })
            $ packEdhArgs that argsSndr
            $ \(_, _, pkv) -> case pkv of
                EdhArgsPack pk ->
                  recvEdhArgs calleeCtx gnr'args pk $ \(_, _, scopeObj) ->
                    case scopeObj of
                      EdhObject (Object rcvd'ent rcvd'cls _)
                        | rcvd'cls == (objClass $ scopeSuper world) -> do
                        -- use direct containing object of the method as `this` in its
                        -- procedure execution
                          local
                              (const pgs
                                -- set method's scope
                                { edh'context =
                                  ctx
                                    { callStack       =
                                      (  Scope rcvd'ent
                                               gnr'this
                                               (NE.toList gnr'lexi'stack)
                                               gnrProc
                                      <| call'stack
                                      )
                                      -- receive yield as the generator caller
                                    , generatorCaller = Just (pgs, recvYield)
                                    }
                                -- restore original tx state after args received
                                , edh'in'tx   = edh'in'tx pgs
                                }
                              )
                          -- use the resolution target object as `that` in execution of 
                          -- the method procedure
                            $ evalStmt that' gnr'body
                            $ \(that'', scope'', gnrRtn) ->
                                -- restore previous context after method returned
                                local (const pgs) $ case gnrRtn of
                                  EdhReturn rtnVal ->
                                  -- explicit return
                                    exitEdhProc exit (that'', scope'', rtnVal)
                                  EdhContinue ->
                                    throwEdh EvalError "Unexpected continue"
                                  EdhBreak ->
                                    -- allows use of `break` to early stop the generator
                                    -- procedure with nil result
                                    exitEdhProc exit (that', scope, nil)
                                  _ ->
                                  -- no explicit return, assuming it returns the last
                                  -- value from procedure execution
                                    exitEdhProc exit (that'', scope'', gnrRtn)

                      _ -> error "bug"
                _ -> error "bug"
      (_, _, val) ->
        throwEdh EvalError
          $  "Can only call a generator method from for-from-do, not "
          <> T.pack (show $ edhTypeOf val)
          <> ": "
          <> T.pack (show val)
    _ -> do
      let -- do one iteration
          do1 :: ArgsPack -> STM () -> STM ()
          do1 !pk !next =
            runEdhProg pgs
              $ recvEdhArgs ctx argsRcvr pk
              $ \(_, _, scopeObj') -> case scopeObj' of
                  EdhObject (Object rcvd'ent' rcvd'cls' _)
                    | rcvd'cls' == (objClass $ scopeSuper world) -> contEdhSTM
                    $  do
                         rcvd'em <- readTVar rcvd'ent'
                         modifyTVar' ent $ Map.union rcvd'em
                         runEdhProg pgs $ evalExpr that doExpr $ \case
                           (_, _, EdhBreak) ->
                          -- break for loop
                             exitEdhProc exit (that, scope, nil)
                          -- early return during for loop
                           rtn@(_, _, EdhReturn _) -> exitEdhProc exit rtn
                          -- continue for loop
                           doResult                -> contEdhSTM $ do
                             iterCollector doResult
                             next
                  _ -> error "bug"

          -- loop over a series of args packs
          iterThem :: [ArgsPack] -> STM ()
          iterThem []         = exitEdhSTM pgs exit (that, scope, nil)
          iterThem (pk : pks) = do1 pk $ iterThem pks

          -- loop over a subscriber's channel of an event sink
          iterEvt :: TChan EdhValue -> STM ()
          iterEvt !subChan = waitEdhSTM pgs (readTChan subChan) $ \case
            EdhArgsPack pk -> do1 pk $ iterEvt subChan
            ev             -> do1 (ArgsPack [ev] Map.empty) $ iterEvt subChan
      evalExpr that iterExpr $ \case

        -- loop from an event sink
        (_, _, EdhSink sink) ->
          contEdhSTM $ subscribeEvents sink >>= \(subChan, mrv) -> case mrv of
            Nothing -> iterEvt subChan
            Just ev ->
              let pk = case ev of
                    EdhArgsPack pk_ -> pk_
                    _               -> ArgsPack [ev] Map.empty
              in  do1 pk $ iterEvt subChan

        -- loop from a positonal-only args pack
        (_, _, EdhArgsPack (ArgsPack !args !kwargs)) | Map.null kwargs ->
          contEdhSTM $ iterThem
            [ case val of
                EdhArgsPack pk' -> pk'
                _               -> ArgsPack [val] Map.empty
            | val <- args
            ]
        -- loop from a keyword-only args pack
        (_, _, EdhArgsPack (ArgsPack !args !kwargs)) | null args ->
          contEdhSTM
            $ iterThem
                [ ArgsPack [EdhString k, v] $ Map.empty
                | (k, v) <- Map.toList kwargs
                ]

        -- loop from a tuple
        (_, _, EdhTuple vs) -> contEdhSTM $ iterThem
          [ case val of
              EdhArgsPack pk' -> pk'
              _               -> ArgsPack [val] Map.empty
          | val <- vs
          ]
        -- loop from a list
        (_, _, EdhList (List l)) -> contEdhSTM $ do
          ll <- readTVar l
          iterThem
            [ case val of
                EdhArgsPack pk' -> pk'
                _               -> ArgsPack [val] Map.empty
            | val <- ll
            ]
        -- loop from a dict
        (_, _, EdhDict (Dict d)) -> contEdhSTM $ do
          ds <- readTVar d
          iterThem -- don't be tempted to yield pairs from a dict here,
               -- it'll be messy if some entry values are themselves pairs
            [ ArgsPack [itemKeyValue k, v] Map.empty | (k, v) <- Map.toList ds ]

        (_, _, val) ->
          throwEdh EvalError
            $  "Can not do a for loop from "
            <> T.pack (show $ edhTypeOf val)
            <> ": "
            <> T.pack (show val)


mkScopeWrapper :: EdhWorld -> Scope -> STM Object
mkScopeWrapper world scope@(Scope !ent !this !lexi'stack _) = do
    -- use an object to wrap the very entity, the entity is same as of this's 
    -- in case we are in a class procedure, but not if we are in a method proc
  entWrapper <- viewAsEdhObject ent wrapperClass []
  -- a scope wrapper object is itself a blank bucket, can be used to store
  -- arbitrary attributes
  wrapperEnt <- newTVar Map.empty
  -- the wrapper object itself is a bunch of magical makeups
  wrapperObj <- viewAsEdhObject
    wrapperEnt
    wrapperClass
    [
  -- put the 'scopeSuper' object as the top super, this is where the builtin
  -- scope manipulation methods are resolved
      scopeSuper world
  -- put the object wrapping the entity (different than this's entity for
  -- a method procedure's scope) as the middle super object, so attributes
  -- not shadowed by those manually assigned ones to 'wrapperEnt', or scope
  -- manipulation methods, can be read off directly from the wrapper object
    , entWrapper
  -- put the original `this` object as the bottom super object, for
  -- information needed later, e.g. eval
    , this
    ]
  return wrapperObj
 where
-- save the scope context as 'classLexiStack' of the fake class for wrapper
  !wrapperClass =
    (objClass $ scopeSuper world) { classLexiStack = scope :| lexi'stack }


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
  let !callerCtx                          = edh'context pgs
      !callerScope@(Scope !ent !this _ _) = contextScope callerCtx
      finishAssign :: Object -> Entity -> AttrKey -> STM ()
      finishAssign tgtObj tgtEnt key = do
        modifyTVar' tgtEnt $ \em -> Map.insert key rhVal em
        -- restore program state after assignment
        runEdhProg pgsAfter $ exitEdhProc exit (tgtObj, callerScope, rhVal)
  case lhExpr of
    AttrExpr !addr -> case addr of
      DirectRef (NamedAttr "_") ->
        -- silently drop value assigned to single underscore
        contEdhSTM $ runEdhProg pgsAfter $ exitEdhProc
          exit
          (this, callerScope, nil)
      DirectRef !addr' ->
        contEdhSTM $ resolveAddr pgs addr' >>= \key -> finishAssign this ent key
      IndirectRef !tgtExpr !addr' -> case tgtExpr of
        AttrExpr ThisRef -> contEdhSTM $ resolveAddr pgs addr' >>= \key ->
          finishAssign this (objEntity this) key
        AttrExpr ThatRef -> contEdhSTM $ resolveAddr pgs addr' >>= \key ->
          finishAssign that (objEntity that) key
        _ -> evalExpr that tgtExpr $ \(!_, !_, !tgtVal) -> case tgtVal of
          EdhObject tgtObj@(Object !tgtEnt _ _) ->
            contEdhSTM $ resolveAddr pgs addr' >>= \key ->
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


recvEdhArgs
  :: Context -> ArgsReceiver -> ArgsPack -> EdhProcExit -> EdhProg (STM ())
recvEdhArgs !calleeCtx !argsRcvr pck@(ArgsPack !posArgs !kwArgs) !exit = do
  !pgsCaller <- ask
  let -- args receive always done in callee's context with tx on
    !pgsRecv = pgsCaller { edh'in'tx = True, edh'context = calleeCtx }
    !calleeScope@(Scope _ !calleeThis _ _) = contextScope calleeCtx
    !world   = contextWorld calleeCtx
    recvFromPack
      :: (ArgsPack, EntityStore) -> ArgReceiver -> STM (ArgsPack, EntityStore)
    recvFromPack (pk@(ArgsPack posArgs' kwArgs'), em) argRcvr = case argRcvr of
      RecvRestPosArgs "_" ->
        -- silently drop the value to single underscore, while consume the args
        -- from incoming pack
        return (ArgsPack [] kwArgs', em)
      RecvRestPosArgs restPosArgAttr -> return
        ( ArgsPack [] kwArgs'
        , Map.insert (AttrByName restPosArgAttr)
                     (EdhArgsPack $ ArgsPack posArgs' Map.empty)
                     em
        )
      RecvRestKwArgs "_" ->
        -- silently drop the value to single underscore, while consume the args
        -- from incoming pack
        return (ArgsPack posArgs' Map.empty, em)
      RecvRestKwArgs restKwArgAttr -> return
        ( ArgsPack posArgs' Map.empty
        , Map.insert (AttrByName restKwArgAttr)
                     (EdhArgsPack $ ArgsPack [] kwArgs')
                     em
        )
      RecvRestPkArgs "_" ->
        -- silently drop the value to single underscore, while consume the args
        -- from incoming pack
        return (ArgsPack [] Map.empty, em)
      RecvRestPkArgs restPkArgAttr -> return
        ( ArgsPack [] Map.empty
        , Map.insert (AttrByName restPkArgAttr) (EdhArgsPack pk) em
        )
      RecvArg "_" _ _ -> do
        -- silently drop the value to single underscore, while consume the arg
        -- from incoming pack
        (_, posArgs'', kwArgs'') <- resolveArgValue "_" Nothing
        return (ArgsPack posArgs'' kwArgs'', em)
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
            SymbolicAttr symName -> -- todo support this ?
              throwEdhSTM pgsRecv EvalError
                $  "Do you mean `this.@"
                <> symName
                <> "` instead ?"
          Just addr@(IndirectRef _ _) -> do
            -- do assignment in callee's context, and return to caller's afterwards
            runEdhProg pgsRecv $ assignEdhTarget
              pgsCaller
              calleeThis
              (AttrExpr addr)
              edhNop
              (calleeThis, calleeScope, argVal)
            return (ArgsPack posArgs'' kwArgs'', em)
          tgt ->
            throwEdhSTM pgsRecv EvalError
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
-- always eval the default value atomically
                runEdhProg (pgsCaller { edh'in'tx = True }) $ evalExpr
-- don't respect `that` in context, during resolution of arg default values
                  calleeThis
                  defaultExpr
                  (\(_, _, val) -> return (putTMVar defaultVar val))
                defaultVal <- readTMVar defaultVar
                return (defaultVal, posArgs', kwArgs'')
              _ ->
                throwEdhSTM pgsCaller EvalError
                  $  "Missing argument: "
                  <> argName
    woResidual :: ArgsPack -> EntityStore -> STM Entity
    woResidual (ArgsPack !posResidual !kwResidual) em
      | not (null posResidual)
      = throwEdhSTM pgsCaller EvalError
        $  "Extraneous "
        <> T.pack (show $ length posResidual)
        <> " positional argument(s)"
      | not (Map.null kwResidual)
      = throwEdhSTM pgsCaller EvalError
        $  "Extraneous keyword arguments: "
        <> T.unwords (Map.keys kwResidual)
      | otherwise
      = newTVar em
    doReturn :: Entity -> STM ()
    doReturn ent = do
      scopeObj <- viewAsEdhObject ent (objClass $ scopeSuper world) []
      -- execute outer code wrt what tx state originally is
      exitEdhSTM pgsCaller exit (calleeThis, calleeScope, EdhObject scopeObj)

  -- execution of the args receiving always in a tx for atomicity, and
  -- in callee's context
  local (const pgsRecv) $ case argsRcvr of
    PackReceiver argRcvrs -> contEdhSTM $ do
      (pck', em) <- foldM recvFromPack (pck, Map.empty) argRcvrs
      ent        <- woResidual pck' em
      doReturn ent
    SingleReceiver argRcvr -> contEdhSTM $ do
      (pck', em) <- recvFromPack (pck, Map.empty) argRcvr
      ent        <- woResidual pck' em
      doReturn ent
    WildReceiver -> return $ if null posArgs
      then do
        ent <- newTVar $ Map.mapKeys AttrByName kwArgs
        doReturn ent
      else
        throwEdhSTM pgsRecv EvalError
        $  "Unexpected "
        <> T.pack (show $ length posArgs)
        <> " positional argument(s) to wild receiver"


packEdhExprs :: Object -> [ArgSender] -> EdhProcExit -> EdhProg (STM ())
packEdhExprs that [] !exit' = do
  pgs <- ask
  let scope = contextScope $ edh'context pgs
  exit' (that, scope, EdhArgsPack $ ArgsPack [] Map.empty)
packEdhExprs that (!x : xs) !exit' = case x of
  UnpackPosArgs _ -> throwEdh EvalError "unpack to expr not supported yet"
  UnpackKwArgs _ -> throwEdh EvalError "unpack to expr not supported yet"
  UnpackPkArgs _ -> throwEdh EvalError "unpack to expr not supported yet"
  SendPosArg !argExpr -> packEdhExprs that xs $ \(that', scope, !pk) ->
    case pk of
      (EdhArgsPack (ArgsPack !posArgs !kwArgs)) ->
        exit'
          ( that'
          , scope
          , EdhArgsPack $ ArgsPack (EdhExpr argExpr : posArgs) kwArgs
          )
      _ -> error "bug"
  SendKwArg !kw !argExpr -> packEdhExprs that xs $ \(that', scope, !pk) ->
    case pk of
      (EdhArgsPack (ArgsPack !posArgs !kwArgs)) -> exit'
        ( that'
        , scope
        , EdhArgsPack $ ArgsPack posArgs $ Map.insert kw
                                                      (EdhExpr argExpr)
                                                      kwArgs
        )
      _ -> error "bug"


packEdhArgs :: Object -> ArgsSender -> EdhProcExit -> EdhProg (STM ())
-- make sure values in a pack are evaluated in same tx
packEdhArgs !that !argsSender !exit =
  local (\s -> s { edh'in'tx = True }) $ packEdhArgs' that argsSender exit

packEdhArgs' :: Object -> [ArgSender] -> EdhProcExit -> EdhProg (STM ())
packEdhArgs' !that [] !exit = do
  !pgs <- ask
  let !ctx   = edh'context pgs
      !scope = contextScope ctx
  exit (that, scope, EdhArgsPack $ ArgsPack [] Map.empty)
packEdhArgs' !that (!x : xs) !exit = do
  !pgs <- ask
  let !ctx   = edh'context pgs
      !scope = contextScope ctx
      edhVal2Kw :: EdhValue -> STM AttrName
      edhVal2Kw = \case
        EdhString s -> return s
        k ->
          throwEdhSTM pgs EvalError
            $  "Invalid argument keyword from value: "
            <> T.pack (show k)
      dictKey2Kw :: ItemKey -> STM AttrName
      dictKey2Kw = \case
        ItemByStr !name -> return name
        k ->
          throwEdhSTM pgs EvalError
            $  "Invalid argument keyword from dict key: "
            <> T.pack (show k)
  case x of
    UnpackPosArgs !posExpr -> evalExpr that posExpr $ \case
      (_, _, EdhArgsPack (ArgsPack !posArgs' _kwArgs')) ->
        packEdhArgs' that xs $ \(_, _, !pk) -> case pk of
          EdhArgsPack (ArgsPack !posArgs !kwArgs) -> exit
            (that, scope, EdhArgsPack (ArgsPack (posArgs ++ posArgs') kwArgs))
          _ -> error "bug"
      (_, _, EdhPair !k !v) -> packEdhArgs' that xs $ \(_, _, !pk) ->
        case pk of
          EdhArgsPack (ArgsPack !posArgs !kwArgs) -> exit
            (that, scope, EdhArgsPack (ArgsPack (posArgs ++ [k, v]) kwArgs))
          _ -> error "bug"
      (_, _, EdhTuple !l) -> packEdhArgs' that xs $ \(_, _, !pk) -> case pk of
        EdhArgsPack (ArgsPack !posArgs !kwArgs) ->
          exit (that, scope, EdhArgsPack (ArgsPack (posArgs ++ l) kwArgs))
        _ -> error "bug"
      (_, _, EdhList (List !l)) -> packEdhArgs' that xs $ \(_, _, !pk) ->
        case pk of
          EdhArgsPack (ArgsPack !posArgs !kwArgs) -> contEdhSTM $ do
            ll <- readTVar l
            runEdhProg pgs $ exit
              (that, scope, EdhArgsPack (ArgsPack (posArgs ++ ll) kwArgs))
          _ -> error "bug"
      (_, _, v) ->
        throwEdh EvalError $ "Can not unpack args from: " <> T.pack (show v)
    UnpackKwArgs !kwExpr -> evalExpr that kwExpr $ \case
      (_, _, EdhArgsPack (ArgsPack _posArgs' !kwArgs')) ->
        packEdhArgs' that xs $ \(_, _, !pk) -> case pk of
          EdhArgsPack (ArgsPack !posArgs !kwArgs) ->
            exit
              ( that
              , scope
              , EdhArgsPack (ArgsPack posArgs (Map.union kwArgs kwArgs'))
              )
          _ -> error "bug"
      (_, _, EdhPair !k !v) -> packEdhArgs' that xs $ \(_, _, !pk) ->
        case pk of
          EdhArgsPack (ArgsPack !posArgs !kwArgs) -> contEdhSTM $ do
            kw <- edhVal2Kw k
            runEdhProg pgs
              $ exit
                  ( that
                  , scope
                  , EdhArgsPack (ArgsPack posArgs $ Map.insert kw v kwArgs)
                  )
          _ -> error "bug"
      (_, _, EdhDict (Dict !ds)) -> packEdhArgs' that xs $ \(_, _, !pk) ->
        case pk of
          EdhArgsPack (ArgsPack !posArgs !kwArgs) -> contEdhSTM $ do
            dm  <- readTVar ds
            kvl <- forM (Map.toAscList dm) $ \(k, v) -> (, v) <$> dictKey2Kw k
            runEdhProg pgs $ exit
              ( that
              , scope
              , EdhArgsPack
                (ArgsPack posArgs $ Map.union kwArgs $ Map.fromAscList kvl)
              )
          _ -> error "bug"
      (_, _, v) ->
        throwEdh EvalError $ "Can not unpack kwargs from: " <> T.pack (show v)
    UnpackPkArgs !pkExpr -> evalExpr that pkExpr $ \case
      (_, _, EdhArgsPack (ArgsPack !posArgs' !kwArgs')) ->
        packEdhArgs' that xs $ \(_, _, !pk) -> case pk of
          EdhArgsPack (ArgsPack !posArgs !kwArgs) -> exit
            ( that
            , scope
            , EdhArgsPack
              (ArgsPack (posArgs ++ posArgs') (Map.union kwArgs kwArgs'))
            )
          _ -> error "bug"
      (_, _, v) ->
        throwEdh EvalError $ "Can not unpack pkargs from: " <> T.pack (show v)
    SendPosArg !argExpr -> evalExpr that argExpr $ \(_, _, !val) ->
      packEdhArgs' that xs $ \(_, _, !pk) -> case pk of
        EdhArgsPack (ArgsPack !posArgs !kwArgs) ->
          exit (that, scope, EdhArgsPack (ArgsPack (val : posArgs) kwArgs))
        _ -> error "bug"
    SendKwArg !kw !argExpr -> evalExpr that argExpr $ \(_, _, !val) ->
      packEdhArgs' that xs $ \(_, _, !pk) -> case pk of
        EdhArgsPack (ArgsPack !posArgs !kwArgs) -> case kw of
          "_" ->
            -- silently drop the value to keyword of single underscore
            exit (that, scope, pk)
          _ -> exit
            ( that
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

