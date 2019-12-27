
module Language.Edh.Batteries.Reflect where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import           Data.List.NonEmpty             ( NonEmpty(..) )
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
  let
    (      Context !world !call'stack _) = edh'context pgs
    scope@(Scope ent this lex'stack _  ) = NE.head call'stack
  -- save the lexical context in the fake class of the wrapper object
    !wrapperClass =
      (objClass $ scopeSuper world) { classLexiStack = lex'stack }
  contEdhSTM $ do
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
    exitEdhSTM pgs exit (that, scope, EdhObject wrapperObj)


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
      _ -> error "bug "
 where
  itemKeyOf :: AttrKey -> ItemKey
  itemKeyOf (AttrByName name) = ItemByStr name
  itemKeyOf (AttrBySym  sym ) = ItemBySym sym


-- | utility scope.stack()
-- get lexical context from the wrapped scope
scopeStackProc :: EdhProcedure
scopeStackProc _ !that _ !exit = do
  !pgs <- ask
  let callerCtx@(Context !world _ _) = edh'context pgs
  contEdhSTM $ do
    wrappedObjs <-
      sequence $ (<$> (classLexiStack $ objClass that)) $ \(Scope _ obj _ _) ->
        do
          wrapperEnt <- newTVar Map.empty
          -- TODO this is not right !
          viewAsEdhObject wrapperEnt
                          (objClass $ scopeSuper world)
                          [scopeSuper world, obj]
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
    callerCtx@(  Context !world _ _) = edh'context pgs
    callerScope@(Scope !ent _ _ _  ) = contextScope $ callerCtx
    !root                            = worldRoot world
    !rootScope =
      Scope (objEntity root) root [] (classProcedure $ objClass root)
    evalThePack
      :: [EdhValue]
      -> Map.Map AttrName EdhValue
      -> [EdhValue]
      -> Map.Map AttrName EdhValue
      -> EdhProg (STM ())
    evalThePack argsValues kwargsValues [] kwargsExprs | Map.null kwargsExprs =
      -- restore original program state and return the eval-ed values
      local (const pgs) $ exitEdhProc
        exit
        ( that
        , callerScope
        , EdhArgsPack $ ArgsPack (reverse argsValues) kwargsValues
        )
    evalThePack argsValues kwargsValues [] kwargsExprs = do
      let (!oneExpr, !kwargsExprs') = Map.splitAt 1 kwargsExprs
          (!kw     , !kwExpr      ) = Map.elemAt 0 oneExpr
      case kwExpr of
        EdhExpr expr -> evalExpr that expr $ \(_, _, val) -> evalThePack
          argsValues
          (Map.insert kw val kwargsValues)
          []
          kwargsExprs'
        v -> throwEdh EvalError $ "Not an expr: " <> T.pack (show v)
    evalThePack argsValues kwargsValues (argExpr : argsExprs') kwargsExprs =
      case argExpr of
        EdhExpr expr -> evalExpr that expr $ \(_, _, val) ->
          evalThePack (val : argsValues) kwargsValues argsExprs' kwargsExprs
        v -> throwEdh EvalError $ "Not an expr: " <> T.pack (show v)
  packEdhArgs that argsSender
    $ \(_, _, EdhArgsPack (ArgsPack !args !kwargs)) ->
        if null kwargs && null args
          then exitEdhProc
            exit
            (that, callerScope, EdhArgsPack $ ArgsPack [] Map.empty)
          else contEdhSTM $ do
            supers <- readTVar $ objSupers that
            case supers of
              [_, this] ->
                -- eval all exprs with the original scope as the only scope
                -- atop the world
                runEdhProg pgs
                    { edh'context =
                      Context
                        { contextWorld = world
                        , callStack    =
                          Scope ent
                                this
                                (classLexiStack $ objClass this)
                                (classProcedure $ objClass $ scopeSuper world)
                            :| [rootScope]
                        , contextStmt  = voidStatement
                        }
                    }
                  $ evalThePack [] Map.empty args kwargs
              _ -> error "<scope> bug - supers wrong"


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

