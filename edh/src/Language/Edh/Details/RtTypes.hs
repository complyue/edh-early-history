
module Language.Edh.Details.RtTypes where

import           Prelude

import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Foldable
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE

import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           System.Mem.Weak
import           System.IO.Unsafe

import           Text.Megaparsec

import           Data.Lossless.Decimal         as D

import           Language.Edh.Control
import           Language.Edh.AST


-- | A dict in Edh is neither an object nor an entity, but just a
-- mutable associative map.
--
-- A dict has items associated by 'ItemKey'.
newtype Dict = Dict (TVar DictStore)
  deriving (Eq)
type DictStore = Map.Map ItemKey EdhValue
instance Show Dict where
  show (Dict d) = if Map.null dm
    then "{,}" -- make it obvious this is an empty dict
    else -- advocate trailing comma here
      "{ "
      ++ concat [ show k ++ ":" ++ show v ++ ", " | (k, v) <- Map.toList dm ]
      ++ "}"
    where dm = unsafePerformIO $ readTVarIO d
data ItemKey = ItemByType !EdhTypeValue
    | ItemByStr !Text | ItemBySym !Symbol
    | ItemByNum !Decimal | ItemByBool !Bool
  deriving (Eq, Ord)
instance Show ItemKey where
  show (ItemByType k) = show k
  show (ItemByStr  k) = show k
  show (ItemBySym  k) = show k
  show (ItemByNum  k) = showDecimal k
  show (ItemByBool k) = show $ EdhBool k


-- | An entity in Edh is the backing storage for a scope, with
-- possibly an object viewing this entity.
--
-- An entity has attributes associated by 'AttrKey'.
type Entity = TVar EntityStore
type EntityStore = Map.Map AttrKey EdhValue
data AttrKey = AttrByName !AttrName | AttrBySym !Symbol
    deriving (Eq, Ord, Show)

-- | A symbol can stand in place of an alphanumeric name, used to
-- address an attribute from an entity object, but symbols are 
-- private to its owning object, can not be read-out from out side
-- of the object, thus serves encapsulation purpose in object
-- oriented type designs.
--
-- And symbol values can be addressed from lexical outer entities,
-- e.g. a symbol bound to a module is available to all procedures 
-- defined in the module, and a symbol bound within a class procedure
-- is available to all its methods as well as nested classes.
--
-- Note: we rely on the 'Ord' instance of 'CString' field (which is
--       essentially a ptr), for trivial implementation of 'Symbol'
--       's 'Ord' instance, so it can be used as attribut key on an
--       entity (the underlying 'Map.Map' storage per se).
newtype Symbol = Symbol {
    -- TODO remember to default the description to the source pos
    symbol'description :: CString
  } deriving (Eq, Ord)
instance Show Symbol where
  show (Symbol dp) = "[@" <> sd <> "]"
    where sd = unsafePerformIO $ peekCString dp
mkSymbol :: String -> IO Symbol
mkSymbol !d = do
  !s <- newCString d
  let !sym = Symbol s
  addFinalizer sym $ free s
  return sym


-- | A list in Edh is a multable, singly-linked, prepend list.
newtype List = List (TVar [EdhValue])
  deriving (Eq)
instance Show List where
  show (List l) = if null ll
    then "[]"
    else "[ " ++ concat [ show i ++ ", " | i <- ll ] ++ "]"
    where ll = unsafePerformIO $ readTVarIO l


-- | The execution context of an Edh thread
data Context = Context {
    -- | the Edh world in context
    contextWorld :: !EdhWorld
    -- | the call stack frames of Edh procedures
    , callStack :: !(NonEmpty Scope)
    -- | `that` object in context, the object triggering current
    -- procedure call, it's the final descendent object when a
    -- super method or constructor is in execution.
    , contextStmt :: !StmtSrc
  }
instance Eq Context where
  Context x'world x'stack x'ss == Context y'world y'stack y'ss =
    x'world == y'world && x'stack == y'stack && x'ss == y'ss
contextScope :: Context -> Scope
contextScope = NE.head . callStack


-- Especially note that Edh has no block scope as in C
-- family languages, JavaScript neither does before ES6,
-- Python neither does until now (2019).
--
-- There is only `procedure scope` in Edh, and there are only 2 types
-- of procedures:
--  * class (constructor) procedure
--  * method procedure
-- Note that module scope is a special kind of class procedure, think
-- it as the module object's constructor procedure; and generator
-- procedure is a special kind of method procedure.
--
-- Every procedure call will create a new scope, with a new entity created
-- for it, and:
--
--  * if it is a class procedure call, a new object of this class is also
--    allocated viewing the entity, serving `this` object of the scope;
--
--  * if it is a methd procedure call, no new object is created, and the
--    scope inherits `this` object from the outer scope.
--
data Scope = Scope {
    -- | the entity of current scope, it's unique in a method procedure,
    -- and is the underlying entity of 'thisObject' in a class procedure.
    scopeEntity :: !Entity
    -- | `this` object of current scope
    , thisObject :: !Object
    -- | the lexical context in which the executing procedure is defined
    , lexiStack :: ![Scope]
    -- | the Edh procedure holding this scope
    , scopeProc :: !ProcDecl
  }
instance Eq Scope where
  Scope x'e _ _ x'p == Scope y'e _ _ y'p = x'e == y'e && x'p == y'p
instance Show Scope where
  show (Scope _ _ _ (ProcDecl pName argsRcvr (StmtSrc (!srcPos, _)))) =
    "["
      ++ T.unpack pName
      ++ show argsRcvr
      ++ " @ "
      ++ sourcePosPretty srcPos
      ++ "]"


-- | An object views an entity, with inheritance relationship 
-- to any number of super objects.
data Object = Object {
    -- | the entity stores attribute set of the object
    objEntity :: !Entity
    -- | the class (a.k.a constructor) procedure of the object
    , objClass :: !Class
    -- | up-links for object inheritance hierarchy
    , objSupers :: !(TVar [Object])
  }
instance Eq Object where
  -- equality by pointer to entity
  Object x'e _ _ == Object y'e _ _ = x'e == y'e
instance Show Object where
  -- it's not right to call 'atomically' here to read 'objSupers' for
  -- the show, as 'show' may be called from an stm transaction, stm
  -- will fail hard on encountering of nested 'atomically' calls.
  show (Object _ (Class _ (ProcDecl cn _ _)) _) =
    "[object: " ++ T.unpack cn ++ "]"

-- | View an entity as object of specified class with specified ancestors
-- this is the black magic you want to avoid
viewAsEdhObject :: Entity -> Class -> [Object] -> STM Object
viewAsEdhObject ent cls supers = Object ent cls <$> newTVar supers

data Class = Class {
    -- | the lexical context where this class procedure is defined
    classLexiStack :: !(NonEmpty Scope)
    , classProcedure :: !ProcDecl
  }
instance Eq Class where
  Class x's x'pd == Class y's y'pd = x's == y's && x'pd == y'pd
instance Show Class where
  show (Class _ (ProcDecl cn _ _)) = "[class: " ++ T.unpack cn ++ "]"

data Method = Method {
    methodLexiStack :: !(NonEmpty Scope)
    , methodProcedure :: !ProcDecl
  } deriving (Eq)
instance Show Method where
  show (Method _ (ProcDecl mn _ _)) = "[method: " ++ T.unpack mn ++ "]"

data Operator = Operator {
    operatorLexiStack :: !(NonEmpty Scope)
    , operatorProcedure :: !ProcDecl
    -- the overridden operator procedure
    , operatorPredecessor :: !(Maybe EdhValue)
    -- todo this is some redundant, as the precedences are always available
    -- from 'worldOperators', but being an 'MVar' that's non-trivial to read
    -- safely from a pure 'show' function. can remove this field once we
    -- switched to a better introspection tool for operators at runtime.
    , operatorPrecedence :: !Precedence
  } deriving (Eq)
instance Show Operator where
  show (Operator _ (ProcDecl opSym _ _) _ prec) =
    "[operator: (" ++ T.unpack opSym ++ ") " ++ show prec ++ " ]"

data GenrDef = GenrDef {
    generatorLexiStack :: !(NonEmpty Scope)
    , generatorProcedure :: !ProcDecl
  } deriving (Eq)
instance Show GenrDef where
  show (GenrDef _ (ProcDecl mn _ _)) = "[generator: " ++ T.unpack mn ++ "]"


newtype GenrIter = GenrIter Context
  deriving (Eq)
instance Show GenrIter where
  show _ = "[iterator]"


-- | Pending evaluated statement, it can be later forced against a
-- target value in context.
--
-- Note: we rely on the 'CString' field (which is essentially a ptr),
--       for equality testing of thunks.
data Thunk = Thunk {
    thunkDescription :: !CString
    , thunkValuator :: !(EdhValue -> IO EdhValue)
  }
instance Eq Thunk where
  Thunk x'd _ == Thunk y'd _ = x'd == y'd
instance Show Thunk where
  show (Thunk d _) = "[thunk: " <> s <> "]"
    where s = unsafePerformIO $ peekCString d
mkThunk :: String -> (EdhValue -> IO EdhValue) -> IO Thunk
mkThunk !desc !valuator = do
  !s <- newCString desc
  let !thk = Thunk s valuator
  addFinalizer thk $ free s
  return thk


-- | A world for Edh programs to change
data EdhWorld = EdhWorld {
    -- | root object of this world
    worldRoot :: !Object
    -- | all module objects in this world belong to this class
    , moduleClass :: !Class
    -- | all scope wrapper objects in this world belong to the same
    -- class as 'scopeSuper' and have it as the top most super,
    -- the bottom super of a scope wraper object is the original
    -- `this` object of that scope, thus an attr addressor can be
    -- used to read the attribute value out of the wrapped scope, when
    -- the attr name does not conflict with scope wrapper methods
    , scopeSuper :: !Object
    -- | all operators declared in this world, this also used as the
    -- _world lock_ in parsing source code to be executed in this world
    , worldOperators :: !(TMVar OpPrecDict)
    -- | all modules loaded into this world
    , worldModules :: !(TVar (Map.Map ModuleId Object))
  }
instance Eq EdhWorld where
  EdhWorld x'root _ _ _ _ == EdhWorld y'root _ _ _ _ = x'root == y'root


-- | The monad for running of an Edh program
type EdhProg = ReaderT EdhProgState STM

-- | The states of a program
data EdhProgState = EdhProgState {
    edh'master'thread :: !ThreadId
    , edh'main'queue :: !(TQueue EdhTxTask)
    , edh'context :: !Context
    , edh'in'tx :: !Bool
    -- , edh'forker'thread :: !ThreadId
    -- , edh'fork'chan :: !(TChan xxx)
  }

-- | Run an Edh program from within STM monad
runEdhProg :: EdhProgState -> EdhProg (STM ()) -> STM ()
runEdhProg pgs prog = join $ runReaderT prog pgs
{-# INLINE runEdhProg #-}

-- | Continue an Edh program with stm computation, there must be NO further
-- action following this statement, or the stm computation is just lost.
--
-- Note: this is just `return`, but procedures writen in the host language
-- (i.e. Haskell) with this instead of `return` will be more readable.
contEdhSTM :: STM () -> EdhProg (STM ())
contEdhSTM = return
{-# INLINE contEdhSTM #-}

-- | Exit an stm computation to the specified Edh continuation
exitEdhSTM :: EdhProgState -> EdhProcExit -> (Object, Scope, EdhValue) -> STM ()
exitEdhSTM pgs exit result = runEdhProg pgs $ exitEdhProc exit result
{-# INLINE exitEdhSTM #-}

-- | Convenient function to be used as short-hand to return from an Edh
-- procedure (or functions with similar signature), this sets transaction
-- boundaries wrt tx stated in the program's current state.
exitEdhProc :: EdhProcExit -> (Object, Scope, EdhValue) -> EdhProg (STM ())
exitEdhProc exit result = do
  pgs <- ask
  let txq   = edh'main'queue pgs
      !inTx = edh'in'tx pgs
  return $ if inTx
    then join $ runReaderT (exit result) pgs
    else writeTQueue txq ((pgs, result), exit)
{-# INLINE exitEdhProc #-}

-- | An atomic task, an Edh program is composed form many this kind of tasks.
type EdhTxTask
  = ( (EdhProgState, (Object, Scope, EdhValue))
    , (Object, Scope, EdhValue) -> EdhProg (STM ())
    )

-- | Type of a procedure in host language that can be called from Edh code.
--
-- Note the caller context/scope can be obtained from the program state.
type EdhProcedure -- such a procedure servs as the callee
  =  ArgsSender -- ^ the manifestation of how the caller wills to send args
  -> Object -- ^ the target object, i.e. `that` object in context
  -> Scope -- ^ the scope from which the callee is addressed off
  -> EdhProcExit -- ^ the CPS exit to return a value from this procedure
  -> EdhProg (STM ())

-- | The type for an Edh procedure's return, in continuation passing style.
type EdhProcExit = (Object, Scope, EdhValue) -> EdhProg (STM ())

-- | Construct an error context from program state and specified message
getEdhErrorContext :: EdhProgState -> Text -> EdhErrorContext
getEdhErrorContext !pgs !msg =
  let (Context _ !stack (StmtSrc (!sp, _))) = edh'context pgs
      !frames                               = foldl'
        (\sfs (Scope _ _ _ (ProcDecl procName _ (StmtSrc (spos, _)))) ->
          (procName, T.pack (sourcePosPretty spos)) : sfs
        )
        []
        ( takeWhile (\(Scope _ _ lexi'stack _) -> not (null lexi'stack))
        $ NE.toList stack
        )
  in  EdhErrorContext msg (T.pack $ sourcePosPretty sp) frames


-- | A pack of evaluated argument values with positional/keyword origin,
-- normally obtained by invoking `packEdhArgs ctx argsSender`.
data ArgsPack = ArgsPack {
    positional'args :: ![EdhValue]
    , keyword'args :: !(Map.Map AttrName EdhValue)
  } deriving (Eq)


-- | Type of procedures implemented in the host language (Haskell).
--
-- Note: we rely on the 'CString' field (which is essentially a ptr),
--       for equality testing of host procedures.
data HostProcedure = HostProcedure {
    hostProc'name :: !CString
    , hostProc'proc :: !EdhProcedure
  }
instance Eq HostProcedure where
  HostProcedure x'n _ == HostProcedure y'n _ = x'n == y'n
instance Show HostProcedure where
  show (HostProcedure pn _) = "[hostproc: " ++ nm ++ "]"
    where nm = unsafePerformIO $ peekCString pn


-- | An event sink is similar to a Go channel, but is broadcast
-- in nature, in contrast to the Go channel's unicast nature.
data EventSink = EventSink {
        evs'mrv :: !(TVar EdhValue) -- most recent value, initially nil
        , evs'chan :: !(TChan EdhValue) -- 
    } deriving Eq
instance Show EventSink where
  show (EventSink e _) =
    "[sink: " ++ show (unsafePerformIO (readTVarIO e)) ++ "]"


-- Atop Haskell, most types in Edh the surface language, are for
-- immutable values, besides dict and list, the only other mutable
-- data structure in Edh, is the entity, an **entity** is a set of
-- mutable attributes.
--
-- After applied a set of rules/constraints about how attributes
-- of an entity can be retrived and altered, it becomes an object.
--
-- Theoretically an entity is not necessarily mandated to have an
-- `identity` attribute among others, while practically the memory
-- address for physical storage of the attribute set, naturally
-- serves an `identity` attribute in single-process + single-run
-- scenario. Distributed programs, especially using a separate
-- database for storage, will tend to define a generated UUID 
-- attribute or the like.

-- | the type for a value
data EdhValue = EdhType !EdhTypeValue -- ^ type itself is a kind of value
  -- * immutable values
    | EdhNil
    | EdhDecimal !Decimal
    | EdhBool !Bool
    | EdhString !Text
    | EdhSymbol !Symbol

  -- * direct pointer (to entities) values
    | EdhObject !Object

  -- * mutable containers
    | EdhDict !Dict
    | EdhList !List

  -- * immutable containers
  --   the elements may still pointer to mutable data
    | EdhPair !EdhValue !EdhValue
    | EdhTuple ![EdhValue]
    | EdhArgsPack ArgsPack

  -- * pending evaluated code block
    | EdhBlock ![StmtSrc]

  -- * harness for lazy evaluation
    | EdhThunk !Thunk

  -- * host procedure callable from Edh world
    | EdhHostProc !HostProcedure
    | EdhHostOper !Precedence !HostProcedure

  -- * precedure definitions
    | EdhClass !Class
    | EdhMethod !Method
    | EdhOperator !Operator
    | EdhGenrDef !GenrDef

  -- * flow control
    | EdhBreak | EdhContinue
    | EdhCaseClose !EdhValue | EdhFallthrough
    | EdhGenrIter !GenrIter
    | EdhYield !EdhValue
    | EdhReturn !EdhValue

  -- * event sink
    | EdhSink !EventSink

  -- * reflection
    | EdhExpr !Expr

edhValueStr :: EdhValue -> Text
edhValueStr (EdhString s) = s
edhValueStr v             = T.pack $ show v

instance Show EdhValue where
  show (EdhType t)    = show t
  show EdhNil         = "nil"
  show (EdhDecimal v) = showDecimal v
  show (EdhBool    v) = if v then "true" else "false"
  show (EdhString  v) = show v
  show (EdhSymbol  v) = show v

  show (EdhObject  v) = show v

  show (EdhDict    v) = show v
  show (EdhList    v) = show v

  show (EdhPair k v ) = show k <> ":" <> show v
  show (EdhTuple v  ) = if null v
    then "(,)" -- mimic the denotation of empty tuple in Python
    else -- advocate trailing comma here
         "( " ++ concat [ show i ++ ", " | i <- v ] ++ ")"
  show (EdhArgsPack (ArgsPack posArgs kwArgs)) =
    if null posArgs && Map.null kwArgs
      then "pack()"
      else
        "pack( "
        ++ concat [ show i ++ ", " | i <- posArgs ]
        ++ concat
             [ T.unpack kw ++ "=" ++ show v ++ ", "
             | (kw, v) <- Map.toList kwArgs
             ]
        ++ ")"

  show (EdhBlock v) = if null v
    then "{;}" -- make it obvious this is an empty block
    else "{ " ++ concat [ show i ++ "; " | i <- v ] ++ "}"

  show (EdhThunk    v) = show v

  show (EdhHostProc v) = show v
  show (EdhHostOper prec (HostProcedure pn _)) =
    "[hostop: (" ++ nm ++ ") " ++ show prec ++ " ]"
    where nm = unsafePerformIO $ peekCString pn


  show (EdhClass    v)  = show v
  show (EdhMethod   v)  = show v
  show (EdhOperator v)  = show v
  show (EdhGenrDef  v)  = show v

  show EdhBreak         = "[break]"
  show EdhContinue      = "[continue]"
  show (EdhCaseClose v) = "[caseclose: " ++ show v ++ "]"
  show EdhFallthrough   = "[fallthrough]"
  show (EdhGenrIter i)  = show i
  show (EdhYield    v)  = "[yield: " ++ show v ++ "]"
  show (EdhReturn   v)  = "[return: " ++ show v ++ "]"

  show (EdhSink     v)  = show v

  show (EdhExpr     v)  = "[expr: " ++ show v ++ "]"

-- Note:
--
-- here is identity-wise equality i.e. pointer equality if mutable,
-- or value equality if immutable.
--
-- the semantics are different from value-wise equality especially
-- for types of:  object/dict/list

instance Eq EdhValue where
  EdhType x            == EdhType y            = x == y
  EdhNil               == EdhNil               = True
  EdhDecimal x         == EdhDecimal y         = x == y
  EdhBool    x         == EdhBool    y         = x == y
  EdhString  x         == EdhString  y         = x == y
  EdhSymbol  x         == EdhSymbol  y         = x == y

  EdhObject  x         == EdhObject  y         = x == y

  EdhDict    x         == EdhDict    y         = x == y
  EdhList    x         == EdhList    y         = x == y
  EdhPair x'k x'v      == EdhPair y'k y'v      = x'k == y'k && x'v == y'v
  EdhTuple    x        == EdhTuple    y        = x == y
  EdhArgsPack x        == EdhArgsPack y        = x == y

  EdhBlock    x        == EdhBlock    y        = x == y

  EdhThunk    x        == EdhThunk    y        = x == y

  EdhHostProc x        == EdhHostProc y        = x == y
  EdhHostOper _ x'proc == EdhHostOper _ y'proc = x'proc == y'proc

  EdhClass    x        == EdhClass    y        = x == y
  EdhMethod   x        == EdhMethod   y        = x == y
  EdhOperator x        == EdhOperator y        = x == y
  EdhGenrDef  x        == EdhGenrDef  y        = x == y

  EdhBreak             == EdhBreak             = True
  EdhContinue          == EdhContinue          = True
  EdhCaseClose x       == EdhCaseClose y       = x == y
  EdhFallthrough       == EdhFallthrough       = True
  EdhGenrIter x        == EdhGenrIter y        = x == y
-- todo: regard a yielded/returned value equal to the value itself ?
  EdhYield    x'v      == EdhYield    y'v      = x'v == y'v
  EdhReturn   x'v      == EdhReturn   y'v      = x'v == y'v

  EdhSink     x        == EdhSink     y        = x == y

  EdhExpr     x'v      == EdhExpr     y'v      = x'v == y'v

-- todo: support coercing equality ?
--       * without this, we are a strongly typed dynamic language
--       * with this, we'll be a weakly typed dynamic language
  _                    == _                    = False


nil :: EdhValue
nil = EdhNil

nan :: EdhValue
nan = EdhDecimal D.nan

inf :: EdhValue
inf = EdhDecimal D.inf

true :: EdhValue
true = EdhBool True

false :: EdhValue
false = EdhBool False


edhTypeOf :: EdhValue -> EdhValue

edhTypeOf EdhNil            = nil
edhTypeOf (EdhDecimal _   ) = EdhType DecimalType
edhTypeOf (EdhBool    _   ) = EdhType BoolType
edhTypeOf (EdhString  _   ) = EdhType StringType
edhTypeOf (EdhSymbol  _   ) = EdhType SymbolType
edhTypeOf (EdhObject  _   ) = EdhType ObjectType
edhTypeOf (EdhDict    _   ) = EdhType DictType
edhTypeOf (EdhList    _   ) = EdhType ListType
edhTypeOf (EdhPair _ _    ) = EdhType PairType
edhTypeOf (EdhTuple    _  ) = EdhType TupleType
edhTypeOf (EdhArgsPack _  ) = EdhType ArgsPackType
edhTypeOf (EdhBlock    _  ) = EdhType BlockType
edhTypeOf (EdhThunk    _  ) = EdhType ThunkType
edhTypeOf (EdhHostProc _  ) = EdhType HostProcType
edhTypeOf (EdhHostOper _ _) = EdhType HostOperType
edhTypeOf (EdhClass    _  ) = EdhType ClassType
edhTypeOf (EdhMethod   _  ) = EdhType MethodType
edhTypeOf (EdhOperator _  ) = EdhType OperatorType
edhTypeOf (EdhGenrDef  _  ) = EdhType GeneratorType

edhTypeOf EdhBreak          = EdhType FlowCtrlType
edhTypeOf EdhContinue       = EdhType FlowCtrlType
edhTypeOf (EdhCaseClose _)  = EdhType FlowCtrlType
edhTypeOf EdhFallthrough    = EdhType FlowCtrlType
edhTypeOf (EdhGenrIter _)   = EdhType FlowCtrlType
edhTypeOf (EdhYield    _)   = EdhType FlowCtrlType
edhTypeOf (EdhReturn   _)   = EdhType FlowCtrlType

edhTypeOf (EdhSink     _)   = EdhType SinkType
edhTypeOf (EdhExpr     _)   = EdhType ExprType

edhTypeOf (EdhType     _)   = EdhType TypeType

