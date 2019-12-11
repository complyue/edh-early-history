
module Language.Edh.Runtime where

import           Prelude

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Data.Lossless.Decimal         as D

import           Language.Edh.Control
import           Language.Edh.AST


-- | A dict in Edh is not an object, a dict has items associated
-- by 'ItemKey'.
newtype Dict = Dict (Map.Map ItemKey EdhValue)
    deriving (Eq)
instance Show Dict where
    -- advocate trailing comma here
    show (Dict d) =
        "{"
            ++ Prelude.concat
                   [ show k ++ ":" ++ show v ++ ", " | (k, v) <- Map.toList d ]
            ++ "}"
data ItemKey = ItemByStr Text | ItemBySym Symbol
            | ItemByNum Decimal | ItemByBool Bool
    deriving (Eq, Ord)
instance Show ItemKey where
    show (ItemByStr  k) = show k
    show (ItemBySym  k) = show k
    show (ItemByNum  k) = showDecimal k
    show (ItemByBool k) = show $ EdhBool k


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
newtype Symbol = Symbol {
    -- use CString to obtain a stable ptr as identity
        symbol'description :: CString
    } deriving (Eq, Ord)
instance Show Symbol where
    show (Symbol dp) = "[@" <> sd <> "]"
        where sd = unsafePerformIO $ peekCString dp


-- | An entity in Edh is the backing storage for both a scope
-- and an object, an entity has attributes associated by 'AttrKey'.
type Entity = IORef (Map.Map AttrKey EdhValue)
data AttrKey = AttrByName AttrName | AttrBySym Symbol
    deriving (Eq, Ord, Show)


-- | There are only 2 types of scope in Edh: `module scope` and
-- `procedure scope`, and there are only 2 types of procedures:
-- `class procedure` and `method procedure` (where
-- `generator procedure`) is a special type of `method procedure`.
--
-- for a class procedure call, 'thisObject' views 'scopeEntity',
-- which is the case of object construction; and for a method 
-- procedure call, 'thisObject' views the direct or indirect
-- 'scopeOuterScope' which is the class procedure's scope used
-- to construct that object.
data Scope = Scope {
        scopeEntity :: !Entity -- current procedure scope
        , scopeOuterScope :: !Scope -- lexical outer scope
        , thisObject :: !Object -- `this` object in scope
    }
instance Eq Scope where
    Scope x'e _ _ == Scope y'e _ _ = x'e == y'e


-- | An object views an entity, with inheritance relationship 
-- to any number of super objects. Objects reference eachothers
-- to form a networked data structure.
data Object = Object {
        -- | pointer to the stored attribute set of an entity
        objEntity :: !Entity

        -- | the class (a.k.a constructor procedure) of an object
        --
        -- similar to obj.__class__ in Python, but does not serve
        -- method resolution as in Python.
        --
        -- similar to obj.(__proto__.)constructor in JavaScript, 
        -- can be used to fake up an object, not going through
        -- the typical construction procedure.
        --
        -- TODO not sure whether referential transparency (GHC)
        -- works to reduce ram overhead of this field to cost a
        -- single pointer, and if that's not the case, may worth
        -- doing an optimisation, as every object created induce
        -- this overhead and there'll be many objects to create
        -- for the run.
        , objClass :: !Class

        -- | up-links for object inheritance hierarchy
        --
        -- similar to obj.__class__.__mro__ in Python, but resides
        -- per object in Edh, in contrast to per class in Python.
        --
        -- this means the inheritance topology is variable compared
        -- to other OO languages.
        , objSupers :: ![Object]

        -- so there's black magic to use different class/supers to
        -- view the same entity as another type of object, fooling
        -- the rest of the world. JavaScript folks have been able
        -- to do this for decades.
    }
instance Eq Object where
    -- equality by pointer to entity
    Object x'e _ _ == Object y'e _ _ = x'e == y'e
instance Show Object where
    show (Object _ (Class _ cn _ _) supers) =
        "[object: "
            ++ T.unpack cn
            ++ (if Prelude.null supers
                   then ""
                   else
                       T.unpack
                       $  " extends"
                       <> T.concat
                              [ " " <> scn
                              | (Object _ (Class _ scn _ _) _) <- supers
                              ]
               )
            ++ "]"

data Class = Class {
        classOuterScope :: !Scope
        , className :: !AttrName
        , classSourcePos :: !SourcePos
        , classProcedure :: !ProcDecl
    }
instance Eq Class where
    Class x's _ x'sp _ == Class y's _ y'sp _ = x's == y's && x'sp == y'sp
instance Show Class where
    show (Class _ cn _ _) = "[class: " ++ T.unpack cn ++ "]"

data Method = Method {
        methodOwnerObject :: !Object
        , methodName :: !AttrName
        , methodSourcePos :: !SourcePos
        , methodProcedure :: !ProcDecl
    }
instance Eq Method where
    Method x'o _ x'sp _ == Method y'o _ y'sp _ = x'o == y'o && x'sp == y'sp
instance Show Method where
    show (Method (Object _ (Class _ cn _ _) _) mn _ _) =
        "[method: " ++ T.unpack cn ++ "#" ++ T.unpack mn ++ "]"

data GenrDef = GenrDef {
        generatorOwnerObject :: !Object
        , generatorSourcePos :: !SourcePos
        , generatorProcedure :: !ProcDecl
    }
instance Eq GenrDef where
    GenrDef x'o x'sp _ == GenrDef y'o y'sp _ = x'o == y'o && x'sp == y'sp
instance Show GenrDef where
    show (GenrDef _ _ _) = "[generator]"

data Module = Module {
        moduleObject :: !Object
        , moduleId :: !ModuleId
    }
instance Eq Module where
    Module x'o _ == Module y'o _ = x'o == y'o
instance Show Module where
    show (Module _ mp) = "[module: " ++ mp ++ "]"

data Iterator = Iterator {
        iterScope :: !Scope
        , iterRestStmts :: ![StmtSrc]
    }
instance Eq Iterator where
    -- TODO prove there won't be concurrent executable iterators
    -- against the same entity, or tackle problems encountered
    Iterator x'e _ == Iterator y'e _ = x'e == y'e
instance Show Iterator where
    show (Iterator _ _) = "[iterator]"


data EdhWorld = EdhWorld {
        worldRoot :: !Entity
    -- all module objects in this world belong to this class
        , moduleClass :: Class
        , worldOperators :: !(IORef OpPrecDict)
        , worldModules :: !(IORef (Map.Map ModuleId Module))
    }


-- Atop Haskell, most types in Edh, as to organize information,
-- are immutable values, the only mutable data structure in Edh,
-- is the entity, an **entity** is a set of mutable attributes.
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

data EdhValue =
    -- * immutable values
          EdhNil
        | EdhDecimal !Decimal
        | EdhBool !Bool
        | EdhString !Text
        | EdhSymbol !Symbol

    -- * direct pointer (to entities) values
        | EdhObject !Object
        | EdhModule !Module

    -- * immutable by self, but may contain pointer to entities
        | EdhDict !Dict
        | EdhList ![EdhValue]
        | EdhTuple ![EdhValue]
        | EdhGroup ![EdhValue]

    -- * immutable by self, but have access to lexical entities
        | EdhClass !Class
        | EdhMethod !Method
        | EdhGenrDef !GenrDef

    -- * special harness
        | EdhIterator !Iterator
        | EdhYield !EdhValue
        | EdhReturn !EdhValue

    -- * reflection
        | EdhProxy !EdhValue


instance Show EdhValue where
    show EdhNil         = "nil"
    show (EdhDecimal v) = showDecimal v
    show (EdhBool    v) = if v then "true" else "false"
    show (EdhString  v) = show v
    show (EdhSymbol  v) = show v

    show (EdhObject  v) = show v
    show (EdhModule  v) = show v

-- advocate trailing comma here
    show (EdhDict    v) = show v
    show (EdhList    v) = if Prelude.null v
        then "[]"
        else "[" ++ Prelude.concat [ show i ++ ", " | i <- v ] ++ "]"
    show (EdhTuple v) = if Prelude.null v
        then "(,)" -- the denotation of empty tuple is same as Python
        else "(" ++ Prelude.concat [ show i ++ ", " | i <- v ] ++ ")"
    show (EdhGroup v) = if Prelude.null v
        then "(;)" -- make it obvious this is an empty group
        else "(" ++ Prelude.concat [ show i ++ "; " | i <- v ] ++ ")"

    show (EdhClass    v) = show v
    show (EdhMethod   v) = show v
    show (EdhGenrDef  v) = show v

    show (EdhIterator i) = show i
    show (EdhYield    v) = "[yield: " ++ show v ++ "]"
    show (EdhReturn   v) = "[return: " ++ show v ++ "]"

    show (EdhProxy    v) = "[proxy: " ++ show v ++ "]"

instance Eq EdhValue where
    EdhNil          == EdhNil          = True
    EdhDecimal  x   == EdhDecimal  y   = x == y
    EdhBool     x   == EdhBool     y   = x == y
    EdhString   x   == EdhString   y   = x == y
    EdhSymbol   x   == EdhSymbol   y   = x == y

    EdhObject   x   == EdhObject   y   = x == y
    EdhModule   x   == EdhModule   y   = x == y

    EdhDict     x   == EdhDict     y   = x == y
    EdhList     x   == EdhList     y   = x == y
    EdhTuple    x   == EdhTuple    y   = x == y
    EdhGroup    x   == EdhGroup    y   = x == y

    EdhClass    x   == EdhClass    y   = x == y
    EdhMethod   x   == EdhMethod   y   = x == y
    EdhGenrDef  x   == EdhGenrDef  y   = x == y

    EdhIterator x   == EdhIterator y   = x == y
    -- todo: regard a yielded/returned value equal to the value itself ?
    EdhYield    x'v == EdhYield    y'v = x'v == y'v
    EdhReturn   x'v == EdhReturn   y'v = x'v == y'v

    EdhProxy    x'v == EdhProxy    y'v = x'v == y'v

    -- todo: support coercing equality ?
    --       * without this, we are a strongly typed dynamic language
    --       * with this, we'll be a weakly typed dynamic language
    _               == _               = False


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


createEdhWorld :: MonadIO m => m EdhWorld
createEdhWorld = liftIO $ do
    e <- newIORef Map.empty
    let srcPos = SourcePos { sourceName   = "<Genesis>"
                           , sourceLine   = mkPos 0
                           , sourceColumn = mkPos 0
                           }
        root = Scope { scopeEntity     = e
-- create using undefined, update to itself immediately
                     , scopeOuterScope = undefined
-- should never reach this this, as all Edh code are
-- defined within some module context.
                     , thisObject      = undefined
                     }
        root' = root { scopeOuterScope = root }
    opPD  <- newIORef Map.empty
    modus <- newIORef Map.empty
    return $ EdhWorld
        { worldRoot      = e
        , moduleClass    = Class
                               { classOuterScope = root'
                               , className       = "<module>"
                               , classSourcePos  = srcPos
                               , classProcedure  = ProcDecl
                                                       { procedure'args =
                                                           WildReceiver
                                                       , procedure'body = ( srcPos
                                                                          , VoidStmt
                                                                          )
                                                       }
                               }
        , worldOperators = opPD
        , worldModules   = modus
        }


declareEdhOperators
    :: MonadIO m => EdhWorld -> Text -> [(OpSymbol, Precedence)] -> m ()
declareEdhOperators world declLoc opps = liftIO
    $ atomicModifyIORef' (worldOperators world) declarePrecedence
  where
    declarePrecedence :: OpPrecDict -> (OpPrecDict, ())
    declarePrecedence opPD =
        flip (,) ()
            $ Map.unionWithKey chkCompatible opPD
            $ Map.fromList
            $ flip Prelude.map opps
            $ \(op, p) -> (op, (p, declLoc))
    chkCompatible
        :: OpSymbol
        -> (Precedence, Text)
        -> (Precedence, Text)
        -> (Precedence, Text)
    chkCompatible op (prevPrec, prevDeclLoc) (newPrec, newDeclLoc) =
        if prevPrec /= newPrec
            then throw $ EvalError
                (  "precedence change from "
                <> T.pack (show prevPrec)
                <> " (declared "
                <> prevDeclLoc
                <> ") to "
                <> T.pack (show newPrec)
                <> " (declared "
                <> T.pack (show newDeclLoc)
                <> ") for operator: "
                <> op
                )
            else (prevPrec, prevDeclLoc)


putEdhAttr :: MonadIO m => Entity -> AttrKey -> EdhValue -> m ()
putEdhAttr e k v =
    liftIO $ void $ atomicModifyIORef' e $ \e0 -> return (Map.insert k v e0, ())

putEdhAttrs :: MonadIO m => Entity -> [(AttrKey, EdhValue)] -> m ()
putEdhAttrs e as = liftIO $ void $ atomicModifyIORef' e $ \e0 ->
    return (Map.union ad e0, ())
    where ad = Map.fromList as

