
module Language.Edh.Runtime where

import           Prelude

import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec                ( SourcePos )

import           Data.Lossless.Decimal         as D

import           Language.Edh.AST
import           Language.Edh.Parser
import           Language.Edh.Parser.Details


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


type Entity = Map.Map AttrKey EdhValue
data AttrKey = AttrByName AttrName | AttrBySym Symbol
    deriving (Eq, Ord, Show)


newtype Dict = Dict (Map.Map ItemKey EdhValue)
    deriving (Eq)
instance Show Dict where
    show (Dict d) = "{" ++ go (Map.toList d) ++ "}"
      where
        go []       = ""
        go (p : ps) = go' p ++ "," ++ go ps
        go' (k, v) = show k ++ ":" ++ show v
data ItemKey = ItemByStr Text | ItemBySym Symbol
            | ItemByNum Decimal | ItemByBool Bool
    deriving (Eq, Ord)
instance Show ItemKey where
    show (ItemByStr  k) = show k
    show (ItemBySym  k) = show k
    show (ItemByNum  k) = showDecimal k
    show (ItemByBool k) = show $ EdhBool k


-- | An object is an in-memory mutable entity in Edh
data Object = Object {
        -- | an object is identified by the pointer value to its
        -- attribute dict
        objEntity :: IORef Entity

        -- | the constructor procedure (a.k.a) class of this object
        --
        -- so there's black magic to change this field, to make
        -- another object, as view of the same entity, fooling
        -- rest of the world that this entity has a different
        -- constructor. Javascript folks have been doing this for
        -- decades tho.
        , objCtor :: Class
    }
instance Eq Object where
    -- equality by pointer to entity
    Object x'attrs _ == Object y'attrs _ = x'attrs == y'attrs
instance Show Object where
    show (Object _ (Class _ cn _)) = "[object of: " ++ T.unpack cn ++ "]"

data Class = Class {
        -- | the lexical context in which this class is defined
        classOuterEntity :: Object
        , className :: AttrName
        , classSourcePos :: SourcePos
    }
instance Eq Class where
    Class x's x'cn _ == Class y's y'cn _ = x's == y's && x'cn == y'cn
instance Show Class where
    show (Class _ cn _) = "[class: " ++ T.unpack cn ++ "]"

data Method = Method {
        methodOwnerObject :: Object
        , methodName :: AttrName
        , methodSourcePos :: SourcePos
    }
instance Eq Method where
    Method x'o x'mn _ == Method y'o y'mn _ = x'o == y'o && x'mn == y'mn
instance Show Method where
    show (Method (Object _ (Class _ cn _)) mn _) =
        "[method: " ++ T.unpack cn ++ "#" ++ T.unpack mn ++ "]"

data Module = Module {
        moduleObject :: Object
        , modulePath :: ModuleId
    }
instance Eq Module where
    Module x'o _ == Module y'o _ = x'o == y'o
instance Show Module where
    show (Module _ mp) = "[module: " ++ mp ++ "]"


data EdhWorld = EdhWorld {
        worldRoot :: Object
        , worldOperators :: IORef OpPrecDict
    }


-- | Atop Haskell, most types in Edh, as to organize information,
-- are immutable values, the only mutable data structure in Edh,
-- is the *entity* viewed as *object* (see 'objEntity').
--
-- The concept of *entity* should be defined as a set of keyed
-- attributes, where each one can be independently & freely rebound
-- to other values at any time. Of course further constraints as
-- business rules would apply on how the binding relationships
-- can change, separately as well as mutually affecting.
--
-- Theoretically it is not necessary to have an `identity`
-- attribute among others, while practically the memory address
-- for physical storage of the attribute set of an entity, serves
-- naturally as an `id` attribute in single-process + single-run
-- scenario. Distributed programs, especially using a separate
-- database for storage, will tend to define a generated UUID 
-- attribute or the like.
data EdhValue =
    -- * immutable values
          EdhNil
        | EdhDecimal Decimal
        | EdhBool Bool
        | EdhString Text
        | EdhSymbol Symbol
    -- * direct pointer (to entities) values
        | EdhObject Object
        | EdhModule Module
    -- * immutable by self, but may contain pointer to entities
        | EdhList [EdhValue]
        | EdhDict Dict
    -- * immutable by self, but have access to lexical entities
        | EdhClass Class
        | EdhMethod Method
    -- * special harness
        | EdhReturn EdhValue

instance Show EdhValue where
    show EdhNil         = "nil"
    show (EdhDecimal v) = showDecimal v
    show (EdhBool    v) = if v then "true" else "false"
    show (EdhString  v) = show v
    show (EdhSymbol  v) = show v

    show (EdhObject  v) = show v
    show (EdhModule  v) = show v

    show (EdhList    v) = show v
    show (EdhDict    v) = show v

    show (EdhClass   v) = show v
    show (EdhMethod  v) = show v

    show (EdhReturn  v) = "[return: " ++ show v ++ "]"

instance Eq EdhValue where
    EdhNil         == EdhNil         = True
    EdhDecimal x   == EdhDecimal y   = x == y
    EdhBool    x   == EdhBool    y   = x == y
    EdhString  x   == EdhString  y   = x == y
    EdhSymbol  x   == EdhSymbol  y   = x == y

    EdhObject  x   == EdhObject  y   = x == y
    EdhModule  x   == EdhModule  y   = x == y

    EdhList    x   == EdhList    y   = x == y
    EdhDict    x   == EdhDict    y   = x == y

    EdhClass   x   == EdhClass   y   = x == y
    EdhMethod  x   == EdhMethod  y   = x == y

    EdhReturn  x'v == EdhReturn  y'v = x'v == y'v
-- todo: regard a returned value equal to the value itself ?
    _              == _              = False


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

