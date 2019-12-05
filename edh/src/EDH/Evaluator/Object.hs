module EDH.Evaluator.Object where

import           RIO                     hiding ( Hashable )

import qualified Data.Text                     as T

import qualified Data.Map.Strict               as M
import           GHC.Show                       ( Show(..) )

import           EDH.Decimal                   as D
import           EDH.Parser.AST


data Object = ODecimal Decimal
            | OBool Bool
            | OString Text
            | OArray [Object]
            | OHash (M.Map Hashable Object)
            | ONil
            | OFn { params :: [Ident]
                  , body :: SeqStmts
                  , env :: EnvRef
                  }
            | OBuiltInFn { name :: Text
                         , numParams :: Int
                         , fn :: BuiltInFn
                         }
            | OReturn Object

instance Show Object where
    show (ODecimal x) = showDecimal x
    show (OBool    x) = if x then "true" else "false"
    show (OString  x) = show x
    show (OArray   x) = show x
    show (OHash    m) = "{" ++ go (M.toList m) ++ "}"
      where
        go []       = ""
        go [(l, o)] = show l ++ ":" ++ show o
        go (x : xs) = go [x] ++ "," ++ go xs
    show ONil               = "nil"
    show (OFn        _ _ _) = "[function]"
    show (OBuiltInFn n _ _) = "[built-in function: " ++ T.unpack n ++ "]"
    show (OReturn o       ) = show o

instance Eq Object where
    ODecimal x       == ODecimal y         = x == y
    OBool    x       == OBool    y         = x == y
    OString  x       == OString  y         = x == y
    OArray   x       == OArray   y         = x == y
    OHash    x       == OHash    y         = x == y
    ONil             == ONil               = True
    OFn p b e        == OFn p' b' e'       = p == p' && b == b' && e == e'
    OReturn o        == o'                 = o == o'
    o                == OReturn o'         = o == o'
    OBuiltInFn n p _ == OBuiltInFn n' p' _ = n == n' && p == p'
    _                == _                  = False

data Hashable = DecimalHash Decimal
              | BoolHash Bool
              | StringHash Text
              deriving (Eq, Ord)

instance Show Hashable where
    show (DecimalHash d) = show $ ODecimal d
    show (BoolHash    b) = show $ OBool b
    show (StringHash  t) = show $ OString t

type BuiltInFnResult = Either Text Object
type BuiltInFn = [Object] -> IO BuiltInFnResult

true :: Object
true = OBool True

false :: Object
false = OBool False

nil :: Object
nil = ONil

nan :: Object
nan = ODecimal D.nan

inf :: Object
inf = ODecimal D.inf

ret :: Object -> Object
ret = OReturn

type EnvRef = IORef Environment

data Environment = Environment { varMap :: M.Map Ident Object
                               , parent :: Maybe EnvRef
                               }
                 deriving (Eq)

emptyEnv :: IO EnvRef
emptyEnv = newIORef $ Environment M.empty Nothing

wrapEnv :: EnvRef -> [(Ident, Object)] -> IO EnvRef
wrapEnv = (newIORef .) . flip (Environment . M.fromList) . Just

insertVar :: Ident -> Object -> EnvRef -> IO EnvRef
insertVar i o ref = atomicModifyIORef' ref (go i o) $> ref
  where
    go :: Ident -> Object -> Environment -> (Environment, ())
    go i_ o_ (Environment m p) = (Environment (M.insert i_ o_ m) p, ())

getVar :: Ident -> EnvRef -> IO (Maybe Object)
getVar i ref = do
    Environment m p <- readIORef ref
    case M.lookup i m of
        Just o  -> return $ Just o
        Nothing -> case p of
            Just p_ -> getVar i p_
            Nothing -> return Nothing
