module Evaluator.Object where

import           RIO                     hiding ( Hashable
                                                , exponent
                                                )

import qualified Data.Text                     as T

import qualified Data.Map.Strict               as M
import           GHC.Show                       ( Show(..) )

import           Parser.AST


data Decimal = Decimal {
    exponent :: Integer
    , denominator :: Integer
    , numerator :: Integer
}

vanillaInt :: Integer -> Decimal
vanillaInt = Decimal 0 1

instance Show Decimal where
    show = showDecimal

instance Ord Decimal where
    compare (Decimal e d n) (Decimal e' d' n') = if e >= e'
        then compare (n * d' * 10 ^ (e - e')) (n' * d)
        else compare (n * d') (n' * d * 10 ^ (e' - e))

instance Eq Decimal where
    Decimal e d n == Decimal e' d' n' = if e >= e'
        then n * d' * 10 ^ (e - e') == n' * d
        else n * d' == n' * d * 10 ^ (e' - e)

normalizeDecimal :: Decimal -> Decimal
normalizeDecimal (Decimal e d n) = if d == 0
    then (Decimal 0 0 $ if n == 0 then 0 else if n < 0 then (-1) else 1)
    else if d'' < 0
        then Decimal (ne - de) (-d'') (-n'')
        else Decimal (ne - de) d'' n''
  where
    (n', d') =
        if e >= 0 then simplify (n * 10 ^ e) d else simplify n (d * 10 ^ (-e))
    (n'', ne) = decodeBase10 n' 0
    (d'', de) = decodeBase10 d' 0
    decodeBase10 :: Integer -> Integer -> (Integer, Integer)
    decodeBase10 n_ e_ = case n_ `rem` 10 of
        0 -> let n_' = n_ `quot` 10 in decodeBase10 n_' (e_ + 1)
        _ -> (n_, e_)
    simplify x y | x == 0 || y == 0 = (x, y)
                 | cd <= 1          = (x, y)
                 | otherwise        = (x `div` cd, y `div` cd)
        where cd = gcd x y

negateDecimal :: Decimal -> Decimal
negateDecimal (Decimal e d n) = Decimal e d (-n)

addDecimal :: Decimal -> Decimal -> Decimal
addDecimal (Decimal e d n) (Decimal e' d' n') = normalizeDecimal $ if e >= e'
    then Decimal 0 (d * d') (n * d' * 10 ^ (e - e') + n' * d)
    else Decimal 0 (d * d') (n * d' + n' * d * 10 ^ (e' - e))

mulDecimal :: Decimal -> Decimal -> Decimal
mulDecimal (Decimal e d n) (Decimal e' d' n') =
    normalizeDecimal $ Decimal (e + e') (d * d') (n * n')

divDecimal :: Decimal -> Decimal -> Decimal
divDecimal (Decimal e d n) (Decimal e' d' n') =
    mulDecimal (Decimal e d n) (Decimal (-e') n' d')

showDecimal :: Decimal -> String
showDecimal (Decimal e d n) = if d == 0
    then (if n == 0 then "NaN" else if n < 0 then "-Inf" else "Inf")
    else if abs e < 5
        then
            (if e < 0
                then show n <> "/" <> show (d * 10 ^ (-e))
                else if d == 1
                    then show (n * 10 ^ e)
                    else show (n * 10 ^ e) <> "/" <> show d
            )
        else if d == 1
            then (if e == 0 then show n else show n <> "e" <> show e)
            else if e == 0
                then (show n <> "/" <> show d)
                else if e > 0
                    then (show n <> "e" <> show e <> "/" <> show d)
                    else (show n <> "/" <> show d <> "e" <> show (-e))


data Object = ODecimal Decimal
            | OBool Bool
            | OString Text
            | OArray [Object]
            | OHash (M.Map Hashable Object)
            | ONull
            | OFn { params :: [Ident]
                  , body :: BlockStmt
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
    show ONull              = "null"
    show (OFn        _ _ _) = "[function]"
    show (OBuiltInFn n _ _) = "[built-in function: " ++ T.unpack n ++ "]"
    show (OReturn o       ) = show o

instance Eq Object where
    ODecimal x       == ODecimal y         = x == y
    OBool    x       == OBool    y         = x == y
    OString  x       == OString  y         = x == y
    OArray   x       == OArray   y         = x == y
    OHash    x       == OHash    y         = x == y
    ONull            == ONull              = True
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
nil = ONull

ret :: Object -> Object
ret o = OReturn o

isReturned :: Object -> Bool
isReturned (OReturn _) = True
isReturned _           = False

returned :: Object -> Object
returned (OReturn o) = o
returned o           = o

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
insertVar i o ref = modifyIORef ref (go i o) $> ref
  where
    go :: Ident -> Object -> Environment -> Environment
    go i o (Environment m p) = Environment (M.insert i o m) p

getVar :: Ident -> EnvRef -> IO (Maybe Object)
getVar i ref = do
    Environment m p <- readIORef ref
    case M.lookup i m of
        Just o  -> return $ Just o
        Nothing -> case p of
            Just parent -> getVar i parent
            Nothing     -> return $ Nothing
