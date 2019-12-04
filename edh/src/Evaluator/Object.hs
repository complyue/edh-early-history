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
    compare (Decimal x'e x'd x'n) (Decimal y'e y'd y'n) =
        if x'd == 0 || y'd == 0
            then -- at least one nan or inf involved
                (case () of
                    -- nan vs
                    () | x'n == 0 && x'd == 0 ->
                        -- it's considered equal compared to another nan,
                        -- and even greater compared to +inf, so that
                        -- nans are sorted last in ascending order.
                        if y'n == 0 && y'd == 0 then EQ else GT
                    -- -inf vs 
                    () | x'n < 0 && x'd == 0 ->
                        if y'n < 0 && y'd == 0 then EQ else LT
                    -- inf vs 
                    () | x'n > 0 && x'd == 0 ->
                        if y'n > 0 && y'd == 0 then EQ else GT
                    () | otherwise -> error "imposible case"
                )
            else if x'e >= y'e -- no nan/inf involved
                then compare (x'n * y'd * 10 ^ (x'e - y'e)) (y'n * x'd)
                else compare (x'n * y'd) (y'n * x'd * 10 ^ (y'e - x'e))

instance Eq Decimal where
    Decimal x'e x'd x'n == Decimal y'e y'd y'n = if x'd == 0 || y'd == 0
        then -- at least one nan or inf involved
            (if x'n == 0 || y'n == 0
                -- at least one nan or zero involved
                then False
                -- no nan, inf is considered equal iff exactly equal
                -- todo handle unnormalized inf if that's possible
                else x'e == y'e && x'd == y'd && x'n == y'n
            )
        else if x'e >= y'e -- no nan/inf involved
            then x'n * y'd * 10 ^ (x'e - y'e) == y'n * y'd
            else x'n * y'd == y'n * x'd * 10 ^ (y'e - x'e)

decimalGreater :: Decimal -> Decimal -> Bool
decimalGreater x@(Decimal _x'e x'd x'n) y@(Decimal _y'e y'd y'n) =
    -- always False when nan involved
    if (x'd == 0 && x'n == 0) || (y'd == 0 && y'n == 0) then False else x > y

decimalLess :: Decimal -> Decimal -> Bool
decimalLess x@(Decimal _x'e x'd x'n) y@(Decimal _y'e y'd y'n) =
    -- always False when nan involved
    if (x'd == 0 && x'n == 0) || (y'd == 0 && y'n == 0) then False else x < y

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
    simplify :: Integer -> Integer -> (Integer, Integer)
    simplify x y | x == 0 || y == 0 = (x, y)
                 | cd <= 1          = (x, y)
                 | otherwise        = (x `div` cd, y `div` cd)
        where cd = gcd x y

negateDecimal :: Decimal -> Decimal
negateDecimal (Decimal e d n) = Decimal e d (-n)

addDecimal :: Decimal -> Decimal -> Decimal
addDecimal (Decimal x'e x'd x'n) (Decimal y'e y'd y'n) =
    normalizeDecimal $ if x'e >= y'e
        then Decimal 0 (x'd * y'd) (x'n * y'd * 10 ^ (x'e - y'e) + y'n * x'd)
        else Decimal 0 (x'd * y'd) (x'n * y'd + y'n * x'd * 10 ^ (y'e - x'e))

subsDecimal :: Decimal -> Decimal -> Decimal
subsDecimal x y = addDecimal x $ negateDecimal y

mulDecimal :: Decimal -> Decimal -> Decimal
mulDecimal (Decimal x'e x'd x'n) (Decimal y'e y'd y'n) =
    normalizeDecimal $ Decimal (x'e + y'e) (x'd * y'd) (x'n * y'n)

divDecimal :: Decimal -> Decimal -> Decimal
divDecimal (Decimal x'e x'd x'n) (Decimal y'e y'd y'n) =
    mulDecimal (Decimal x'e x'd x'n) (Decimal (-y'e) y'n y'd)

showDecimal :: Decimal -> String
showDecimal (Decimal e d n) = if d == 0
    then (if n == 0 then "nan" else if n < 0 then "-inf" else "inf")
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

nan :: Object
nan = ODecimal $ Decimal 0 0 0

inf :: Object
inf = ODecimal $ Decimal 0 0 1

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
