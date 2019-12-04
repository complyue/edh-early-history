-- | a lossless numeric type for decimal arithmetic
--
-- the only odd thing with this type is absence of the
-- `decimal point`, i.e. you write '123e-9' instead of
-- '1.23e-7', '123e-2' instead of '1.23'
module EDH.Decimal where

import           RIO

import           Prelude                        ( Num(..) )

import           Data.Ratio
import           GHC.Show                       ( Show(..) )


data Decimal = Decimal {
    denominator'10 :: !Integer
    , exponent'10 :: !Integer
    , numerator'10 :: !Integer
}

nan :: Decimal
nan = Decimal 0 0 0

inf :: Decimal
inf = Decimal 0 0 1

normalizeDecimal :: Decimal -> Decimal
normalizeDecimal (Decimal d e n)
    | d == 0    = Decimal 0 0 $ if n == 0 then 0 else if n < 0 then (-1) else 1
    | d'' < 0   = Decimal (-d'') (ne - de) (-n'')
    | otherwise = Decimal d'' (ne - de) n''
  where
    (n', d') | e < 0     = simplify n (d * 10 ^ (-e))
             | otherwise = simplify (n * 10 ^ e) d
    (ne, n'') = decodeRadix'10 0 n'
    (de, d'') = decodeRadix'10 0 d'

    simplify :: Integer -> Integer -> (Integer, Integer)
    simplify x y | x == 0 || y == 0 = (x, y)
                 | cd <= 1          = (x, y)
                 | otherwise        = (x `div` cd, y `div` cd)
        where cd = gcd x y


instance Show Decimal where
    show = showDecimal

instance Num Decimal where
    fromInteger = uncurry (Decimal 1) . (decodeRadix'10 0 . fromIntegral)
    (+)         = addDecimal
    (*)         = mulDecimal
    abs (Decimal d e n) = Decimal (abs d) e (abs n)
    signum (Decimal d e n) = Decimal (abs d) e $ signum n * signum d
    negate = negateDecimal

instance Fractional Decimal where
    fromRational x = Decimal (denominator x) 0 (numerator x)
    (/) = divDecimal

-- | neither x nor y can be nan or inf, but not checked
compareVanillaDecimal :: Decimal -> Decimal -> Ordering
compareVanillaDecimal (Decimal x'd x'e x'n) (Decimal y'd y'e y'n) =
    if x'e >= y'e
        then compare (x'n * y'd * 10 ^ (x'e - y'e)) (y'n * x'd)
        else compare (x'n * y'd) (y'n * x'd * 10 ^ (y'e - x'e))

instance Ord Decimal where
    -- a nan is considered greater even compared to +inf,
    -- so that nans are sorted to last in ascending order.
    -- a nan is considered equal compared to another nan,
    -- this only hold in sorting, not in equality testing.
    compare x@(Decimal x'd _x'e x'n) y@(Decimal y'd _y'e y'n)
        | x'd == 0 = if x'n == 0
            then -- nan vs y
                (case () of
                    () | y'd /= 0 -> -- nan vs vanilla y
                        GT
                    () | y'n == 0 -> -- nan vs nan 
                        EQ
                    _ -> -- nan vs ±inf
                        GT
                )
            else -- ±inf vs y
                (case () of
                    () | y'd /= 0 -> -- ±inf vs vanilla y
                        if x'n < 0 then LT else GT
                    () | y'n == 0 -> -- ±inf vs nan 
                        LT
                    () | x'n < 0 -> -- -inf vs ±inf
                        if y'n < 0 then EQ else LT
                    _ -> -- +inf vs ±inf
                        if y'n < 0 then GT else EQ
                )
        | y'd == 0 = -- vanilla x vs nan or ±inf
                     if y'n < 0 then GT else LT
        | -- no nan/inf involved
          otherwise = compareVanillaDecimal x y

instance Eq Decimal where
    x@(Decimal x'd x'e x'n) == y@(Decimal y'd y'e y'n)
        | nanInvolved'or'notBothInf = False
        | -- no nan involved, 1 or both inf, considered equal iff exactly equal
          -- todo handle unnormalized inf if that's possible
          nanOrInfInvolved          = x'e == y'e && x'd == y'd && x'n == y'n
        | otherwise                 = EQ == compareVanillaDecimal x y
      where
        nanOrInfInvolved          = x'd == 0 || y'd == 0
        nanInvolved'or'notBothInf = nanOrInfInvolved && (x'n == 0 || y'n == 0)

decimalGreater :: Decimal -> Decimal -> Bool
decimalGreater x@(Decimal x'd _x'e x'n) y@(Decimal y'd _y'e y'n)
    | -- always False when nan involved
      (x'd == 0 && x'n == 0) || (y'd == 0 && y'n == 0) = False
    | otherwise = GT == compareVanillaDecimal x y

decimalLess :: Decimal -> Decimal -> Bool
decimalLess x@(Decimal x'd _x'e x'n) y@(Decimal y'd _y'e y'n)
    | -- always False when nan involved
      (x'd == 0 && x'n == 0) || (y'd == 0 && y'n == 0) = False
    | otherwise = LT == compareVanillaDecimal x y

negateDecimal :: Decimal -> Decimal
negateDecimal (Decimal d e n) = Decimal d e (-n)

addDecimal :: Decimal -> Decimal -> Decimal
addDecimal (Decimal x'd x'e x'n) (Decimal y'd y'e y'n) =
    normalizeDecimal $ if x'e >= y'e
        then Decimal (x'd * y'd) y'e (x'n * y'd * 10 ^ (x'e - y'e) + y'n * x'd)
        else Decimal (x'd * y'd) x'e (x'n * y'd + y'n * x'd * 10 ^ (y'e - x'e))

subsDecimal :: Decimal -> Decimal -> Decimal
subsDecimal x y = addDecimal x $ negateDecimal y

mulDecimal :: Decimal -> Decimal -> Decimal
mulDecimal (Decimal x'd x'e x'n) (Decimal y'd y'e y'n) =
    normalizeDecimal $ Decimal (x'd * y'd) (x'e + y'e) (x'n * y'n)

divDecimal :: Decimal -> Decimal -> Decimal
divDecimal (Decimal x'd x'e x'n) (Decimal y'd y'e y'n) =
    mulDecimal (Decimal x'd x'e x'n) (Decimal y'n (-y'e) y'd)

showDecimal :: Decimal -> String
showDecimal (Decimal d e n)
    | d == 0 = if n == 0 then "nan" else if n < 0 then "-inf" else "inf"
    | abs e < 5 = if e < 0
        then show n <> "/" <> show (d * 10 ^ (-e))
        else if d == 1
            then show (n * 10 ^ e)
            else show (n * 10 ^ e) <> "/" <> show d
    | d == 1 = if e == 0 then show n else show n <> "e" <> show e
    | e == 0 = show n <> "/" <> show d
    | e > 0 = show n <> "e" <> show e <> "/" <> show d
    | otherwise = show n <> "/" <> show d <> "e" <> show (-e)


decodeRadix'10 :: Integer -> Integer -> (Integer, Integer)
decodeRadix'10 e_ n_ | n_ == 0   = (0, 0)
                     | r == 0    = decodeRadix'10 (e_ + 1) n_'
                     | otherwise = (e_, n_)
    where (n_', r) = quotRem n_ 10

