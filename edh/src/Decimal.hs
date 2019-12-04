-- | a Pointless Decimal type
module Decimal where

import           RIO                     hiding ( exponent )

import           GHC.Show                       ( Show(..) )


data Decimal = Decimal {
    exponent :: Integer
    , denominator :: Integer
    , numerator :: Integer
}

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
