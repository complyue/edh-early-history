
module NiceLook where

import           RIO                     hiding ( Hashable )

shortenDecimal :: Integer -> String
shortenDecimal i =
    let (n, e) = split10 i 0
    in  if e >= 5 then show n <> "e" <> show e else show i
  where
    split10 :: Integer -> Integer -> (Integer, Integer)
    split10 n e = case n `mod` 10 of
        0 -> let n' = n `quot` 10 in split10 n' (e + 1)
        _ -> (n, e)
