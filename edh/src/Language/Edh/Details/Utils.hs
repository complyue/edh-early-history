
module Language.Edh.Details.Utils where

import           Prelude

import qualified Data.Map.Strict               as Map


-- TODO seek more optimal method for this
takeOutFromMap :: Ord k => k -> Map.Map k a -> (Maybe a, Map.Map k a)
takeOutFromMap k m = (p, Map.union left right)
    where (left, p, right) = Map.splitLookup k m


