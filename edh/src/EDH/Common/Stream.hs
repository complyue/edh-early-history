{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module EDH.Common.Stream where

import           RIO

import qualified Data.Text                     as T

class Stream s a | s -> a where
  read :: s -> Maybe (a, s)

instance Stream [a] a where
    read (x : xs) = Just (x, xs)
    read []       = Nothing

instance Stream Text Char where
    read = T.uncons
