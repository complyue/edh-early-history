{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module EDH.Common.Stream where

import           RIO

class Stream s a | s -> a where
  read :: s -> Maybe (a, s)

instance Stream [a] a where
    read (x : xs) = Just (x, xs)
    read []       = Nothing
