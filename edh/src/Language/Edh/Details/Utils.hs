{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Language.Edh.Details.Utils where

import           Prelude

import           Control.Exception
import           Control.Monad.Except

import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes


-- TODO seek more optimal method for this
takeFromMapByKey :: Ord k => k -> Map.Map k a -> (Maybe a, Map.Map k a)
takeFromMapByKey k map = (lookedup, Map.union left right)
    where (left, lookedup, right) = Map.splitLookup k map
