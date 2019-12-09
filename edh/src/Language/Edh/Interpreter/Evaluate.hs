
module Language.Edh.Interpreter.Evaluate where


import           Prelude

import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec                ( SourcePos )

import           Data.Lossless.Decimal         as D

import           Language.Edh.AST
import           Language.Edh.Runtime




