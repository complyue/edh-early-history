
module Language.Edh.Control where

import           Prelude

import           Control.Exception
import           Control.Monad.State.Strict

import           Data.Void
import           Data.Typeable
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec         hiding ( State )

import           Language.Edh.AST


-- use such a dict as the parsing state, to implement
-- object-language-declarable operator precendence
type OpPrecDict = Map.Map OpSymbol (Precedence, Text)

-- no backtracking needed for precedence dict, so it
-- can live in the inner monad of 'ParsecT'.
type Parser = ParsecT Void Text (State OpPrecDict)

-- so goes this simplified parsing err type name
type ParserError = ParseErrorBundle Text Void


newtype EvalError = EvalError Text
    deriving (Eq, Typeable)
instance Show EvalError where
    show (EvalError msg) = T.unpack msg
instance Exception EvalError


data InterpretError = EdhParseError ParserError | EdhEvalError EvalError
    deriving (Eq, Typeable)
instance Show InterpretError where
    show (EdhParseError err) = "⛔ " ++ errorBundlePretty err
    show (EdhEvalError  err) = "💣 " ++ show err
instance Exception InterpretError

