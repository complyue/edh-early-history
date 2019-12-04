{-# LANGUAGE MultiParamTypeClasses #-}
module EDH.Lexer.Types where

import           RIO

import qualified Data.Text                     as T

import           EDH.Common.ParserT
import           EDH.Common.Stream

instance Stream Text Char where
    read = T.uncons

type Lexer = ParserT Text Identity

execLexer :: Lexer a -> Text -> Either ParserError a
execLexer = (runIdentity .) . execParserT
