{-# LANGUAGE MultiParamTypeClasses #-}
module EDH.Lexer.Types where

import           RIO

import           EDH.Common.ParserT

type Lexer = ParserT Text Identity

execLexer :: Lexer a -> Text -> Either ParserError a
execLexer = (runIdentity .) . execParserT
