{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Edh.Compiler.Lexer.Types where

import           RIO

import           Language.Edh.Compiler.ParserT

type Lexer = ParserT Text Identity

execLexer :: Lexer a -> Text -> Either ParserError a
execLexer = (runIdentity .) . execParserT
