module Language.Edh.Compiler.Parser.Types where

import           RIO

import           Language.Edh.Compiler.ParserT
import           Language.Edh.Compiler.Lexer.Token

type Parser = ParserT [Token] Identity

execParser :: Parser a -> [Token] -> Either ParserError a
execParser = (runIdentity .) . execParserT
