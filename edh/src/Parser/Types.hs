module Parser.Types where

import           RIO

import           Common.ParserT
import           Lexer.Token

type Parser = ParserT [Token] Identity

execParser :: Parser a -> [Token] -> Either ParserError a
execParser = (runIdentity .) . execParserT
