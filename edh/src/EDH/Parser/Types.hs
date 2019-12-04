module EDH.Parser.Types where

import           RIO

import           EDH.Common.ParserT
import           EDH.Lexer.Token

type Parser = ParserT [Token] Identity

execParser :: Parser a -> [Token] -> Either ParserError a
execParser = (runIdentity .) . execParserT
