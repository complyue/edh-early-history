{-# LANGUAGE LambdaCase #-}
module EDH.Lexer where

import           Prelude                        ( read )
import           RIO                     hiding ( many )

import qualified Data.Text                     as T

import           EDH.Common.ParserT
import           EDH.Lexer.Token
import           EDH.Lexer.Types
import           EDH.Utils                      ( isLetter
                                                , isIdentChar
                                                , isDigit
                                                )

lexToken :: Lexer Token
lexToken = choose
    [ lexOperator
    , lexPunctuation
    , lexString
    , lexReservedOrIdent
    , lexInteger
    , lexIllegal
    ]

parseMap :: Text -> Token -> Lexer Token
parseMap str tkn = string str $> tkn

lexOperator :: Lexer Token
lexOperator = choose
    [ parseMap "==" Eq
    , parseMap "="  Assign
    , parseMap "+"  Plus
    , parseMap "-"  Minus
    , parseMap "*"  Multiply
    , parseMap "/"  Divide
    , parseMap "!=" NotEq
    , parseMap "!"  Not
    , parseMap ">"  GreaterThan
    , parseMap "<"  LessThan
    , parseMap "#"  UpdHash
    ]

lexPunctuation :: Lexer Token
lexPunctuation = choose
    [ parseMap ":" Colon
    , parseMap ";" SemiColon
    , parseMap "," Comma
    , parseMap "(" LParen
    , parseMap ")" RParen
    , parseMap "{" LBrace
    , parseMap "}" RBrace
    , parseMap "[" LBracket
    , parseMap "]" RBracket
    ]

lexString :: Lexer Token
lexString = do
    delim <- atom '"' <|> atom '\''
    let go :: Lexer Text
        go = next >>= \case
            '\\' -> do
                c <- next
                T.cons
                        (case c of
                            'n' -> '\n'
                            't' -> '\t'
                            c_  -> c_
                        )
                    <$> go
            c | delim == c -> return ""
              | otherwise  -> T.cons c <$> go
    StringLiteral <$> go

letter :: Lexer Char
letter = predicate isLetter

identChar :: Lexer Char
identChar = predicate isIdentChar

digit :: Lexer Char
digit = predicate isDigit

lexReservedOrIdent :: Lexer Token
lexReservedOrIdent = do
    str <- (:) <$> letter <*> many (identChar <|> digit)
    return $ case str of
        "fn"     -> Function
        "if"     -> If
        "then"   -> Then
        "else"   -> Else
        "return" -> Return
        "true"   -> BoolLiteral True
        "false"  -> BoolLiteral False
        _        -> Ident (T.pack str)

lexInteger :: Lexer Token
lexInteger = many1 digit >>= \n ->
    ((string "e-" <|> string "E-") >> many1 digit >>= \e ->
            return $ DecLiteral (read n) (-(read e))
        )
        <|> ((atom 'e' <|> atom 'E') >> many1 digit >>= \e ->
                return $ DecLiteral (read n) (read e)
            )
        <|> (return $ DecLiteral (read n) 0)

lexIllegal :: Lexer Token
lexIllegal = consume $> Illegal

skipWhitespaces :: Lexer ()
skipWhitespaces =
    many (predicate $ flip elem [' ', '\t', '\n', '\r']) >> return ()

lex :: Text -> Either ParserError [Token]
lex = execLexer go
  where
    go :: Lexer [Token]
    go = do
        skipWhitespaces
        preview >>= \case
            Nothing -> return [EOF]
            _       -> (:) <$> lexToken <*> go
