module EDH.Lexer.Token where

import           RIO

data Token = Illegal
           | EOF
           -- identifier and literals
           | Ident Text
           | DecLiteral Integer Integer
           | BoolLiteral Bool
           | StringLiteral Text
           -- statements
           | Assign
           | If
           | Else
           -- operators
           | Plus
           | Minus
           | Divide
           | Multiply
           | Eq
           | NotEq
           | GreaterThan
           | LessThan
           | Not
           -- reserved words
           | Function
           | Return
           -- punctuations
           | Comma
           | Colon
           | SemiColon
           | LParen
           | RParen
           | LBrace
           | RBrace
           | LBracket
           | RBracket
           deriving (Show, Eq)
